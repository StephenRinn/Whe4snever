import scala.annotation.tailrec
import scala.util.Random

class Lexer(input: String) {

  @tailrec
  private def tokenizeHelper(position: Int, line: Int, acc: List[Token]): List[Token] = {
    if(position >= input.length) {
      acc :+ EOFToken(line)
    }else {
      val result: TokenResult = nextToken(position, line)
      tokenizeHelper(result.position, result.line, acc:+ result.token)
    }
  }

  private def nextToken(position: Int, line: Int): TokenResult = {
    val result = skipWhitespaceAndComments(position, line)
    // Simple split for readability
    val (pos, ln) = (result.position, result.line)
    if(pos >= input.length) {
      TokenResult(pos, ln, EOFToken(ln))
    } else {
      val ch = input(pos)
      ch match {
        case '(' => TokenResult(pos + 1, ln, LeftParenToken(ln))
        case ')' => TokenResult(pos + 1, ln, RightParenToken(ln))
        case ',' => TokenResult(pos + 1, ln, CommaToken(ln))
        case ';' => TokenResult(pos + 1, ln, SemicolonToken(ln))
        case '#' => TokenResult(pos + 1, ln, HashToken(ln))
        case '+' => TokenResult(pos + 1, ln, PlusToken(ln))
        case '*' => TokenResult(pos + 1, ln, MultiplyToken(ln))
        case '/' => TokenResult(pos + 1, ln, DivideToken(ln))
        case '!' =>
          if (pos + 1 < input.length && input(pos + 1) == '=') {
            val errorMsg = errorMessageCreator("!= Operator is not supported in Whenever.")
            TokenResult(pos + 2, ln, ErrorToken(errorMsg, ln))
          } else {
            TokenResult(pos + 1, ln, NotToken(ln))
          }
        case '-' =>
          if (pos + 1 < input.length && input(pos + 1).isDigit) {
            val result = readNumber(pos + 1)
            TokenResult(result.position, ln, NumberToken(-result.value, ln))
          } else {
            TokenResult(pos + 1, ln, MinusToken(ln))
          }
        case '&' =>
          if (pos + 1 < input.length && input(pos + 1) == '&'){
            TokenResult(pos + 2, ln, AndToken(ln))
          } else {
            val errorMsg = errorMessageCreator("Unexpected character: &")
            TokenResult(pos + 1, ln, ErrorToken(errorMsg, ln))
          }
        case '|' =>
          if (pos + 1 < input.length && input(pos + 1) == '|') {
            TokenResult(pos + 2, ln, AndToken(ln))
          } else {
            val errorMsg = errorMessageCreator("Unexpected character: |")
            TokenResult(pos + 1, ln, ErrorToken(errorMsg, ln))
          }
        case '"' =>
          val result = readString(pos + 1, ln)
          TokenResult(result.position, result.line, result.token)
        case ch if ch.isDigit =>
          val res = readNumber(pos)
          TokenResult(res.position, ln, LineNumberToken(res.value.toInt, ln))
        case _ if isAlpha(ch) =>
          val result = readKeywordOrFunction(pos, ln)
          TokenResult(result.position, result.line, result.token)
        case _ =>
          val errorMsg = errorMessageCreator(s"Unexpected character: $ch")
          TokenResult(pos + 1 , ln, ErrorToken(errorMsg, line))
      }
    }
  }

  @tailrec
  private def skipWhitespaceAndComments(position: Int, line: Int): WhiteSpaceResult = {
    if(position >= input.length) {
      WhiteSpaceResult(position, line)
    } else {
      input(position) match {
        case ' ' | '\t' | '\r' => skipWhitespaceAndComments(position + 1, line)
        case '\n' => skipWhitespaceAndComments(position + 1, line + 1)
      }
    }
  }

  private def readWhile(position: Int, predicate: Char => Boolean): ReadWhileResult = {
    val end = input.indexWhere(ch => !predicate(ch), position)
    val confirmedEnd = if(end == -1) input.length else end
    ReadWhileResult(confirmedEnd, input.substring(position, confirmedEnd))
  }

  private def readUntil(position: Int, line: Int, terminator: Char): ReadUntilResult = {
    readUntilHelper(position = position, line = line, terminator = terminator, acc = new StringBuilder)
  }

  @tailrec
  private def readUntilHelper(position: Int, line: Int, terminator: Char, acc: StringBuilder): ReadUntilResult = {
    if (position >= input.length) {
      ReadUntilResult(position, line, acc.mkString)
    } else if (input(position) == terminator) {
      ReadUntilResult(position, line, acc.mkString)
    } else {
      val ch = input(position)
      val (nextPos, newLine, newAcc) = if (ch == '\\' && position + 1 < input.length) {
        (position + 2, line, acc.append(ch).append(input(position + 1)))
      } else if (ch == '\n') {
        (position + 1, line + 1, acc.append(ch))
      } else {
        (position + 1, line, acc.append(ch))
      }
      readUntilHelper(position = nextPos, line = newLine, terminator = terminator, acc = newAcc)
    }
  }

  private def readNumber(position: Int): ReadNumberResult = {
    val _readWhileResult = readWhile(position, ch => ch.isDigit)
    val result = if(_readWhileResult.position < input.length && input(_readWhileResult.position) == '.') {
      val readWhileDecimalResult = readWhile(_readWhileResult.position + 1, ch => ch.isDigit)
      ReadWhileResult(position = readWhileDecimalResult.position, value = _readWhileResult.value + "." + readWhileDecimalResult.value)
    } else {
      _readWhileResult
    }
    ReadNumberResult(_readWhileResult.position, _readWhileResult.value.toDouble)
  }

  private def readString(position: Int, line: Int): TokenResult = {
    val result = readUntil(position, line, '"')
    if (result.position >= input.length) {
      val errorMsg = errorMessageCreator("Unterminated string.")
      TokenResult(position = result.position, result.line, ErrorToken(errorMsg, line))
    } else {
      val unescaped = unescapeString(result.value)
      TokenResult(result.position + 1, result.line, StringToken(unescaped, line))
    }
  }

  private def readKeywordOrFunction(position: Int, line: Int): TokenResult = {
    val result = readWhile(position = position, predicate = isAlphaNumeric)
    val token: Token = result.value.toLowerCase match {
      case "print" => PrintToken(line)
      case "read" => ReadToken(line)
      case "defer" => DeferToken(line)
      case "again" => AgainToken(line)
      case "forget" => ForgetToken(line)
      case "n" => NToken(line)
      case "u" => UToken(line)
      case _ =>
        val errorMsg = errorMessageCreator(s"Unknown keyword: ${result.value}")
        ErrorToken(errorMsg, line)
    }
    TokenResult(position = result.position, line = line, token = token)
  }

  private def errorMessageCreator(errorMessage: String): String = {
    val errorList = errorMessage.split("(?<=\\s)(?=\\S)|(?<=\\S)(?=\\s)")
    Random().shuffle(errorList).mkString
  }

  private def unescapeString(s: String): String = {
    s.replace("\\n", "\n")
      .replace("\\t", "\t")
      .replace("\\\"", "\"")
      .replace("\\\\", "\\")
  }

  private def isAlpha(ch: Char): Boolean = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
  private def isAlphaNumeric(ch: Char): Boolean = isAlpha(ch) || ch.isDigit

}

case class TokenResult(position: Int, line: Int, token: Token)
case class WhiteSpaceResult(position: Int, line: Int)
case class ReadNumberResult(position: Int, value: Double)
case class ReadWhileResult(position: Int, value: String)
case class ReadUntilResult(position: Int, line: Int, value: String)