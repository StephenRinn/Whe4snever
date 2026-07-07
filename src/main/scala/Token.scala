sealed trait Token {
  def line: Int
}

sealed trait LiteralToken extends Token
case class NumberToken(value: Double, line: Int) extends LiteralToken
case class StringToken(value: String, line: Int) extends LiteralToken

sealed trait KeywordToken extends Token
case class PrintToken(line: Int) extends KeywordToken
case class ReadToken(line: Int) extends KeywordToken
case class DeferToken(line: Int) extends KeywordToken
case class AgainToken(line: Int) extends KeywordToken
case class ForgetToken(line: Int) extends KeywordToken

sealed trait FunctionToken extends Token
case class NToken(line: Int) extends FunctionToken
case class UToken(line: Int) extends FunctionToken

sealed trait OperatorToken extends Token
case class PlusToken(line: Int) extends OperatorToken
case class MinusToken(line: Int) extends OperatorToken
case class MultiplyToken(line: Int) extends OperatorToken
case class DivideToken(line: Int) extends OperatorToken
case class AndToken(line: Int) extends OperatorToken
case class OrToken(line: Int) extends OperatorToken
case class NotToken(line: Int) extends OperatorToken

sealed trait DelimiterToken extends Token
case class LeftParenToken(line: Int) extends DelimiterToken
case class RightParenToken(line: Int) extends DelimiterToken
case class CommaToken(line: Int) extends DelimiterToken
case class SemicolonToken(line: Int) extends DelimiterToken
case class HashToken(line: Int) extends DelimiterToken

case class LineNumberToken(value: Int, line: Int) extends Token

case class EOFToken(line: Int) extends Token

case class ErrorToken(message: String, line: Int) extends Token