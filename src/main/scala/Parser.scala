import scala.annotation.tailrec
import scala.reflect.ClassTag

case class ParserState(position: Int, tokens: List[Token]) {
  def current(): Token = {
    if(position < tokens.length) tokens(position) else EOFToken
  }

  def advance(): ParserState = copy(position = position + 1)

  def isAtEnd: Boolean = current().isInstanceOf[EOFToken]

  def check[T <: Token](implicit ct: ClassTag[T]): Boolean = {
    ct.runtimeClass.isInstance(current())
  }
}

case class ParseResult[T](state: ParserState, value: T)

class Parser(tokens: List[Token]) {
  private val initialState = ParserState(0, tokens)

  def parse(): Either[ParseError, List[Statement]] = {
    parseStatements(initialState, List().map(_.value))
  }

  @tailrec
  private def parseStatements(state: ParserState, acc: List[Statement]): Either[ParseError, ParseResult[List[Statement]]] = {
    if(state.isAtEnd){
      Right(ParseResult[List[Statement]](state = state, value = acc))
    } else {
      parseStatement(state) match {
        case Right(result) => parseStatements(result.state, acc :+ result.value)
        case Left(error) => Left(error)
      }
    }
  }
  
  private def parseStatement(state: ParserState): Either[ParseError, ParseResult[Statement]] = {
    state.current() match {
      case token: LineNumberToken =>
        val lineNum = token.value
        val nextState = state.advance()
        
        nextState.current() match {
          case _: DeferToken => parseDeferred(nextState.advance(), lineNum)
          case _: AgainToken => parseAgain(nextState.advance(), lineNum)
          case _ => parseSimpleStatement(nextState, lineNum)
        }

      case _: EOFToken => Right(ParseResult(state, SimpleStatement(0,LineReferencesAction(List()))))
      case token => Left(ParseError(s"Expected line number, got ${token.getClass.getSimpleName}"))
    }
  }
  
  private def parseDeferred(state: ParserState, lineNum: Int): Either[ParseError, ParseResult[Statement]] = {???}

  private def parseAgain(state: ParserState, lineNum: Int): Either[ParseError, ParseResult[Statement]] = {???}
  
  private def parseSimpleStatement(state: ParserState, lineNum: Int): Either[ParseError, ParseResult[Statement]] = {???}
}
