import scala.annotation.tailrec
import scala.reflect.ClassTag

case class ParserState(position: Int, tokens: List[Token]) {
  def current(): Token = {
    if(position < tokens.length) tokens(position) else EOFToken(0)
  }

  def advance(offset: Int = 1): ParserState = copy(position = position + offset)

  def isAtEnd: Boolean = current().isInstanceOf[EOFToken]

  def check[T <: Token](implicit ct: ClassTag[T]): Boolean = {
    ct.runtimeClass.isInstance(current())
  }
}

case class ParseResult[T](state: ParserState, value: T)

class Parser(tokens: List[Token]) {
  private val initialState = ParserState(0, tokens)

  def parse(): Either[ParseError, List[Statement]] = {
    parseStatements(initialState, List()).map(_.value)
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

  private def parseDeferred(state: ParserState, lineNum: Int): Either[ParseError, ParseResult[Statement]] = {
    for {
      _ <- expectToken[LeftParenToken](state, "Expected '(' after defer.")
      condState = state.advance()
      condResult <- parseExpression(condState)
      _ <- expectToken[RightParenToken](condResult.state, "Expected ')' after defer.")
      actionState = condResult.state.advance()
      actionResult <- parseAction(actionState)
      _ <- expectToken[SemicolonToken](actionResult.state, "Expected ';' at end of statement.")
      finalState = actionResult.state.advance()
    } yield ParseResult(finalState, DeferredStatement(lineNum, condResult.value, actionResult.value))
  }

  private def parseAgain(state: ParserState, lineNum: Int): Either[ParseError, ParseResult[Statement]] = {???}

  private def parseSimpleStatement(state: ParserState, lineNum: Int): Either[ParseError, ParseResult[Statement]] = {???}

  private def parseExpression(state: ParserState): Either[ParseError, ParseResult[Expression]] = {???}

  private def parseOrExpression(state: ParserState): Either[ParseError, ParseResult[Expression]] = {???}

  private def parseAndExpression(state: ParserState): Either[ParseError, ParseResult[Expression]] = {???}

  private def parseAddSubtractExpression(state: ParserState): Either[ParseError, ParseResult[Expression]] = {???}

  private def parseMultiplyDivideExpression(state: ParserState): Either[ParseError, ParseResult[Expression]] = {???}

  private def parseUnaryExpression(state: ParserState): Either[ParseError, ParseResult[Expression]] = {???}

  private def parsePrimaryExpression(state: ParserState): Either[ParseError, ParseResult[Expression]] = {???}

  private def parseFunctionCall(state: ParserState): Either[ParseError, ParseResult[Expression]] = {???}

  private def parseFunctionArgs(state: ParserState, acc: List[Expression]): Either[ParseError, ParseResult[List[Expression]]] = {???}

  private def parseAction(state: ParserState): Either[ParseError, ParseResult[Action]] = {
    state.current() match {
      case _: PrintToken =>
        parsePrintAction(state.advance())
      case _: ReadToken =>
        for {
          _ <- expectToken[LeftParenToken](state.advance(), "Expected '(' after read")
          _ <- expectToken[RightParenToken](state.advance(2), "Expected ')' after read")
        } yield ParseResult(state.advance(3), ReadAction())
      case _: LineNumberToken =>
        parseLineReferences(state)
      case token => Left(ParseError(s"Expected actijon, got ${token.getClass.getSimpleName}"))
    }
  }

  private def parsePrintAction(state: ParserState): Either[ParseError, ParseResult[Action]] = {???}

  private def parseLineReferences(state: ParserState): Either[ParseError, ParseResult[Action]] = {
    parseLineReferenceList(state, List()).map { result =>
      ParseResult(result.state, LineReferencesAction(result.value))
    }
  }

  @tailrec
  private def parseLineReferenceList(state: ParserState, acc: List[LineReference]): Either[ParseError, ParseResult[List[LineReference]]] = {
    parseLineReference(state) match {
      case Right(result) =>
        result.state.current() match {
          case _: CommaToken =>
            parseLineReferenceList(result.state.advance(), acc :+ result.value)
          case _ =>
            Right(ParseResult(result.state, acc :+ result.value))
        }
      case Left(error) => Left(error)
    }
  }

  private def parseLineReference(state: ParserState): Either[ParseError, ParseResult[LineReference]] = {
    state.current() match {
      case token: LineNumberToken =>
        val lineNum = token.value
        val nextState = state.advance()

        nextState.current() match {
          case _: HashToken =>
            parseExpression(nextState.advance()).map { exprResult =>
              ParseResult(exprResult.state, RepeatedLineRef(lineNumber = lineNum, repeat = exprResult.value))
            }
          case _ =>
            Right(ParseResult(nextState, SingleLineRef(lineNum)))
        }
      case token => Left(ParseError(s"Expected line number got ${token.getClass.getSimpleName}"))
    }
  }


  private def expectToken[T <: Token](state: ParserState, errorMsg: String)(implicit ct: ClassTag[T]): Either[ParseError, Unit] = {
    if(state.check[T]){
      Right(())
    } else {
      Left(ParseError(errorMsg))
    }
  }
}
