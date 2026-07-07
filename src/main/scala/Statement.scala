sealed trait Statement {
  def lineNumber: Int
}

case class SimpleStatement(lineNumber: Int, action: Action) extends Statement
case class DeferredStatement(lineNumber: Int, condition: Expression, action: Action) extends Statement
case class AgainStatement(lineNumber: Int, condition: Expression, action: Action) extends Statement

sealed trait Action
case class PrintAction(expr: Expression) extends Action
case class ReadAction() extends Action
case class LineReferencesAction(refernces: List[LineReference]) extends Action

sealed trait LineReference
case class SingleLineRef(lineNumber: Int) extends LineReference
case class RepeatedLineRef(lineNumber: Int, repeat: Expression) extends LineReference

sealed trait Expression
case class NumberExpr(value: Double) extends Expression
case class StringExpr(value: String) extends Expression
case class BinaryOpExpr(left: Expression, op: BinaryOp, right: Expression) extends Expression
case class UnaryOpExpr(op: UnaryOp, expression: Expression) extends Expression
case class FunctionCallExpr(function: BuiltinFunction, args: List[Expression]) extends Expression

sealed trait BinaryOp
case object AddOp extends BinaryOp
case object SubtractOp
case object MultiplyOp extends BinaryOp
case object DivideOp extends BinaryOp
case object AndOp extends BinaryOp
case object OrOp extends BinaryOp

sealed trait UnaryOp
case object NotOp extends UnaryOp
case object NegateOp extends UnaryOp

sealed trait BuiltinFunction
case object NFunc extends BuiltinFunction
case object UFunc extends BuiltinFunction

case class ParseError(message: String)


