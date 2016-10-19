package xyz.hyperreal.mos6502


trait AST

trait ExpressionAST extends AST
case class NumberExpressionAST( n: Int ) extends ExpressionAST
case class StringExpressionAST( s: String ) extends ExpressionAST
case class ReferenceExpressionAST( l: String ) extends ExpressionAST
case class BinaryExpressionAST( left: ExpressionAST, op: String, right: ExpressionAST ) extends ExpressionAST
case class UnaryExpressionAST( op: String, exp: ExpressionAST ) extends ExpressionAST

abstract class ModeAST extends AST {
	var operand: Option[Int] = None
}

case object ImplicitModeAST extends ModeAST
case object AccumulatorModeAST extends ModeAST
case class ImmediateModeAST( v: ExpressionAST ) extends ModeAST
case class DirectModeAST( a: ExpressionAST ) extends ModeAST
case class DirectXModeAST( a: ExpressionAST ) extends ModeAST
case class DirectYModeAST( a: ExpressionAST ) extends ModeAST
case class IndirectModeAST( a: ExpressionAST ) extends ModeAST
case class IndirectXModeAST( a: ExpressionAST ) extends ModeAST
case class IndirectYModeAST( a: ExpressionAST ) extends ModeAST

case class SourceAST( statements: List[StatementAST] ) extends AST

trait StatementAST extends AST

trait DirectiveAST extends StatementAST
case class LabelDirectiveAST( label: String ) extends DirectiveAST
case class OriginDirectiveAST( org: ExpressionAST ) extends DirectiveAST

abstract class AllocationAST extends StatementAST {
	var size: Option[Int] = None
}

case class InstructionAST( mnemonic: String, mode: ModeAST ) extends AllocationAST
case class DataByteAST( data: Seq[ExpressionAST] ) extends AllocationAST
case class DataWordAST( data: Seq[ExpressionAST] ) extends AllocationAST
case class ReserveByteAST( count: Option[ExpressionAST] ) extends AllocationAST
case class ReserveWordAST( count: Option[ExpressionAST] ) extends AllocationAST