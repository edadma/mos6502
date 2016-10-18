package xyz.hyperreal.mos6502


trait AST

trait ExpressionAST extends AST
case class NumberExpressionAST( n: Int ) extends ExpressionAST
case class StringExpressionAST( s: String ) extends ExpressionAST
case class ReferenceExpressionAST( l: String ) extends ExpressionAST
case class BinaryExpressionAST( left: ExpressionAST, op: String, right: ExpressionAST ) extends ExpressionAST
case class UnaryExpressionAST( op: String, exp: ExpressionAST ) extends ExpressionAST

trait ModeAST
case object ImplicitModeAST extends ModeAST
case object AccumulatorModeAST extends ModeAST
case class ImmediateModeAST( v: ExpressionAST ) extends ModeAST
case class DirectModeAST( a: ExpressionAST, var absolute: Boolean = false ) extends ModeAST
case class DirectXModeAST( a: ExpressionAST, var absolute: Boolean = false ) extends ModeAST
case class DirectYModeAST( a: ExpressionAST, var absolute: Boolean = false ) extends ModeAST
case class IndirectModeAST( a: ExpressionAST ) extends ModeAST
case class IndirectXModeAST( a: ExpressionAST ) extends ModeAST
case class IndirectYModeAST( a: ExpressionAST ) extends ModeAST


trait StatementAST extends AST

trait DirectiveAST extends StatementAST
case class LabelDirectiveAST( label: String ) extends DirectiveAST
case class OriginDirectiveAST( org: ExpressionAST ) extends DirectiveAST

trait AllocationAST extends StatementAST
case class InstructionAST( label: Option[String], mnemonic: String, mode: ModeAST ) extends AllocationAST
case class DataByteAST( label: Option[String], data: Seq[ExpressionAST] ) extends AllocationAST
case class DataWordAST( label: Option[String], data: Seq[ExpressionAST] ) extends AllocationAST
case class ReserveByteAST( label: Option[String], count: Option[ExpressionAST] ) extends AllocationAST
case class ReserveWordAST( label: Option[String], count: Option[ExpressionAST] ) extends AllocationAST