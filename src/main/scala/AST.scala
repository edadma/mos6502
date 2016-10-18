package xyz.hyperreal.mos6502


trait AST

trait ExpressionAST extends AST
case class NumberExpressionAST( n: Int ) extends ExpressionAST
case class LabelExpressionAST( l: String ) extends ExpressionAST
case class BinaryExpressionAST( left: ExpressionAST, op: String, right: ExpressionAST ) extends ExpressionAST
case class UnaryExpressionAST( op: String, exp: ExpressionAST ) extends ExpressionAST

trait ModeAST
case object ImplicitModeAST extends ModeAST
case object AccumulatorModeAST extends ModeAST
case class ImmediateModeAST( v: ExpressionAST ) extends ModeAST
case class DirectModeAST( a: ExpressionAST, var absolute: Boolean = false ) extends ModeAST
case class DirectXModeAST( a: ExpressionAST, var absolute: Boolean = false ) extends ModeAST
case class DirectYModeAST( a: ExpressionAST, var absolute: Boolean = false ) extends ModeAST
case class IndirectModeAST( v: ExpressionAST ) extends ModeAST
case class IndirectXModeAST( a: ExpressionAST ) extends ModeAST
case class IndirectYModeAST( a: ExpressionAST ) extends ModeAST


trait StatementAST extends AST

trait DirectiveAST extends StatementAST
case class LabelDirectiveAST( label: String ) extends DirectiveAST

trait AllocationAST extends StatementAST

trait DataAST extends AllocationAST

case class InstructionAST( label: Option[String], mnemonic: String, mode: ModeAST ) extends AllocationAST
