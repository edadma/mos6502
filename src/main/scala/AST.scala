package xyz.hyperreal.mos6502


trait AST

trait ExpressionAST extends AST
case class NumberExpressionAST( n: Int ) extends ExpressionAST
case class LabelExpressionAST( l: String ) extends ExpressionAST
case class BinaryExpressionAST( left: ExpressionAST, op: String, right: ExpressionAST ) extends ExpressionAST
case class UnaryExpressionAST( op: String, exp: ExpressionAST ) extends ExpressionAST

trait ModeAST
case object ImplicitModeAST
case class ImmediateModeAST( v: ExpressionAST )
case class AddressModeAST( a: ExpressionAST, var absolute: Boolean = false )
case class AddressXModeAST( a: ExpressionAST, var absolute: Boolean = false )
case class AddressYModeAST( a: ExpressionAST, var absolute: Boolean = false )
case class IndirectXModeAST( a: ExpressionAST )
case class IndirectYModeAST( a: ExpressionAST )


trait StatementAST extends AST

trait DirectiveAST extends StatementAST
case class LabelDirectiveAST( label: String ) extends DirectiveAST

trait AllocationAST extends StatementAST

trait DataAST extends AllocationAST

case class InstructionAST( label: Option[String], inst: String, mode: ModeAST ) extends AllocationAST
