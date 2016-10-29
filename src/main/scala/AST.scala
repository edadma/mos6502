package xyz.hyperreal.mos6502


trait AST

trait ExpressionAST extends AST
case class NumberExpressionAST( n: Int ) extends ExpressionAST
case class StringExpressionAST( s: String ) extends ExpressionAST
case class ReferenceExpressionAST( l: String ) extends ExpressionAST
case class BinaryExpressionAST( left: ExpressionAST, op: String, right: ExpressionAST ) extends ExpressionAST
case class UnaryExpressionAST( op: String, exp: ExpressionAST ) extends ExpressionAST

trait ModeAST extends AST

case class SimpleModeAST( mode: Symbol ) extends ModeAST	// 'implicit, 'accumulator

case class OperandModeAST( mode: Symbol, a: ExpressionAST, var operand: Option[Int] = None ) extends ModeAST

case class SourceAST( statements: List[StatementAST] ) extends AST

trait StatementAST extends AST

trait DirectiveAST extends StatementAST
case class LabelDirectiveAST( label: String, var definite: Boolean = false ) extends DirectiveAST
case class EquateDirectiveAST( equ: String, expr: ExpressionAST, var definite: Boolean = false ) extends DirectiveAST
case class OriginDirectiveAST( org: ExpressionAST, var value: Option[Int] = None ) extends DirectiveAST
case class IncludeDirectiveAST( file: ExpressionAST, var ast: Option[SourceAST] = None ) extends DirectiveAST

abstract class AllocationAST extends StatementAST {
//	var size: Option[Int] = None
}

case class InstructionAST( mnemonic: String, mode: ModeAST, var size: Option[Int] = None ) extends AllocationAST
case class DataByteAST( data: Seq[ExpressionAST] ) extends AllocationAST
case class DataWordAST( data: Seq[ExpressionAST] ) extends AllocationAST
case class ReserveByteAST( count: Option[ExpressionAST] ) extends AllocationAST
case class ReserveWordAST( count: Option[ExpressionAST] ) extends AllocationAST