package xyz.hyperreal.mos6502

import java.io.File

import collection.immutable.PagedSeq

import util.parsing.combinator.RegexParsers
import util.parsing.input.PagedSeqReader


class AssemblyParser( input: io.Source ) extends RegexParsers {

	override val skipWhitespace = false
	
 	def mnemonic: Parser[String] =
		"adc|and|asl|bcc|bcs|beq|bit|bmi|bne|bpl|brk|bvc|bvs|clc|cld|cli|clv|cmp|cpx|cpy|dec|dex|dey|eor|inc|inx|iny|jmp|jsr|lda|ldx|ldy|lsr|nop|ora|pha|php|pla|plp|rol|ror|rti|rts|sbc|sec|sed|sei|sta|stx|sty|tax|tay|tsx|txa|txs|tya|ADC|AND|ASL|BCC|BCS|BEQ|BIT|BMI|BNE|BPL|BRK|BVC|BVS|CLC|CLD|CLI|CLV|CMP|CPX|CPY|DEC|DEX|DEY|EOR|INC|INX|INY|JMP|JSR|LDA|LDX|LDY|LSR|NOP|ORA|PHA|PHP|PLA|PLP|ROL|ROR|RTI|RTS|SBC|SEC|SED|SEI|STA|STX|STY|TAX|TAY|TSX|TXA|TXS|TYA".r ^^ {_.toLowerCase}
	
	def numberLiteral = """[0-9]+|\$[0-9a-fA-F]+""".r ^^ {
		s =>
			NumberExpressionAST(
				if (s startsWith "$")
					Integer.parseInt( s substring 1, 16 )
				else
					s.toInt
			)
	}

	def escape( s: String ) = s.
				replaceAll("""\\0""", "\u0000").
				replaceAll("""\\\\""", "\\").
				replaceAll("""\\'""", "'").
				replaceAll("""\\"""", "\"").
				replaceAll("""\\b""", "\b").
				replaceAll("""\\f""", "\f").
				replaceAll("""\\t""", "\t").
				replaceAll("""\\r""", "\r").
				replaceAll("""\\n""", "\n")

	def charLiteral = """'(?:[^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt0])'""".r ^^ {
		s =>
			NumberExpressionAST( escape(s.substring(1, s.length - 1)).head.toInt )
	}

	def string = """"(?:[^"\x00-\x1F\x7F\\]|\\[\\'"bfnrt0])*"""".r ^^ {s => s.substring(1, s.length - 1)}
	
	def stringLiteral = string ^^ {s => 	StringExpressionAST( escape(s) )}
	
	def label = """(?:\.[_0-9a-zA-Z]+|[_a-zA-Z][_0-9a-zA-Z]*):?""".r ^^ {
		s =>
			if (s endsWith ":")
				s dropRight 1
			else
				s
	}
	
	def reference = label ^^ {ReferenceExpressionAST}
	
	def space = "[ \t]+".r
	
	def os = opt(space)
	
	def nl = "[\r\n]+".r
	
	def comment = ";.*".r
	
	def blank = os ~ opt(comment) ~ nl
	
	def source: Parser[SourceAST] = rep(blank) ~> repsep(statement, rep1(blank)) <~ (rep(blank) ~ opt(os ~ opt(comment))) ^^ {l => SourceAST( l.flatten )}
	
	def statement =
		instruction |
		directive
	
	def expression: Parser[ExpressionAST] =
		"<" ~> expression ^^ {UnaryExpressionAST( "<", _ )} |
		">" ~> expression ^^ {UnaryExpressionAST( ">", _ )} |
		numberLiteral |
		charLiteral |
		reference |
		stringLiteral
	
	def mode =
		"#" ~> expression ^^ {OperandModeAST( 'immediate, _ )} |
		"a|A".r <~ guard(not("[_0-9a-zA-Z]"r)) ^^^ SimpleModeAST( 'accumulator ) |
		expression <~ ("," ~ os ~ "x|X".r) ^^ {OperandModeAST( 'directX, _ )} |
 		expression <~ ("," ~ os ~ "y|Y".r) ^^ {OperandModeAST( 'directY, _ )} |
		expression ^^ {OperandModeAST( 'direct, _ )} |
		"(" ~> expression <~ ("," ~ os ~ "x|X".r ~ ")") ^^ {OperandModeAST( 'indirectX, _ )} |
		"(" ~> expression <~ (")," ~ os ~ "y|Y".r) ^^ {OperandModeAST( 'indirectY, _ )} |
		"(" ~> expression <~ ")" ^^ {OperandModeAST( 'indirect, _ )}
		
	def instruction =
		(opt(label) <~ space) ~ mnemonic ~ opt(space ~> mode) ^^ {
			case None ~ mnemonic ~ None =>
				List( InstructionAST(mnemonic, SimpleModeAST('implicit)) )
			case None ~ mnemonic ~ Some( mode ) =>
				List( InstructionAST(mnemonic, mode) )
			case Some( label ) ~ mnemonic ~ None =>
				List( LabelDirectiveAST(label), InstructionAST(mnemonic, SimpleModeAST('implicit)) )
			case Some( label ) ~ mnemonic ~ Some( mode ) =>
				List( LabelDirectiveAST(label), InstructionAST(mnemonic, mode) )
		}
	
	def directive =
		("#include" ~ space) ~> string ^^ {
			file =>
				new AssemblyParser( io.Source.fromFile(file) ).parse.statements
			} |
		(space ~ "org|ORG|.org|.ORG".r ~ space) ~> expression ^^ {e => List( OriginDirectiveAST(e) )} |
		(space ~ "seg|SEG".r ~ space) ~> string ^^ {name => List( SegmentDirectiveAST(name) )} |
		(label <~ (space ~ "equ|EQU|=".r ~ space)) ~ expression ^^ {
			case equ ~ expr =>
				List( EquateDirectiveAST(equ, expr) )
			} |
		(opt(label) <~ (space ~ "db|DB|dcb|DCB|.byte|.BYTE".r ~ space)) ~ rep1sep(expression, os ~ "," ~ os) ^^ {
			case None ~ exprs =>
				List( DataByteAST(exprs) )
			case Some( label ) ~ exprs =>
				List( LabelDirectiveAST(label), DataByteAST(exprs) )
			} |
		(opt(label) <~ (space ~ "dw|DW|dcw|DCW|.word|.WORD".r ~ space)) ~ rep1sep(expression, os ~ "," ~ os) ^^ {
			case None ~ exprs =>
				List( DataWordAST(exprs) )
			case Some( label ) ~ exprs =>
				List( LabelDirectiveAST(label), DataWordAST(exprs) )
			} |
		(opt(label) <~ (space ~ "rb|RB|dsb|DSB".r)) ~ opt(space ~> expression) ^^ {
			case None ~ expr =>
				List( ReserveByteAST(expr) )
			case Some( label ) ~ expr =>
				List( LabelDirectiveAST(label), ReserveByteAST(expr) )
			} |
		(opt(label) <~ (space ~ "rw|RW|dsw|DSW".r)) ~ opt(space ~> expression) ^^ {
			case None ~ expr =>
				List( ReserveWordAST(expr) )
			case Some( label ) ~ expr =>
				List( LabelDirectiveAST(label), ReserveWordAST(expr) )
			} |
		label ^^ {s => List( LabelDirectiveAST(s) )}
		
	def parse = parseAll( source, new PagedSeqReader(PagedSeq.fromSource(input)) ) match {
		case Success( result, _ ) => result
		case NoSuccess( msg, input ) => sys.error( msg + ": " + input.pos + '\n' + input.pos.longString.replace( "\n\n", "\n" ) )
	}
	
}