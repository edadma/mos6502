package xyz.hyperreal.mos6502

import util.parsing.combinator.RegexParsers


object Assembler extends RegexParsers {

	override val skipWhitespace = false
	
 	def mnemonic: Parser[String] =
		"adc|and|asl|bcc|bcs|beq|bit|bmi|bne|bpl|brk|bvc|bvs|clc|cld|cli|clv|cmp|cpx|cpy|dec|dex|dey|eor|inc|inx|iny|jmp|jsr|lda|ldx|ldy|lsr|nop|ora|pha|php|pla|plp|rol|ror|rti|rts|sbc|sec|sed|sei|sta|stx|sty|tax|tay|tsx|txa|txs|tya|ADC|AND|ASL|BCC|BCS|BEQ|BIT|BMI|BNE|BPL|BRK|BVC|BVS|CLC|CLD|CLI|CLV|CMP|CPX|CPY|DEC|DEX|DEY|EOR|INC|INX|INY|JMP|JSR|LDA|LDX|LDY|LSR|NOP|ORA|PHA|PHP|PLA|PLP|ROL|ROR|RTI|RTS|SBC|SEC|SED|SEI|STA|STX|STY|TAX|TAY|TSX|TXA|TXS|TYA".r ^^ {_.toLowerCase}
	
	def number = """[0-9]+|\$[0-9a-fA-F]+""".r ^^ {
		s =>
			NumberExpressionAST(
				if (s startsWith "$")
					Integer.parseInt( s substring 1, 16 )
				else
					s.toInt
			)
	}

	def label = "[_0-9a-zA-Z]+".r ^^ {LabelExpressionAST}
	
	def space = "[ \t]+".r
	
	def os = opt(space)
	
	def nl = "[\r\n]+".r
	
	def comment = ";.*".r
	
	def blank = os ~ opt(comment) ~ nl
	
	def source/*: Parser[List[InstructionAST]]*/ = rep(blank) ~> rep1sep(statement, nl) <~ rep(blank)
	
	def statement = instruction <~ (os ~ opt(comment))
	
	def expression: Parser[ExpressionAST] =
		"<" ~> expression ^^ {UnaryExpressionAST( "<", _ )} |
		">" ~> expression ^^ {UnaryExpressionAST( ">", _ )} |
		number |
		label
		
	def mode =
		"#" ~> expression ^^ {ImmediateModeAST} |
		"a|A".r <~ guard(not("[a-zA-Z]"r)) ^^^ AccumulatorModeAST |
		label <~ ("," ~ os ~ "x|X".r) ^^ {DirectXModeAST(_)} |
 		label <~ ("," ~ os ~ "y|Y".r) ^^ {DirectYModeAST(_)} |
		label ^^ {DirectModeAST(_)} |
		"(" ~> label <~ ")" ^^ {IndirectModeAST} |
		"(" ~> label <~ ("," ~ os ~ "x|X".r ~ ")") ^^ {IndirectXModeAST} |
		"(" ~> label <~ (")," ~ os ~ "y|Y".r) ^^ {IndirectModeAST}
		
	def instruction = space ~> mnemonic ~ opt(space ~> mode) ^^ {
		case mnemonic ~ None =>
			InstructionAST( None, mnemonic, ImplicitModeAST )
		case mnemonic ~ Some( mode ) =>
			InstructionAST( None, mnemonic, mode )
	}
	
	def apply( input: String ) = parseAll( source, input ) match {
		case Success( result, _ ) => println( result )
		case failure: NoSuccess => scala.sys.error( failure.msg )
	}
	
}