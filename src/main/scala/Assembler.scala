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

	def string = """".*"""".r ^^ {s => StringExpressionAST( s.substring(1, s.length - 1) )}
	
	def label = "[_0-9a-zA-Z]+".r
	
	def reference = label ^^ {ReferenceExpressionAST}
	
	def space = "[ \t]+".r
	
	def os = opt(space)
	
	def nl = "[\r\n]+".r
	
	def comment = ";.*".r
	
	def blank = os ~ opt(comment) ~ nl
	
	def source = rep(blank) ~> repsep(statement, rep(blank)) <~ (rep(blank) ~ opt(os ~ opt(comment)))
	
	def statement =
		instruction |
		directive
	
	def expression: Parser[ExpressionAST] =
		"<" ~> expression ^^ {UnaryExpressionAST( "<", _ )} |
		">" ~> expression ^^ {UnaryExpressionAST( ">", _ )} |
		number |
		reference
		
	def mode =
		"#" ~> expression ^^ {ImmediateModeAST} |
		"a|A".r <~ guard(not("[a-zA-Z]"r)) ^^^ AccumulatorModeAST |
		expression <~ ("," ~ os ~ "x|X".r) ^^ {DirectXModeAST(_)} |
 		expression <~ ("," ~ os ~ "y|Y".r) ^^ {DirectYModeAST(_)} |
		expression ^^ {DirectModeAST(_)} |
		"(" ~> expression <~ ")" ^^ {IndirectModeAST} |
		"(" ~> expression <~ ("," ~ os ~ "x|X".r ~ ")") ^^ {IndirectXModeAST} |
		"(" ~> expression <~ (")," ~ os ~ "y|Y".r) ^^ {IndirectModeAST}
		
	def instruction =
		(opt(label) <~ space) ~ mnemonic ~ opt(space ~> mode) ^^ {
			case label ~ mnemonic ~ None =>
				InstructionAST( label, mnemonic, ImplicitModeAST )
			case label ~ mnemonic ~ Some( mode ) =>
				InstructionAST( label, mnemonic, mode )}
	
	def directive =
		(space ~ "org|ORG".r ~ space) ~> expression ^^ {OriginDirectiveAST} |
		(opt(label) <~ (space ~ "db|DB".r ~ space)) ~ rep1sep(expression | string, os ~ "," ~ os) ^^ {
			case label ~ exprs => DataByteAST( label, exprs )} |
		(opt(label) <~ (space ~ "dw|DW".r ~ space)) ~ rep1sep(expression, os ~ "," ~ os) ^^ {
			case label ~ exprs => DataWordAST( label, exprs )} |
		(opt(label) <~ (space ~ "rb|RB".r)) ~ opt(space ~> expression) ^^ {
			case label ~ expr => ReserveByteAST( label, expr )} |
		(opt(label) <~ (space ~ "rw|RW".r)) ~ opt(space ~> expression) ^^ {
			case label ~ expr => ReserveWordAST( label, expr )} |
		label ^^ {LabelDirectiveAST}
		
	def apply( input: String ) = parseAll( source, input ) match {
		case Success( result, _ ) => println( result )
		case failure: NoSuccess => scala.sys.error( failure.msg )
	}
	
}