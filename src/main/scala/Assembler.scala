package xyz.hyperreal.mos6502

import java.io.File

import collection.mutable.{ListBuffer, HashMap}


case class AssemblerResult( symbols: Map[String,Int], segments: List[(Int, List[Byte])] )

object Assembler {
	
	def apply( mem: Memory, src: String ) {apply( mem, io.Source.fromString(src) )}
	
	def apply( mem: Memory, src: io.Source ) {
		val AssemblerResult(symbols, segments) = apply( src )
		
		for (((base, data), ind) <- segments zipWithIndex)
			mem add new ROM( "asm" + ind, base, data )
	}
	
	def apply( src: io.Source ): AssemblerResult = apply( new AssemblyParser(src).parse )
	
	def apply( ast: SourceAST ): AssemblerResult = {
		
		val symbols = new HashMap[String, Option[Int]]
		var pointer = 0
		var pointerExact = true
		var allknown = true
		
		def check( cond: Boolean, msg: String ) =
			if (cond)
				problem( msg )
				
		def problem( msg: String ) = sys.error( msg )
		
		def define( symbol: String, target: Option[Int] ) = {
			val v = symbols.get( symbol )
			
			check( v != Some( None ) && v != None, "duplicate symbol: " + symbol )
			symbols(symbol) = target
			
			if (target == None)
				allknown = false
				
			target != None
		}
		
		def defineHere( symbol: String ) = define( symbol, if (pointerExact) Some(pointer) else None )
		
		def eval( expr: ExpressionAST, mustknow: Boolean ) =
			expr match {
				case NumberExpressionAST( n ) => Known( n )
				case ReferenceExpressionAST( r ) =>
					symbols get r match {
						case None|Some( None ) if mustknow => problem( "undefined reference: " + r )
						case None => Unknown
						case Some( None ) => Knowable
						case Some( Some(a) ) => Known( a )
					}
			}
		
		def dblength( data: Seq[ExpressionAST] ) = {
			var length = 0
			
			data foreach {
				case StringExpressionAST( s ) =>
					length += s.length
				case expr =>
					length += 1
			}
			
			length
		}
		
		def pass1 {
			
			pointer = 0
			pointerExact = true
			allknown = true
			
			ast.statements foreach {
				case lab@LabelDirectiveAST( label, false ) =>
					lab.definite = defineHere( label )
				case LabelDirectiveAST( _, true ) =>
				case OriginDirectiveAST( expr ) =>
					eval( expr, false ) match {
						case Known( org ) =>
							check( org < 0 || org >= 0x10000, "origin must be less than 0x10000" )
							pointer = org
							pointerExact = true
						case Knowable|Unknown => problem( "origin must be known when the directive is encountered" )
					}
				case InstructionAST( _, _, Some(size) ) =>
					pointer += size
				case inst@InstructionAST( _, SimpleModeAST(_), None ) =>
					inst.size = Some( 1 )
					pointer += 1
				case inst@InstructionAST( _, OperandModeAST('immediate|'indirectX|'indirectY, _, _), None ) =>
					inst.size = Some( 2 )
					pointer += 2
				case inst@InstructionAST( "jmp"|"jsr", _, None ) =>
					inst.size = Some( 3 )
					pointer += 3
				case inst@InstructionAST( "bcc"|"bcs"|"beq"|"bmi"|"bne"|"bpl"|"bvc"|"bvs", _, None ) =>
					inst.size = Some( 2 )
					pointer += 2
		case inst@InstructionAST( _, mode@OperandModeAST(_, expr, _), None ) =>
					eval( expr, false ) match {
						case Known( a ) =>
							if (a < 0x100) {
								inst.size = Some( 2 )
								pointer += 2
							} else {
								inst.size = Some( 3 )
								pointer += 3
							}
							
							mode.operand = Some( a )
						case Knowable =>
							inst.size = Some( 3 )
							pointer += 3
						case Unknown =>
							pointerExact = false
					}
				case DataByteAST( data ) =>
					pointer += dblength( data )
				case DataWordAST( data ) =>
					pointer += data.length*2
				case ReserveByteAST( count ) =>
					pointer += 1
				case ReserveWordAST( count ) =>
					pointer += 2
			}
		
			if (!allknown)
				pass1
		}
		
		def pass2 = {
			
			val segments = new ListBuffer[(Int, List[Byte])]
			val segment = new ListBuffer[Byte]
			var base = 0
			
			def word( w: Int ) {
				segment += w.toByte
				segment += (w >> 8).toByte
			}
			
			def opcode( mnemonic: String, mode: Symbol ) =
				CPU.asm6502.get( (mnemonic, mode) ) match {
					case None => problem( "illegal instruction: " + (mnemonic, mode) )
					case Some( op ) => segment += op
				}

			def append =
				if (!segment.isEmpty) {
					segments += (base -> segment.toList)
					segment.clear
				}

			pointer = 0
			pointerExact = true
			
			ast.statements foreach {
				case OriginDirectiveAST( expr ) =>
					append
					pointer = eval( expr, false ).get
					base = pointer
				case InstructionAST( mnemonic, SimpleModeAST(mode), _ ) =>
					pointer += 1
					opcode( mnemonic, mode )
				case InstructionAST( mnemonic@("jmp"|"jsr"), OperandModeAST(mode, expr, operand), _ ) =>
					pointer += 3
					opcode( mnemonic, mode )					
					word( operand match {
						case None => eval( expr, true ).get
						case Some( t ) => t
					} )
				case InstructionAST( mnemonic, OperandModeAST('indirect, _, _), _ ) =>
					problem( "illegal instruction: " + (mnemonic, 'indirect) )
				case InstructionAST( mnemonic@("bcc"|"bcs"|"beq"|"bmi"|"bne"|"bpl"|"bvc"|"bvs"), OperandModeAST(_, expr, operand), _ ) =>
					val target = operand match {
						case None => eval( expr, true ).get
						case Some( t ) => t
					}
					
					pointer += 2
					opcode( mnemonic, 'relative )
					segment += (target - pointer).toByte
				case InstructionAST( mnemonic, OperandModeAST(mode@('immediate|'indirectX|'indirectY), expr, operand), _ ) =>
					pointer += 2
					opcode( mnemonic, mode )
					segment += (operand match {
						case None => eval( expr, true ).get
						case Some( t ) => t
					}).toByte
				case InstructionAST( mnemonic, OperandModeAST(mode, expr, operand), _ ) =>
					val o = operand match {
						case None => eval( expr, true ).get
						case Some( t ) => t
					}
					
					if (o < 0x100) {
						pointer += 2
						
						val m = mode match {
							case 'direct => 'zeroPage
							case 'directX => 'zeroPageX
							case 'directY => 'zeroPageY
						}
						
						opcode( mnemonic, m )
						segment += o.toByte
					} else {
						pointer += 3
						opcode( mnemonic, mode )
						word( o )
					}
				case InstructionAST( mnemonic, mode, _ ) =>
					problem( "pass2: uncaught instruction: " + (mnemonic, mode) )
				case DataByteAST( data ) =>
					data foreach {
						case StringExpressionAST( s ) =>
							segment ++= s.getBytes
						case expr =>
							segment += eval( expr, true ).get.toByte
					}
					
					pointer += dblength( data )
				case DataWordAST( data ) =>
					for (d <- data)
						word( eval(d, true).get )
					
					pointer += data.length*2
				case _ =>
			}
			
			append
			AssemblerResult( symbols map {case (k, v) => k -> v.get} toMap, segments.toList )
			
		}
		
		pass1
		pass2
		
	}
	
}