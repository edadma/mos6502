package xyz.hyperreal.mos6502

import java.io.File

import collection.mutable.{ListBuffer, HashMap}


class Segment( val name: String ) {
	var base: Int = 0
	val data: ListBuffer[Byte] = new ListBuffer[Byte]
	var pointer = 0
	var pointerExact = true
}

case class AssemblerResult( symbols: Map[String, Any], segments: (String, Int, List[Byte]) )

object Assembler {
	
	def apply( src: String ): AssemblerResult = apply( io.Source.fromString(src) )
	
	def apply( src: io.Source ): AssemblerResult = apply( new AssemblyParser(src).parse )
	
	def apply( ast: SourceAST ): AssemblerResult = {
		
		val symbols = new HashMap[String, Option[Any]]
		var allknown = true
		var last: String = null
		val segments = new HashMap[String, Segment]
		var seg: Segment = null
		var count = 0
		
		switchSegment( name: String ) =
			segments get name match {
				case None =>
					seg = new Segment( "seg" + count )
					count += 1
					segments(seg.name) = seg
				case Some( s ) =>
					seg = s
			}
		
		def check( cond: Boolean, msg: String ) =
			if (cond)
				problem( msg )
				
		def problem( msg: String ) = sys.error( msg )
		
		def define( symbol: String, target: Option[Any] ) = {
			val v = symbols.get( symbol )
			
			check( v != Some( None ) && v != None, "duplicate symbol: " + symbol )
			symbols(symbol) = target
			
			if (target == None)
				allknown = false
				
			target != None
		}
		
		def defineHere( symbol: String ) = define( symbol, if (seg.pointerExact) Some(seg.pointer) else None )
		
		def ieval( expr: ExpressionAST, mustknow: Boolean ) = eval( expr, mustknow ).asInstanceOf[Know[Int]]
		
		def seval( expr: ExpressionAST, mustknow: Boolean ) = eval( expr, mustknow ).asInstanceOf[Know[String]]
		
		def eval( expr: ExpressionAST, mustknow: Boolean ): Know[Any] =
			expr match {
				case NumberExpressionAST( n ) => Known( n )
				case StringExpressionAST( s ) => Known( s )
				case ReferenceExpressionAST( r ) =>
					val r1 =
						if (r startsWith ".") {
							if (last eq null)
								problem( "no previous label" )
								
							last + r
						} else
							r

					symbols get r1 match {
						case None|Some( None ) if mustknow => problem( "undefined reference: " + r1 )
						case None => Unknown
						case Some( None ) => Knowable
						case Some( Some(a) ) => Known( a )
					}
				case UnaryExpressionAST( ">", expr ) => ieval( expr, mustknow ) map (_ >> 8)
				case UnaryExpressionAST( "<", expr ) => ieval( expr, mustknow ) map (_&0xFF)
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
		
		def pass1( ast: SourceAST ) {
			
			last = null
			allknown = true
			switchSegment( "seg0" )
			count = 1
			
			ast.statements foreach {
				case dir@LabelDirectiveAST( label, false ) =>
					val label1 =
						if (label startsWith ".") {
							if (last eq null)
								problem( "no previous label: " + label )
								
							last + label
						} else {
							last = label
							label
						}
					
					dir.definite = defineHere( label1 )
				case LabelDirectiveAST( label, true ) =>
					if (!(label startsWith "."))
						last = label
				case OriginDirectiveAST( expr ) =>
					ieval( expr, false ) match {
						case Known( org ) =>
							check( org < 0 || org >= 0x10000, "origin must be less than 0x10000" )
							seg.pointer = org
							seg.pointerExact = true
						case Knowable|Unknown => problem( "origin must be known when the directive is encountered" )
					}
				case dir@EquateDirectiveAST( equ, expr, None ) =>
					eval( expr, false ) match {
						case Known( v ) =>
							dir.value = Some( v )
							define( equ, dir.value )
						case Knowable|Unknown => problem( "equate must be known when the directive is encountered" )
					}
				case EquateDirectiveAST( _, _, Some(_) ) =>
				case inc@IncludeDirectiveAST( file, ast ) =>
					if (ast == None)
						inc.ast = Some( new AssemblyParser(io.Source.fromFile(seval(file, true).get)).parse )
						
					pass1( inc.ast.get )
				case InstructionAST( _, _, Some(size) ) =>
					seg.pointer += size
				case inst@InstructionAST( _, SimpleModeAST(_), None ) =>
					inst.size = Some( 1 )
					seg.pointer += 1
				case inst@InstructionAST( _, OperandModeAST('immediate|'indirectX|'indirectY, _, _), None ) =>
					inst.size = Some( 2 )
					seg.pointer += 2
				case inst@InstructionAST( "jmp"|"jsr", _, None ) =>
					inst.size = Some( 3 )
					seg.pointer += 3
				case inst@InstructionAST( "bcc"|"bcs"|"beq"|"bmi"|"bne"|"bpl"|"bvc"|"bvs", _, None ) =>
					inst.size = Some( 2 )
					seg.pointer += 2
				case inst@InstructionAST( _, mode@OperandModeAST(_, expr, _), None ) =>
					ieval( expr, false ) match {
						case Known( a ) =>
							if (a < 0x100) {
								inst.size = Some( 2 )
								seg.pointer += 2
							} else {
								inst.size = Some( 3 )
								seg.pointer += 3
							}
							
							mode.operand = Some( a )
						case Knowable =>
							inst.size = Some( 3 )
							seg.pointer += 3
						case Unknown =>
							seg.pointerExact = false
					}
				case DataByteAST( data ) =>
					seg.pointer += dblength( data )
				case DataWordAST( data ) =>
					seg.pointer += data.length*2
				case ReserveByteAST( None ) =>
					seg.pointer += 1
				case ReserveWordAST( None ) =>
					seg.pointer += 2
				case ReserveByteAST( Some(count) ) =>
					seg.pointer += ieval( count, true ).get
				case ReserveWordAST( Some(count) ) =>
					seg.pointer += ieval( count, true ).get*2
			}
		
			if (!allknown)
				pass1( ast )
		}
		
		def pass2( ast: SourceAST ): AssemblerResult = {
						
			last = null
			switchSegment( "seg0" )
			count = 1
			
			def word( w: Int ) {
				seg.data += w.toByte
				seg.data += (w >> 8).toByte
			}
			
			def opcode( mnemonic: String, mode: Symbol ) =
				CPU.asm6502.get( (mnemonic, mode) ) match {
					case None => problem( "illegal instruction: " + (mnemonic, mode) )
					case Some( op ) => seg.data += op
				}

			def append =
				if (!block.isEmpty) {
					blocks += (base -> block.toList)
					block.clear
				}
			
			ast.statements foreach {
				case LabelDirectiveAST( label, _ ) =>
					if (!(label startsWith "."))
						last = label
				case OriginDirectiveAST( expr ) =>
					append
					pointer = ieval( expr, false ).get
					base = pointer
				case IncludeDirectiveAST( _, ast ) =>
					pass2( ast.get )
				case InstructionAST( mnemonic, SimpleModeAST(mode), _ ) =>
					pointer += 1
					opcode( mnemonic, mode )
				case InstructionAST( mnemonic@("jmp"|"jsr"), OperandModeAST(mode, expr, operand), _ ) =>
					pointer += 3
					opcode( mnemonic, mode )					
					word( operand match {
						case None => ieval( expr, true ).get
						case Some( t ) => t
					} )
				case InstructionAST( mnemonic, OperandModeAST('indirect, _, _), _ ) =>
					problem( "illegal instruction: " + (mnemonic, 'indirect) )
				case InstructionAST( mnemonic@("bcc"|"bcs"|"beq"|"bmi"|"bne"|"bpl"|"bvc"|"bvs"), OperandModeAST(_, expr, operand), _ ) =>
					val target = operand match {
						case None => ieval( expr, true ).get
						case Some( t ) => t
					}
					
					pointer += 2
					opcode( mnemonic, 'relative )
					block += (target - pointer).toByte
				case InstructionAST( mnemonic, OperandModeAST(mode@('immediate|'indirectX|'indirectY), expr, operand), _ ) =>
					pointer += 2
					opcode( mnemonic, mode )
					block += (operand match {
						case None => ieval( expr, true ).get
						case Some( t ) => t
					}).toByte
				case InstructionAST( mnemonic, OperandModeAST(mode, expr, operand), _ ) =>
					val o = operand match {
						case None => ieval( expr, true ).get
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
						block += o.toByte
					} else {
						pointer += 3
						opcode( mnemonic, mode )
						word( o )
					}
				case InstructionAST( mnemonic, mode, _ ) =>
					problem( "pass2: uncaught instruction: " + (mnemonic, mode) )
				case DataByteAST( data ) =>
					for (d <- data)
						eval( d, true ).get match {
							case s: String =>
								block ++= s.getBytes
							case v: Int =>
								block += v.toByte
						}
					
					pointer += dblength( data )
				case DataWordAST( data ) =>
					for (d <- data)
						word( ieval(d, true).get )
					
					pointer += data.length*2
				case _ =>
			}
			
			append
			AssemblerResult( symbols map {case (k, v) => k -> v.get} toMap, blocks.toList )
			
		}
		
		pass1( ast )
		pass2( ast )
		
	}
	
}