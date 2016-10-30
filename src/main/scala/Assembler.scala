package xyz.hyperreal.mos6502

import java.io.File

import collection.mutable.{ListBuffer, HashMap}


class Segment( val name: String ) {
	var base = 0
	val data: ListBuffer[Byte] = new ListBuffer[Byte]
	var pointer = 0
	var pointerExact = true
}

case class AssemblerResult( symbols: Map[String, Any], segments: List[(String, Int, List[Byte])] )

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
		
		def switchSegment( name: String = null ) {
			val name1 =
				if (name eq null)
					"seg" + count
				else
					name
			
			segments get name1 match {
				case None =>
					seg = new Segment( name1 )
					segments(name1) = seg
				case Some( s ) =>
					seg = s
			}
			
			if (name eq null)
				count += 1
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
								problem( "no label scope" )
								
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
		
		def reset {
			for ((_, s) <- segments) {
				s.pointer = s.base
				s.pointerExact = true
			}
			
			count = 0
			switchSegment()
		}
		
		def pass1( ast: SourceAST ) {
			
			last = null
			allknown = true
			reset
			
			ast.statements foreach {
				case dir@LabelDirectiveAST( label, false ) =>
					val label1 =
						if (label startsWith ".") {
							if (last eq null)
								problem( "no label scope: " + label )
								
							last + label
						} else {
							last = label
							label
						}
					
					dir.definite = defineHere( label1 )
				case LabelDirectiveAST( label, true ) =>
					if (!(label startsWith "."))
						last = label
				case dir@OriginDirectiveAST( expr, None ) =>
					ieval( expr, false ) match {
						case Known( org ) =>
							check( org < 0 || org >= 0x10000, "origin must be less than 0x10000" )
							
							if (seg.base != seg.pointer)
								switchSegment()
									
							seg.base = org
							seg.pointer = org
							seg.pointerExact = true
							dir.value = Some( org )
						case Knowable|Unknown => problem( "origin must be known when the directive is encountered" )
					}
				case OriginDirectiveAST( expr, Some(org) ) =>		
					if (seg.base != seg.pointer)
						switchSegment()
							
					seg.pointer = org
					seg.pointerExact = true
				case dir@EquateDirectiveAST( equ, expr, false ) =>
					eval( expr, false ) match {
						case Known( v ) =>
							dir.definite = define( equ, Some(v) )
// 						case Knowable|Unknown => problem( "equate must be known when the directive is encountered" )
						case Knowable|Unknown => dir.definite = define( equ, None )
					}
				case EquateDirectiveAST( _, _, true ) =>
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
				case dir@DataByteAST( data, None ) =>
					dir.size = Some( dblength(data) )
					seg.pointer += dir.size.get
				case DataByteAST( data, Some(size) ) =>
					seg.pointer += size
				case dir@DataWordAST( data, None ) =>
					dir.size = Some( data.length*2 )
					seg.pointer += dir.size.get
				case DataWordAST( data, Some(size) ) =>
					seg.pointer += size
				case dir@ReserveByteAST( None, _ ) =>
					dir.size = Some( 1 )
					seg.pointer += 1
				case dir@ReserveByteAST( Some(count), None ) =>
					seg.pointer += ieval( count, true ).get
				case ReserveByteAST( _, Some(size) ) =>
					seg.pointer += size
				case dir@ReserveWordAST( None, _ ) =>
					dir.size = Some( 2 )
					seg.pointer += 2
				case dir@ReserveWordAST( Some(count), None ) =>
					seg.pointer += ieval( count, true ).get*2
				case ReserveWordAST( _, Some(size) ) =>
					seg.pointer += size*2
			}
		
			if (!allknown)
				pass1( ast )
		}
		
		def pass2( ast: SourceAST ): AssemblerResult = {
						
			last = null
			reset
			
			def word( w: Int ) {
				seg.data += w.toByte
				seg.data += (w >> 8).toByte
			}
			
			def opcode( mnemonic: String, mode: Symbol ) =
				CPU.asm6502.get( (mnemonic, mode) ) match {
					case None => problem( "illegal instruction: " + (mnemonic, mode) )
					case Some( op ) => seg.data += op
				}
			
			ast.statements foreach {
				case LabelDirectiveAST( label, _ ) =>
					if (!(label startsWith "."))
						last = label
				case OriginDirectiveAST( expr, Some(org) ) =>
					if (seg.base != seg.pointer)
						switchSegment()
					
//					println( seg.name, seg.base)
					seg.pointer = org
					seg.pointerExact = true
				case IncludeDirectiveAST( _, ast ) =>
					pass2( ast.get )
				case InstructionAST( mnemonic, SimpleModeAST(mode), _ ) =>
					seg.pointer += 1
					opcode( mnemonic, mode )
				case InstructionAST( mnemonic@("jmp"|"jsr"), OperandModeAST(mode, expr, operand), _ ) =>
					seg.pointer += 3
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
					
					seg.pointer += 2
					opcode( mnemonic, 'relative )
					seg.data += (target - seg.pointer).toByte
				case InstructionAST( mnemonic, OperandModeAST(mode@('immediate|'indirectX|'indirectY), expr, operand), _ ) =>
					seg.pointer += 2
					opcode( mnemonic, mode )
					seg.data += (operand match {
						case None => ieval( expr, true ).get
						case Some( t ) => t
					}).toByte
				case InstructionAST( mnemonic, OperandModeAST(mode, expr, operand), _ ) =>
					val o = operand match {
						case None => ieval( expr, true ).get
						case Some( t ) => t
					}
					
					if (o < 0x100) {
						seg.pointer += 2
						
						val m = mode match {
							case 'direct => 'zeroPage
							case 'directX => 'zeroPageX
							case 'directY => 'zeroPageY
						}
						
						opcode( mnemonic, m )
						seg.data += o.toByte
					} else {
						seg.pointer += 3
						opcode( mnemonic, mode )
						word( o )
					}
				case InstructionAST( mnemonic, mode, _ ) =>
					problem( "pass2: uncaught instruction: " + (mnemonic, mode) )
				case DataByteAST( data, Some(size) ) =>
					for (d <- data)
						eval( d, true ).get match {
							case s: String =>
								seg.data ++= s.getBytes
							case v: Int =>
								seg.data += v.toByte
						}
					
					seg.pointer += size
				case DataWordAST( data, Some(size) ) =>
					for (d <- data)
						word( ieval(d, true).get )
					
					seg.pointer += size
				case ReserveByteAST( _, Some(size) ) =>
					seg.data ++= Seq.fill( size )( 0 )
					seg.pointer += size
				case ReserveWordAST( _, Some(size) ) =>
					seg.data ++= Seq.fill( size )( 0 )
					seg.pointer += size
				case _ =>
			}

			AssemblerResult( symbols map {case (k, v) => k -> v.get} toMap, segments.toList
				filterNot {case (_, s) => s.data isEmpty}
				map {case (name, s) => (name, s.base, s.data.toList)} )
			
		}
		
		pass1( ast )
		pass2( ast )
		
	}
	
}