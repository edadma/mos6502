package xyz.hyperreal.mos6502

import java.io.File

import collection.mutable.{ListBuffer, HashMap}


class Block( var base: Int = 0, val data: ListBuffer[Byte] = new ListBuffer[Byte] )

class Segment( val name: String, val segments: ListBuffer[Block] = new ListBuffer[Block] )

case class AssemblerResult( symbols: Map[String, Any], segments: List[(String, List[(Int, List[Byte])])] )

object Assembler {
	
	def apply( mem: Memory, src: String ): Map[String, Any] = apply( mem, io.Source.fromString(src) )
	
	def apply( mem: Memory, src: io.Source ): Map[String, Any] = {
		val AssemblerResult(symbols, segments) = apply( src )
		
		for (((base, data), ind) <- segments zipWithIndex)
			mem add new ROM( "asm" + ind, base, data )
			
		symbols
	}
	
	def apply( src: String ): AssemblerResult = apply( io.Source.fromString(src) )
	
	def apply( src: io.Source ): AssemblerResult = apply( new AssemblyParser(src).parse )
	
	def apply( ast: SourceAST ): AssemblerResult = {
		
		val symbols = new HashMap[String, Option[Any]]
		var pointer = 0
		var pointerExact = true
		var allknown = true
		
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
		
		def defineHere( symbol: String ) = define( symbol, if (pointerExact) Some(pointer) else None )
		
		def ieval( expr: ExpressionAST, mustknow: Boolean ) = eval( expr, mustknow ).asInstanceOf[Know[Int]]
		
		def seval( expr: ExpressionAST, mustknow: Boolean ) = eval( expr, mustknow ).asInstanceOf[Know[String]]
		
		def eval( expr: ExpressionAST, mustknow: Boolean ): Know[Any] =
			expr match {
				case NumberExpressionAST( n ) => Known( n )
				case StringExpressionAST( s ) => Known( s )
				case ReferenceExpressionAST( r ) =>
					symbols get r match {
						case None|Some( None ) if mustknow => problem( "undefined reference: " + r )
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
			
			pointer = 0
			pointerExact = true
			allknown = true
			
			ast.statements foreach {
				case dir@LabelDirectiveAST( label, false ) =>
					dir.definite = defineHere( label )
				case LabelDirectiveAST( _, true ) =>
				case OriginDirectiveAST( expr ) =>
					ieval( expr, false ) match {
						case Known( org ) =>
							check( org < 0 || org >= 0x10000, "origin must be less than 0x10000" )
							pointer = org
							pointerExact = true
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
					ieval( expr, false ) match {
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
				case ReserveByteAST( None ) =>
					pointer += 1
				case ReserveWordAST( None ) =>
					pointer += 2
				case ReserveByteAST( Some(count) ) =>
					pointer += ieval( count, true ).get
				case ReserveWordAST( Some(count) ) =>
					pointer += ieval( count, true ).get*2
			}
		
			if (!allknown)
				pass1( ast )
		}
		
		def pass2( ast: SourceAST ): AssemblerResult = {
			
			val segments = new HashMap[String, Segment]
			val blocks = new ListBuffer[Block]
			val block = new Block
			
			def word( w: Int ) {
				block += w.toByte
				block += (w >> 8).toByte
			}
			
			def opcode( mnemonic: String, mode: Symbol ) =
				CPU.asm6502.get( (mnemonic, mode) ) match {
					case None => problem( "illegal instruction: " + (mnemonic, mode) )
					case Some( op ) => block += op
				}

			def append =
				if (!block.isEmpty) {
					blocks += (base -> block.toList)
					block.clear
				}

			pointer = 0
			pointerExact = true
			
			ast.statements foreach {
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