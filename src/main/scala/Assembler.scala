package xyz.hyperreal.mos6502

import java.io.File

import collection.mutable.HashMap


object Assembler {
	
	def apply( ast: SourceAST ) = {
		
		val symbols = new HashMap[String, Option[Int]]
		var pointer = 0
		var pointerExact = true
		
		def check( cond: Boolean, msg: String ) =
			if (cond)
				problem( msg )
				
		def problem( msg: String ) = sys.error( msg )
		
		def define( symbol: String, target: Option[Int] ) = {
			val v = symbols.get( symbol )
			
			check( v != Some( None ) && v != None, "duplicate symbol: " + symbol )
			symbols(symbol) = target
			target != None
		}
		
		def defineHere( symbol: String ) = define( symbol, if (pointerExact) Some(pointer) else None )
		
		def pass1 {
			
			pointer = 0
			pointerExact = true
			
			ast.statements foreach {
				case LabelDirectiveAST( label ) =>
					defineHere( label )
				case OriginDirectiveAST( NumberExpressionAST(org) ) =>
					check( org < 0 || org >= 0x10000, "origin must be less than 0x10000" )
					pointer = org
					pointerExact = true
				case OriginDirectiveAST( _ ) =>
					problem( "origin must be literal" )
				case InstructionAST( mnemonic, mode ) =>
					
				case DataByteAST( data ) =>
					pointer += data.length
				case ReserveByteAST( count ) =>
					pointer += 1
				case ReserveWordAST( count ) =>
					pointer += 2
			}
		
		}
		
		def pass2 {
			
			pointer = 0
			pointerExact = true
			
			
			
		}
		
		pass1
		pass2
		
	}
	
// 	def apply( s: String ) = apply( io.Source.fromString(s) )
//
// 	def apply( s: io.Source ) = apply( new AssemblyParser(s).parse )
}