package xyz.hyperreal.mos6502

import collection.mutable.ArrayBuffer


object Hex {
	
	def apply( mem: Memory, hex: String ) = fromSource( mem, io.Source.fromString(hex) )
	
	def fromSource( mem: Memory, hex: io.Source ) {
		val buf = new ArrayBuffer[Byte]
		var start = -1
		var ptr = -1
		
		hex.getLines foreach { l =>
			val line = l.replace( " ", "" ).replace( "\t", "" )
			
			if (line != "") {
				val addr = hexWord( line, 0 )
				
				if (addr != ptr) {
					if (ptr != -1) {
						mem.add( new ROM(start, buf.toIndexedSeq) )
						buf.clear
					}
				
					start = addr
					ptr = addr
				}
				
				ptr = addr + (line.length - 5)/2
				
				for (i <- 5 until line.length by 2)
					buf += hexByte( line, i ).toByte
			}
		}
		
		mem.add( new ROM(start, buf.toIndexedSeq) )
	}
	
	def fromFile( mem: Memory, file: String ) = fromSource( mem, io.Source.fromFile(file) )
	
	private def hex( s: String, index: Int, digits: Int ) = Integer.parseInt( s.substring(index, index + digits), 16 )
		
	private def hexByte( s: String, index: Int ) = hex( s, index, 2 )
	
	private def hexWord( s: String, index: Int ) = hex( s, index, 4 )
	
}