package xyz.hyperreal.mos6502


class StdInChar( addr: Int ) extends Port( addr, 1 ) {
	
	def readByte( addr: Int ) = io.StdIn.readChar.toInt
	
	def writeByte( addr: Int, value: Int ) {
		print( value.toChar )
	}
	
}

class StdInInt( addr: Int ) extends Port( addr, 1 ) {
	
	def readByte( addr: Int ) = io.StdIn.readInt
	
	def writeByte( addr: Int, value: Int ) {
		print( value )
	}
	
}
