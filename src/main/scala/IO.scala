package xyz.hyperreal.mos6502


class StdInChar( val start: Int ) extends SingleAddressPort {
	
	def readByte( addr: Int ) = io.StdIn.readChar.toInt
	
	def writeByte( addr: Int, value: Int ) {
		print( value.toChar )
	}
	
}

class StdInInt( val start: Int ) extends SingleAddressPort {
	
	def readByte( addr: Int ) = io.StdIn.readInt
	
	def writeByte( addr: Int, value: Int ) {
		print( value )
	}
	
}
