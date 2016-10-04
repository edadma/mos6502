package xyz.hyperreal.mos6502

import scala.swing._


class StdIOChar( val start: Int ) extends SingleAddressPort {
	
	def readByte( addr: Int ) = io.StdIn.readChar.toInt
	
	def writeByte( addr: Int, value: Int ) {
		print( value.toChar )
	}
	
}

class StdIOInt( val start: Int ) extends SingleAddressPort {
	
	def readByte( addr: Int ) = io.StdIn.readInt
	
	def writeByte( addr: Int, value: Int ) {
		print( value )
	}
	
}

class VideoRAM( start: Int ) extends RAM( start, start + 1023 ) {
	
	require( start >= 0 )

	override def toString = "video " + super.toString
	
}
