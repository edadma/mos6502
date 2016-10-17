package xyz.hyperreal.mos6502

import scala.swing._


class StdIOChar( val start: Int ) extends SingleAddressPort {
	
	val name = "StdIO char"
	
	def readByte( addr: Int ) = io.StdIn.readChar.toInt
	
	def writeByte( addr: Int, value: Int ) {
		print( value.toChar )
	}
	
}

class StdIOInt( val start: Int ) extends SingleAddressPort {
	
	val name = "StdIO int"
	
	def readByte( addr: Int ) = io.StdIn.readInt
	
	def writeByte( addr: Int, value: Int ) {
		print( value )
	}
	
}

class VideoRAM( start: Int ) extends RAM( "video", start, start + 1023 ) {
	
	require( start >= 0 )
	
}