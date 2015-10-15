package xyz.hyperreal


package object mos6502 {
	
	def hexByte( a: Int ) = "%02x".format( a&0xFF ).toUpperCase
	
	def hexWord( a: Int ) = hexByte( a>>8 ) + hexByte( a )
	
}