package xyz.hyperreal


package object mos6502 {
	
	def hexByte( a: Int ) = "%02x".format( a&0xFF ).toUpperCase
	
	def hexWord( a: Int ) = hexByte( a>>8 ) + hexByte( a )
	
	def isHex( s: String ) = !s.isEmpty && s.forall( c => "012345679abcdefABCDEF" contains c )
	
	def hex( s: String ) = Integer.parseInt( s, 16 )
	
}