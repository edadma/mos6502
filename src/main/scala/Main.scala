package xyz.hyperreal.mos6502


object Main extends App {
	
	val m = new Memory
	
	m add new Storage( 0x0000, 0x1000 )
	
	m add
		new Port( 0xFF00, 1 ) {
			def readByte( addr: Int ) = io.StdIn.readChar.toInt
			
			def writeByte( addr: Int, value: Int ) {
				print( value.toChar )
			}
		}
	m add
		new Port( 0xFF01, 1 ) {
			def readByte( addr: Int ) = io.StdIn.readInt
			
			def writeByte( addr: Int, value: Int ) {
				print( value )
			}
		}
		
	m writeByte( 0x0100, 0x5a )
	printf( "%x\n", m readByte 0x100 )
	
	val in = m readByte( 0xFF01 )
	
	m writeByte( 0xFF01, in )
	m writeByte( 0xFF00, '\n' )
}