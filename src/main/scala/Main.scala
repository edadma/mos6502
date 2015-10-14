package xyz.hyperreal.mos6502


object Main extends App {
	
	val m = new Memory
	
	m add new Storage( 0x0000, 0x1000 )
	
	m writeByte (0x0100, 0x5a)
	printf( "%x\n", m readByte 0x100 )
}