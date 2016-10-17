package xyz.hyperreal.mos6502

import java.io.File


object TestMain extends App {
	
	val m = new Memory
	
	m add new RAM( 0x0000, 0x00DF )
	m add new RAM( 0x0100, 0x01FF )
	m add new VideoRAM( 0x0200 )
	m add new RAM( 0x1000, 0x2FFF )
	m add new StdIOChar( 0xE0 )
	m add new StdIOInt( 0xE1 )
	SREC( m, new File("code/example.s") )
	println( m )
	
	val cpu = new CPU6502( m ) //{trace = true}
	
	cpu.reset
}