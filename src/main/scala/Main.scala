package xyz.hyperreal.mos6502

import xyz.hyperreal.options._


object Main extends App {
	
	val options = new Options( List(), List("-f"), Nil )
	val m = new Memory
	val cpu = new CPU6502( m )
	
	m add new RAM( "zero page", 0x0000, 0x00DF )
	m add new RAM( "stack", 0x0100, 0x01FF )
	m add new VideoRAM( 0x0200 )
	m add new RAM( "main", 0x1000, 0x2FFF )
	m add new StdIOChar( 0xE0 )
	m add new StdIOInt( 0xE1 )
	
	options parse args
	
	options get "-f" match {
		case None => monitor
		case Some( file ) => println( "load " + file )
	}
	
	def monitor {
		
	}
}