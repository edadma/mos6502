package xyz.hyperreal.mos6502

import java.io.File


object TestMain extends App {
	
	val mem = new Memory
	
	mem add new RAM( "main", 0x0000, 0x7FFF )
	mem add new StdIOChar( 0x8000 )
	mem add new StdIOInt( 0x8001 )
	println( Assembler( mem,
		"""
		|				org $9000
		|l1			db  "a\0"
		""".trim.stripMargin ) )
	println( mem )
	println( mem.readByte(0x9000).toHexString )
	val cpu = new CPU6502( mem ) //{trace = true}

// 	cpu.reset
// 	cpu.run
}