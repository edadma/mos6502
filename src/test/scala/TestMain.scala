package xyz.hyperreal.mos6502

import java.io.File


object TestMain extends App {
	
	val mem = new Memory
	mem add new RAM( "zp", 0x0000, 0x00FF )
	mem add new RAM( "stack", 0x0100, 0x01FF )
	mem add new RAM( "main", 0x0600, 0x7FFF )
	println( Assembler( mem, io.Source.fromFile("hw1.asm") ) map
		{case (k, v) => (k, if (v.isInstanceOf[Int]) v.asInstanceOf[Int].toHexString else v.toString)} )
	println( mem )
	val cpu = new CPU6502( mem ) //{trace = true}

	cpu.reset
	cpu.run
}

/*
	println( Assembler( mem,
		"""
		|				org $9000
		|start	
		|				brk
		|				org $fffc
		|				dw		start
		""".trim.stripMargin ) )
*/