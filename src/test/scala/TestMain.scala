package xyz.hyperreal.mos6502

import java.io.File


object TestMain extends App {
	
	val mem = new Memory
	mem add new RAM( "zp", 0x0000, 0x00FF )
	mem add new RAM( "stack", 0x0100, 0x01FF )
	mem add new RAM( "main", 0x0600, 0x7FFF )
	mem add new RAM( "video", 0x0200, 0x05FF )
//	mem add new VideoRAM( 0x0200, 32, 32, Vector(0x000000, 0xffffff, 0x880000, 0xaaffee, 0xcc44cc, 0x00cc55, 0x0000aa, 0xeeee77, 0xdd8855, 0x664400, 0xff7777, 0x333333, 0x777777, 0xaaff66, 0x0088ff, 0xbbbbbb) )
	mem add new StdIOChar( 0x8000 )
	mem add new StdIOInt( 0x8001 )
	println( Assembler( mem,
		"""
		|				org $200
		|video		rb
		|				org $9000
		|				ldx	#1
		|				lda colors,x
		|				sta $8001
		|				sta ($0),y
		|				lda #1
		|				sta $200
		|				brk
		|colors	dcb 0,2,0,2,2,8,2,8,8,7,8,7,7,1,7,1,1,7,1,7,7,8,7,8,8,2,8,2,2,0,2,0
		|				org $fffc
		|				dw		$9000
		""".trim.stripMargin ) )
	println( mem )
	val cpu = new CPU6502( mem ) //{trace = true}

	cpu.reset
	cpu.run
}