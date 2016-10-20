package xyz.hyperreal.mos6502

import java.io.File


object TestMain extends App {
	
	val mem = new Memory
	
	mem add new RAM( "main", 0x0000, 0x7FFF )
	mem add new StdIOChar( 0x8000 )
	mem add new StdIOInt( 0x8001 )
	Assembler( mem,
		"""
		|				org $9000
		|				lda #'\n'
		""".trim.stripMargin )
	println( mem )
	println( mem.readByte(0x9001).toHexString )
	val cpu = new CPU6502( mem ) //{trace = true}

// 	cpu.reset
// 	cpu.run
}

// 		|; zero page definitions
// 		|
// 		|r
// 		|r0		rb
// 		|r1		rb
// 		|wr
// 		|wr0	rw
// 		|wr1	rw
// 		|
// 		|			org $8000
// 		|
// 		|cout	rb
// 		|iout	rb
// 		|
// 		|			org $9000
// 		|
// 		|			ldx	#$ff
// 		|			txs
// 		|
// 		|			lda	#$41
// 		|			sta r0
// 		|			lda	r0
// 		|			sta cout
// 		|			lda	#$0a
// 		|			sta cout
// 		|
// 		|			brk
// 		|
// 		|			org	$fffc
// 		|			dw	$9000
