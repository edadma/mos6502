package xyz.hyperreal.mos6502

import java.io.File


object TestMain extends App {
	
	println(
	Assembler(
		"""
		|; zero page definitions
		|
		|r
		|r0		rb
		|r1		rb
		|wr
		|wr0	rw
		|wr1	rw
		|
		|			org $9000
		|
		|			ldx	#$ff
		|			txs
		|
		|			lda	#$12
		|			sta r0
		|			lda	#$34
		|			sta r1
		|
		|			brk
		""".trim.stripMargin
	).segments.head._2 map ((b: Byte) => (b&0xff).toHexString)
	)
}

// 	val mem = new Memory
// 	
// 	mem add new RAM( "main", 0x0000, 0x7FFF )
// 	mem add new StdIOChar( 0x8000 )
// 	mem add new StdIOInt( 0x8001 )
// 	SREC( mem, new File("code/example.s") )
// 	println( mem )
// 	
// 	val cpu = new CPU6502( mem ) //{trace = true}
// 	
// 	cpu.reset
// 	cpu.run
