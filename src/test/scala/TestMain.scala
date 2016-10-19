package xyz.hyperreal.mos6502

import java.io.File


object TestMain extends App {
	
	Assembler(
		new AssemblyParser(
			"""
			|     ; asdf
			|
			|    org   $9000
			|     ; asdf
			|
			|l1	ldx	a	;wert
			|     ; asdf
			|
			|	stx	asdf
			|     ; asdf
			|v1 db 5,"asdf"
			|v2 rb 3
			|v3 rb
			|l2  ;asdf
			|     ; asdf
			|
			""".trim.stripMargin
		).parse
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
