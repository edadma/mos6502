package xyz.hyperreal.mos6502


object Main extends App {
	
	val m = new Memory
	
	m add new RAM( 0x0000, 0x00DF )
	m add new RAM( 0x0100, 0x01FF )
	m add new VideoRAM( 0x0200 )
	m add new StdInChar( 0xE0 )
	m add new StdInInt( 0xE1 )
// 	m add ROM( 0x0600,
// 		)
	Hex( m, """
		0600: a9 00 85 00 e6 00 a5 00 c9 06 d0 01 00 85 e1 a9 
		0610: 0a 85 e0 4c 04 06 
		""" )
	m add ROM( 0xFFFC, 0x00, 0x06, 0x00, 0x00 )
	println( m )
	
	val cpu = new CPU6502( m ) //{trace = true}
	
	cpu.reset
}

/*
		0xA9, 0x00,					// 				LDA #$00
		0x85, 0x00,					// 				STA $00
		0xE6, 0x00,					// LOOP:	INC $00
		0xA5, 0x00,					// 				LDA $00
		0xC9, 0x06,					// 				CMP #$06
		0xD0, 0x01,					// 				BNE PRINT
		0x00,								// 				BRK
		0x8D, 0x01, 0x40,		// PRINT:	STA $4001
		0xA9, '\n',					// 				LDA #'\n'
		0x8D, 0x00, 0x40,		// 				STA $4000
		0x4C, 0x04, 0x80		// 				JMP LOOP
*/

/*
				LDA #$00
				STA $00
LOOP:	INC $00
				LDA $00
				CMP #$06
				BNE PRINT
				BRK
PRINT:	STA $4001
				LDA #'\n'
				STA $4000
				JMP LOOP
*/