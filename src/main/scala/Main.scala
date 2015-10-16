package xyz.hyperreal.mos6502


object Main extends App {
	
	val m = new Memory
	
	m add new RAM( 0x0000, 0x4000 )
	m add new StdInChar( 0x4000 )
	m add new StdInInt( 0x4001 )
	m add ROM( 0x8000,
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
		)
	m add ROM( 0xFFFC, 0x00, 0x80, 0x00, 0x00 )
	
	val cpu = new CPU6502( m ) //{trace = true}
	
	cpu.reset
}
