package xyz.hyperreal.mos6502


object Main extends App {
	
	val m = new Memory
	
	m add new RAM( 0x0000, 0x4000 )
	m add new StdInChar( 0x4000 )
	m add new StdInInt( 0x4001 )
	m add new ROM( 0x8000, Seq(
		0xA9, 0x12,					// LDA #$12
		0x8D, 0x01, 0x40,		// STA $4001
		0xA9, '\n',					// LDA #'\n'
		0x8D, 0x00, 0x40,		// STA $4000
		0x00
		) )
	m add new ROM( 0xFFFC, Seq(0x00, 0x80) )
	
	val cpu = new CPU6502( m ) //{trace = true}
	
	cpu.reset
}