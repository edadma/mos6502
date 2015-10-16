package xyz.hyperreal.mos6502


object Instructions extends Flags {

	//
	// cc = 01
	//
	def ora( cpu: CPU, addr: Int ) = {
		cpu.A |= cpu.read( addr )
		cpu.flags( cpu.A )
	}
	
	def sta( cpu: CPU, addr: Int ) = cpu.write( addr, cpu.A )
	
	def lda( cpu: CPU, addr: Int ) = cpu.A = cpu.flags( cpu.read(addr) )
	
	def cmp( cpu: CPU, addr: Int ) = {
		val diff = cpu.A - cpu.read( addr )
		cpu.flags( diff )
		cpu.status(C) = diff >= 0
	}
	
	//
	// cc = 10
	//
	def inc( cpu: CPU, addr: Int ) = cpu.write( addr, cpu.flags(cpu.read(addr) + 1) )
	
	//
	// cc = 10 (X)
	//
	def ldx( cpu: CPU, addr: Int ) = cpu.X = cpu.flags( cpu.read(addr) )
	
	//
	// simple
	//
	def txs = (cpu: CPU) => cpu.SP = cpu.X + 0x0100
	
	def jmp = (cpu: CPU) => cpu.PC = cpu.nextWord
	
	def todo( cpu: CPU, addr: Int ) = sys.error( "unimplemented instruction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}
