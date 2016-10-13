package xyz.hyperreal.mos6502


object Instructions extends Flags {

	//
	// cc = 01
	//
	def ora( cpu: CPU, addr: Int ) = cpu.loadA( cpu.readByte(addr) )
	
	def and( cpu: CPU, addr: Int ) = cpu.loadA( cpu.readByte(addr) )
	
	def eor( cpu: CPU, addr: Int ) = cpu.loadA( cpu.readByte(addr) )
	
	def adc( cpu: CPU, addr: Int ) =
		if (cpu.status(D)) {
			todo( cpu, addr )
		} else {
			val res = cpu.A + cpu.readByte( addr ) + cpu.read(C)
			
			cpu.set( C, res > 255 )
			
			cpu.set( V, (res&0x80)^(cpu.A&0x80) )
			cpu.set( N, res&0x80 )
			cpu.loadA( res&0xFF )
		}
	
	def sta( cpu: CPU, addr: Int ) = cpu.writeByte( addr, cpu.A )
	
	def lda( cpu: CPU, addr: Int ) = cpu.loadA( cpu.readByte(addr) )
	
	def cmp( cpu: CPU, addr: Int ) = {
		val diff = cpu.A - cpu.readByte( addr )
		
		cpu.flags( diff )
		cpu.set( C, diff >= 0 )
	}
	
	//
	// cc = 10
	//
	def asl( cpu: CPU, addr: Int ) = {
		val res = cpu.readByte( addr ) << 1
		
		cpu.set( C, res&0x100 )
		cpu.loadA( res )
	}
	
	def inc( cpu: CPU, addr: Int ) = cpu.writeByte( addr, cpu.flags(cpu.readByte(addr) + 1) )
	
	//
	// cc = 10 (X)
	//
	def ldx( cpu: CPU, addr: Int ) = cpu.X = cpu.flags( cpu.readByte(addr) )
	
	//
	// cc = 00
	//
	def ldy( cpu: CPU, addr: Int ) = cpu.Y = cpu.flags( cpu.readByte(addr) )
	
	//
	// simple
	//	
	def clc = (cpu: CPU) => cpu.clear( C )
	
	def cli = (cpu: CPU) => cpu.clear( I )
	
	def iny = (cpu: CPU) => cpu.Y = cpu.flags( cpu.Y + 1 )
	
	def jmp = (cpu: CPU) => cpu.PC = cpu.nextWord
	
	def pha = (cpu: CPU) => cpu.push( cpu.A )
	
	def php = (cpu: CPU) => cpu.push( cpu.S )
	
	def pla = (cpu: CPU) => cpu.loadA( cpu.pull )
	
	def plp = (cpu: CPU) => cpu.S = cpu.pull
	
	def sec = (cpu: CPU) => cpu.set( C )
	
	def sei = (cpu: CPU) => cpu.set( I )
	
	def txs = (cpu: CPU) => cpu.SP = cpu.X + 0x100
	
	def todo( cpu: CPU, addr: Int ) = sys.error( "unimplemented instruction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}
