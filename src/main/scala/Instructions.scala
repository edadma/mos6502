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
		cpu.writeByte( addr, cpu.flags(res) )
	}
	
	def dec( cpu: CPU, addr: Int ) = cpu.writeByte( addr, cpu.flags(cpu.readByte(addr) - 1) )
	
	def inc( cpu: CPU, addr: Int ) = cpu.writeByte( addr, cpu.flags(cpu.readByte(addr) + 1) )
	
	def lsr( cpu: CPU, addr: Int ) = {
		val src = cpu.readByte( addr )
		
		cpu.set( C, src&0x01 )
		cpu.writeByte( addr, cpu.flags(src >> 1) )
	}
	
	def rol( cpu: CPU, addr: Int ) = {
		val src = cpu.readByte( addr )
		val res = (src << 1) | (if ((src&0x80) != 0) 1 else 0)
		
		cpu.set( C, res > 0xFF )
		cpu.writeByte( addr, cpu.flags(res) )
	}
	
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
	val clc = (cpu: CPU) => cpu.clear( C )
	
	val cli = (cpu: CPU) => cpu.clear( I )
	
	val clv = (cpu: CPU) => cpu.clear( V )
	
	val dex = (cpu: CPU) => cpu.X = cpu.flags( cpu.X - 1 )
	
	val dey = (cpu: CPU) => cpu.Y = cpu.flags( cpu.Y - 1 )
	
	val inx = (cpu: CPU) => cpu.X = cpu.flags( cpu.X + 1 )
	
	val iny = (cpu: CPU) => cpu.Y = cpu.flags( cpu.Y + 1 )
	
	val jmp = (cpu: CPU) => cpu.PC = cpu.nextWord
	
	val jsr = (cpu: CPU) => {
		val pc = cpu.PC + 1
		
		cpu.push( pc >> 8 )
		cpu.push( pc )
		cpu.PC = cpu.nextWord
	}
	
	val pha = (cpu: CPU) => cpu.push( cpu.A )
	
	val php = (cpu: CPU) => cpu.push( cpu.S )
	
	val pla = (cpu: CPU) => cpu.loadA( cpu.pull )
	
	val plp = (cpu: CPU) => cpu.S = cpu.pull
	
	val rts = (cpu: CPU) => cpu.PC = ((cpu.pull<<8) + cpu.pull) + 1
	
	val sec = (cpu: CPU) => cpu.set( C )
	
	val sei = (cpu: CPU) => cpu.set( I )
	
	val txs = (cpu: CPU) => cpu.SP = cpu.X + 0x100
	
	val tya = (cpu: CPU) => cpu.loadA( cpu.Y )
	
	def todo( cpu: CPU, addr: Int ) = sys.error( "unimplemented instruction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}
