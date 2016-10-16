package xyz.hyperreal.mos6502


object Instructions extends Flags {

	//
	// cc = 01
	//
	def adc( cpu: CPU, addr: Int ) {
		val src = cpu.readByte( addr )
		val res = cpu.A + src + cpu.read( C )
		
		if (cpu.status( D )) {
			val res1 =
				if (((cpu.A&0x0f) + (src & 0x0f) + cpu.read( C )) > 9)
					res + 6
				else
					res
			
			cpu.set( Z, res )
			cpu.set( N, res1 )
			cpu.set( V, (cpu.A^res1)&(src^res1)&0x80 )
			
			val res2 =
				if (res1 > 0x99)
					res1 + 96
				else
					res1
			
			cpu.set( C, res2 > 0x99 )
			cpu.A = res2
		} else {
			cpu.set( C, res > 0xff )
			cpu.set( V, (cpu.A^res)&(src^res)&0x80 )
			cpu.loadA( res )
		}
	}
	
	def and( cpu: CPU, addr: Int ) = cpu.loadA( cpu.readByte(addr) )
	
	def cmp( cpu: CPU, addr: Int ) = cpu.set( C, cpu.flags(cpu.A - cpu.readByte(addr)) >= 0 )
	
	def eor( cpu: CPU, addr: Int ) = cpu.loadA( cpu.readByte(addr) )

	def lda( cpu: CPU, addr: Int ) = cpu.loadA( cpu.readByte(addr) )

	def ora( cpu: CPU, addr: Int ) = cpu.loadA( cpu.readByte(addr) )
	
	def sbc( cpu: CPU, addr: Int ) {
		val src = cpu.readByte( addr )
		val temp = cpu.flags( cpu.A - src - cpu.read(C) )
		
		cpu.set( V, ((cpu.A^temp)&0x80) != 0 && ((cpu.A^src)&0x80) != 0 )
		
		if (cpu.status( D )) {
			val temp1 =
				if (((cpu.A&0x0f) - cpu.read( C )) < (src&0x0f))
					temp - 6
				else
					temp
					
			val temp2 =
				if (temp1 > 0x99)
					temp1 - 0x60
				else
					temp1
					
			cpu.set( C, temp2 < 0x100 )
			cpu.A = temp2&0xff
		} else {
			cpu.set( C, temp < 0x100 )
			cpu.A = temp&0xff
		}
	}

	def sta( cpu: CPU, addr: Int ) = cpu.writeByte( addr, cpu.A )
	
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
	
	def ror( cpu: CPU, addr: Int ) = {
		val src = cpu.readByte( addr )
		val carry = (src&0x01) != 0
		
		cpu.set( C, carry )
		cpu.writeByte( addr, cpu.flags((src >> 1)|(if (carry) 0x80 else 0)) )
	}
	
	//
	// cc = 10 (X)
	//
	def stx( cpu: CPU, addr: Int ) = cpu.writeByte( addr, cpu.X )
	
	def ldx( cpu: CPU, addr: Int ) = cpu.X = cpu.flags( cpu.readByte(addr) )
	
	//
	// cc = 00
	//
	def bit( cpu: CPU, addr: Int ) = {
		val src = cpu.readByte( addr )
		
		cpu.set( N, src&0x80 )
		cpu.set( V, src&0x40 )
		cpu.set( Z, cpu.A&src )
	}
	
	def cpx( cpu: CPU, addr: Int ) = cpu.set( C, cpu.flags(cpu.X - cpu.readByte(addr)) >= 0 )
	
	def cpy( cpu: CPU, addr: Int ) = cpu.set( C, cpu.flags(cpu.Y - cpu.readByte(addr)) >= 0 )
	
	def ldy( cpu: CPU, addr: Int ) = cpu.Y = cpu.flags( cpu.readByte(addr) )
	
	def sty( cpu: CPU, addr: Int ) = cpu.writeByte( addr, cpu.Y )
	
	//
	// simple
	//	
	val clc = (cpu: CPU) => cpu.clear( C )
	
	val cld = (cpu: CPU) => cpu.clear( D )
	
	val cli = (cpu: CPU) => cpu.clear( I )
	
	val clv = (cpu: CPU) => cpu.clear( V )
	
	val dex = (cpu: CPU) => cpu.X = cpu.flags( cpu.X - 1 )
	
	val dey = (cpu: CPU) => cpu.Y = cpu.flags( cpu.Y - 1 )
	
	val inx = (cpu: CPU) => cpu.X = cpu.flags( cpu.X + 1 )
	
	val iny = (cpu: CPU) => cpu.Y = cpu.flags( cpu.Y + 1 )
	
	val jmp = (cpu: CPU) => cpu.PC = cpu.nextWord
	
	val jmpind = (cpu: CPU) => cpu.PC = cpu.mem.readWord( cpu.nextWord )
	
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
	
	val rti = (cpu: CPU) => {
		cpu.S = cpu.pull
		cpu.PC = cpu.pull + (cpu.pull<<8)
	}
	
	val rts = (cpu: CPU) => cpu.PC = (cpu.pull + (cpu.pull<<8)) + 1
	
	val sec = (cpu: CPU) => cpu.set( C )
	
	val sed = (cpu: CPU) => cpu.set( D )
	
	val sei = (cpu: CPU) => cpu.set( I )
	
	val tax = (cpu: CPU) => cpu.X = cpu.flags( cpu.A )
	
	val tay = (cpu: CPU) => cpu.Y = cpu.flags( cpu.A )
	
	val tsx = (cpu: CPU) => cpu.X = cpu.flags( cpu.SP )
	
	val txs = (cpu: CPU) => cpu.SP = cpu.X + 0x100
	
	val txa = (cpu: CPU) => cpu.loadA( cpu.X )
	
	val tya = (cpu: CPU) => cpu.loadA( cpu.Y )
	
	def todo( cpu: CPU, addr: Int ) = sys.error( "unimplemented insrtuction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}
