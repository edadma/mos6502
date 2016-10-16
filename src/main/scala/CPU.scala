package xyz.hyperreal.mos6502


abstract class CPU( val mem: Memory ) extends LogicalAddressModes with VectorsAddresses with Flags {
	
	val opcodes: Seq[Instruction]
	
	var A = 0
	var X = 0
	var Y = 0
	var SP = 0
	var PC = 0
	
	var S = 0
	
	var trace = false
	var opcode = 0
	
	def status( flag: Int ) = (S&flag) != 0
	
	def set( flag: Int ) {S |= flag}
	
	def set( flag: Int, v: Boolean ) {
		if (v)
			set( flag )
		else
			clear( flag )
	}
	
	def set( flag: Int, v: Int ) {set( flag, v != 0 )}

	def clear( flag: Int ) = S &= (flag^0xFF)
	
	def read( flag: Int ) = if (status( flag )) 1 else 0
	
	def nextByte = {
		val res = mem.readByte(PC)
		
		PC += 1
		res
	}
	
	def nextWord = {
		val res = mem.readWord(PC)
		
		PC += 2
		res
	}
	
	def readByte( addr: Int ) =
		addr match {
			case ACCUMULATOR => A
			case _ => mem.readByte( addr )
		}
		
	def readWord( addr: Int ) = mem.readWord( addr )
		
	def writeByte( addr: Int, v: Int ) =
		addr match {
			case ACCUMULATOR => loadA( v )
			case _ => mem.writeByte( addr, v )
		}
	
	def loadA( v: Int ) = A = flags( v )
	
	def flags( a: Int ) = {
		val b = a&0xFF
		
		set( N, b&0x80 )
		set( Z, b == 0 )
		b
	}
	
	def push( a: Int ) {
		mem.writeByte( SP, a )
		SP -= 1
	}
	
	def pull = {
		SP += 1
		mem.readByte( SP )
	}
	
	def step = {
		
		if (trace)
			println( hexWord(PC) + ' ' + hexByte(mem.readByte(PC)) )
			
		opcode = nextByte
		
		val cont = opcodes(opcode) perform this
		
		if (trace)
			printf( "A:%s X:%s Y:%s SP:%s PC:%s N:%d V:%d B:%d D:%d I:%d Z:%d C:%d\n\n", hexByte(A), hexByte(X), hexByte(Y), hexWord(SP), hexWord(PC),
							read(N), read(V), read(B), read(D), read(I), read(Z), read(C) )
			
		cont
	}
	
	def run {
		if (step)
			run
	}
	
	def reset {
		PC = mem.readWord( RESET_VECTOR )
		run
	}
	
}

trait Flags {
	val C = 0x00
	val Z = 0x02
	val I = 0x04
	val D = 0x08
	val B = 0x10
	val V = 0x40
	val N = 0x80
}

class CPU6502( mem: Memory ) extends CPU( mem ) {
	
	val opcodes = CPU.table6502
	
}

object CPU {
	
	import Instructions._
	import AddressModes._
	
	def populate( table: Array[Instruction], instructions: Seq[(CPU, Int) => Unit], modes: Seq[CPU => Int], cc: Int, exceptions: Int* ) {
		for (aaa <- 0 to 7; bbb <- 0 to 7) {
			val opcode = aaa<<5 | bbb<<2 | cc
			val inst = instructions(aaa)
			val mode = modes(bbb)
			
			if (!exceptions.contains( opcode ) && inst != null && mode != null) {
				if (table(opcode) ne IllegalInstruction)
					sys.error( "opcode already populated: " + opcode.toHexString )
					
				table(opcode) = new AddressModeInstruction( inst, mode )
			}
		}
	}
	
	val table6502 = {
		val opcodes = Array.fill[Instruction]( 256 )( IllegalInstruction )
		
		opcodes(0) = BRK
		
		List(
			0x18 -> clc,
			0xD8 -> cld,
			0x58 -> cli,
			0xB8 -> clv,
			0xCA -> dex,
			0x88 -> dey,
			0xE8 -> inx,
			0xC8 -> iny,
			0x4C -> jmp,
			0x6C -> jmpind,
			0x20 -> jsr,
			0x48 -> pha,
			0x08 -> php,
			0x68 -> pla,
			0x28 -> plp,
			0x40 -> rti,
			0x60 -> rts,
			0x38 -> sec,
			0xF8 -> sed,
			0x78 -> sei,
			0xAA -> tax,
			0xA8 -> tay,
			0xBA -> tsx,
			0x9A -> txs,
			0x8A -> txa,
			0x98 -> tya,
			0xEA -> ((_: CPU) => ())
			) foreach {case (opcode, computation) => opcodes(opcode) = new SimpleInstruction( computation )}
		populate( opcodes, Seq(ora, and, eor, adc, sta, lda, cmp, sbc),
							Seq(indirectX, zeroPage, immediate, absolute, indirectY, zeroPageIndexedX, absoluteIndexedY, absoluteIndexedX), 1, 0x89 )
		populate( opcodes, Seq(asl, rol, lsr, ror, null, null, dec, inc),
							Seq(null, zeroPage, accumulator, absolute, null, zeroPageIndexedX, null, absoluteIndexedX), 2, 0xCA, 0xEA )
		populate( opcodes, Seq(null, null, null, null, stx, ldx, null, null),
							Seq(immediate, zeroPage, null, absolute, null, zeroPageIndexedY, null, absoluteIndexedY), 2, 0x82, 0x9E )
		populate( opcodes, Seq(null, bit, null, null, sty, ldy, cpx, cpy),
							Seq(immediate, zeroPage, null, absolute, null, zeroPageIndexedX, null, absoluteIndexedX), 0, 0x20, 0x24, 0x2C, 0x80, 0x9C, 0xD4, 0xDC, 0xF4, 0xFC )
		
		for (xx <- 0 to 3; y <- 0 to 1)
			opcodes(xx<<6 | y<<5 | 0x10) = new BranchInstruction( xx, if (y == 0) false else true )
			
		opcodes.toVector
	}
	
}