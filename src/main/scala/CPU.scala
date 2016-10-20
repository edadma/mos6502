package xyz.hyperreal.mos6502

import collection.mutable.HashMap


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
		if (SP < 0)
			sys.error( "stack overflow" )
			
		mem.writeByte( SP, a )
		SP -= 1
	}
	
	def pull = {
		if (SP == 0x1FF)
			sys.error( "stack underflow" )
			
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
	
	def run = while (step) {}
	
	def reset {
		A = 0
		X = 0
		Y = 0
		SP = 0x1FD
		PC = mem.readWord( RESET_VECTOR )
		S = 0
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
	
	val opcodes = CPU.opcodes6502
}

object CPU {
	
	import Instructions._
	import AddressModes._
	
	def populate( opcodes: Array[Instruction], asmmap: HashMap[(String, Symbol), Byte], dismap: HashMap[Int, (String, Symbol)], instructions: Seq[((CPU, Int) => Unit, String)], modes: Seq[(CPU => Int, Symbol)], cc: Int, exceptions: Int* ) {
		for (aaa <- 0 to 7; bbb <- 0 to 7) {
			val opcode = aaa<<5 | bbb<<2 | cc
			
			if (!exceptions.contains( opcode )) {
				val inst = instructions(aaa)
				
				if (inst ne null) {
					val mode = modes(bbb)
					
					if (mode ne null) {
				
						if (opcodes(opcode) ne IllegalInstruction)
							sys.error( "opcode already populated: " + opcode.toHexString )
							
						opcodes(opcode) = new AddressModeInstruction( inst._1, mode._1 )
						asmmap((inst._2, mode._2)) = opcode.toByte
						dismap(opcode) = (inst._2, mode._2)
					}
				}
			}
		}
	}
	
	val (opcodes6502, asm6502, dis6502) = {
		val opcodes = Array.fill[Instruction]( 256 )( IllegalInstruction )
		val asmmap = new HashMap[(String, Symbol), Byte]
		val dismap = new HashMap[Int, (String, Symbol)]
		
		opcodes(0) = BRK
		asmmap(("brk", 'implicit)) = 0
		dismap(0) = ("brk", 'implicit)
		
		List(
			(0x18, clc, "clc"),
			(0xD8, cld, "cld"),
			(0x58, cli, "cli"),
			(0xB8, clv, "clv"),
			(0xCA, dex, "dex"),
			(0x88, dey, "dey"),
			(0xE8, inx, "inx"),
			(0xC8, iny, "iny"),
			(0x48, pha, "pha"),
			(0x08, php, "php"),
			(0x68, pla, "pla"),
			(0x28, plp, "plp"),
			(0x40, rti, "rti"),
			(0x60, rts, "rts"),
			(0x38, sec, "sec"),
			(0xF8, sed, "sed"),
			(0x78, sei, "sei"),
			(0xAA, tax, "tax"),
			(0xA8, tay, "tay"),
			(0xBA, tsx, "tsx"),
			(0x9A, txs, "txs"),
			(0x8A, txa, "txa"),
			(0x98, tya, "tya"),
			(0xEA, nop, "nop")
			) foreach {
				case (opcode, computation, mnemonic) =>
					opcodes(opcode) = new SimpleInstruction( computation )
					asmmap((mnemonic, 'implicit)) = opcode.toByte
					dismap(opcode) = (mnemonic, 'implicit)
			}
			
		List(
			(0x4C, jmp, "jmp", 'direct),
			(0x6C, jmpind, "jmp", 'indirect),
			(0x20, jsr, "jsr", 'direct)
			) foreach {
				case (opcode, computation, mnemonic, mode) =>
					opcodes(opcode) = new SimpleInstruction( computation )
					asmmap((mnemonic, mode)) = opcode.toByte
					dismap(opcode) = (mnemonic, mode)
			}
		
		populate( opcodes, asmmap, dismap, IndexedSeq((ora, "ora"), (and, "and"), (eor, "eor"), (adc, "adc"), (sta, "sta"), (lda, "lda"), (cmp, "cmp"), (sbc, "sbc")),
							IndexedSeq((indirectX, 'indirectX), (zeroPage, 'zeroPage), (immediate, 'immediate), (absolute, 'direct), (indirectY, 'indirectY), (zeroPageIndexedX, 'zeroPageIndexedX), (absoluteIndexedY, 'directY), (absoluteIndexedX, 'directX)), 1, 0x89 )
		populate( opcodes, asmmap, dismap, IndexedSeq((asl, "asl"), (rol, "rol"), (lsr, "lsr"), (ror, "ror"), null, null, (dec, "dec"), (inc, "inc")),
							IndexedSeq(null, (zeroPage, 'zeroPage), (accumulator, 'accumulator), (absolute, 'direct), null, (zeroPageIndexedX, 'zeroPageIndexedX), null, (absoluteIndexedX, 'directX)), 2, 0xCA, 0xEA )
		populate( opcodes, asmmap, dismap, IndexedSeq(null, null, null, null, (stx, "stx"), (ldx, "ldx"), null, null),
							IndexedSeq((immediate, 'immediate), (zeroPage, 'zeroPage), null, (absolute, 'direct), null, (zeroPageIndexedY, 'zeroPageIndexedY), null, (absoluteIndexedY, 'directY)), 2, 0x82, 0x9E )
		populate( opcodes, asmmap, dismap, IndexedSeq(null, (bit, "bit"), null, null, (sty, "sty"), (ldy, "ldy"), (cpx, "cpx"), (cpy, "cpy")),
							IndexedSeq((immediate, 'immediate), (zeroPage, 'zeroPage), null, (absolute, 'direct), null, (zeroPageIndexedX, 'zeroPageIndexedX), null, (absoluteIndexedX, 'directX)), 0, 0x20, 0x24, 0x2C, 0x80, 0x9C, 0xD4, 0xDC, 0xF4, 0xFC )
		
		val branches = Vector( "bpl", "bmi", "bvc", "bvs", "bcc", "bcs", "bne", "beq" )
		
		for (xx <- 0 to 3; y <- 0 to 1) {
			val opcode = xx<<6 | y<<5 | 0x10
			
			opcodes(opcode) = new BranchInstruction( xx, if (y == 0) false else true )
			asmmap((branches(xx<<1 | y), 'direct)) = opcode.toByte
			dismap(opcode) = (branches(xx<<1 | y), 'direct)
		}
		
		(opcodes.toVector, asmmap.toMap, dismap.toMap)
	}
	
}