package xyz.hyperreal.mos6502


abstract class CPU( val mem: Memory ) extends LogicalAddressModes {
	
	val opcodes: Seq[Instruction]
	
	var A = 0
	var X = 0
	var Y = 0
	var SP = 0
	var PC = 0
	var status = 0
	
	var opcode = 0
	var aaa = 0
	var bbb = 0
	var cc = 0
	
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
	
	def read( addr: Int ) =
		addr match {
			case ACCUMULATOR => A
			case _ => mem.readByte( addr )
		}
		
	def write( addr: Int, v: Int ) =
		addr match {
			case ACCUMULATOR => A = v&0xFF
			case _ => mem.writeByte( addr, v )
		}
	
	def step = {
		opcode = nextByte
		aaa = opcode>>5
		bbb = (opcode>>2)&0x03
		cc = opcode&0x03
		opcodes( opcode ) perform this
	}
	
	def run = while (step) {}
	
}

class CPU6502( mem: Memory ) extends CPU( mem ) {
	
	val opcodes = CPU.table6502
	
}

object CPU {
	
	val table6502 = {
		val opcodes = Array.fill( 256 )( IllegalInstruction )
		
		
		opcodes.toVector
	}
	
}