package xyz.hyperreal.mos6502


abstract class CPU( val mem: Memory ) extends LogicalAddressModes {
	
	val opcodes: Seq[Instruction]
	
	var A = 0
	var X = 0
	var Y = 0
	var SP = 0
	var PC = 0
	var status = 0
	
	var trace = false
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
	
	def run {
		if (trace)
			println( hexWord(PC) + ' ' + hexByte(mem.readByte(PC)) )
			
		val cont = step
		
		if (trace)
			println( "A = " + hexByte(A) )
			
		if (cont)
			run
	}
	
	def reset {
		PC = mem.readWord( 0xFFFC )
		run
	}
	
}

class CPU6502( mem: Memory ) extends CPU( mem ) {
	
	val opcodes = CPU.table6502
	
}

object CPU {
	
	def populate( table: Array[Instruction], instructions: Seq[AddressModeInstruction], cc: Int, exceptions: Int* ) {
		for (aaa <- 0 to 7; bbb <- 0 to 7) {
			val opcode = aaa<<5 | bbb<<2 | cc
			
			if (!exceptions.contains( opcode ))
				table(opcode) = instructions(aaa)
		}
	}
	
	val table6502 = {
		val opcodes = Array.fill[Instruction]( 256 )( IllegalInstruction )
		
		opcodes(0) = StopInstruction
		populate( opcodes, Seq(ORA, TODO, TODO, TODO, STA, LDA, TODO, TODO), 1, 0x89 )
		opcodes.toVector
	}
	
}