package xyz.hyperreal.mos6502


abstract class CPU( val mem: Memory ) extends LogicalAddressModes with Vectors with Flags {
	
	val opcodes: Seq[Instruction]
	
	var A = 0
	var X = 0
	var Y = 0
	var SP = 0
	var PC = 0
	
	val status = Array( false, false, false, false )
	
	var B = false
	var D = false
	var I = false
	
	var trace = false
	var opcode = 0
	var aaa = 0
	var bbb = 0
	var cc = 0
	var xx = 0
	var y = false
	
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
//		aaa = opcode>>5
		bbb = (opcode>>2)&0x03
//		cc = opcode&0x03
		xx = opcode>>6
		y = ((opcode>>5)&1) == 1
		opcodes(opcode) perform this
	}
	
	def flags( a: Int ) = {
		status(N) = a < 0
		status(Z) = a == 0
		a
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
		PC = mem.readWord( RESET_VECTOR )
		run
	}
	
}

trait Flags {
	val N = 0
	val V = 1
	val C = 2
	val Z = 3
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
			
			if (!exceptions.contains( opcode ) && inst != null && mode != null)
				table(opcode) = new AddressModeInstruction( inst, mode )
		}
	}
	
	val table6502 = {
		val opcodes = Array.fill[Instruction]( 256 )( IllegalInstruction )
		
		opcodes(0) = BRK
		
		List(
			0x4C -> jmp,
			0x9A -> txs
			) foreach {case (opcode, computation) => opcodes(opcode) = new SimpleInstruction( computation )}		
		populate( opcodes, Seq(ora, todo, todo, todo, sta, lda, cmp, todo),
							Seq(indirectX, zeroPage, immediate, absolute, indirectY, zeroPageIndexedX, absoluteIndexedY, absoluteIndexedX), 1, 0x89 )
		populate( opcodes, Seq(todo, todo, todo, todo, null, null, todo, inc),
							Seq(null, zeroPage, accumulator, absolute, null, zeroPageIndexedX, null, absoluteIndexedX), 2, 0xCA, 0xEA )
		populate( opcodes, Seq(null, null, null, null, todo, todo, null, null),
							Seq(immediate, zeroPage, accumulator, absolute, null, zeroPageIndexedY, null, absoluteIndexedY), 2, 0x82, 0x9E )
		
		for (xx <- 0 to 3; y <- 0 to 1)
			opcodes(xx<<6 | y<<5 | 0x10) = new BranchInstruction( xx, if (y == 0) false else true )
			
		opcodes.toVector
	}
	
}
