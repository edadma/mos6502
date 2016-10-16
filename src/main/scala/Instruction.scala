package xyz.hyperreal.mos6502


abstract class Instruction extends VectorsAddresses with Flags {
	
	def perform( cpu: CPU ): Boolean
	
}

object IllegalInstruction extends Instruction {
	
	def perform( cpu: CPU ) = sys.error( "illegal instruction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}

object BRK extends Instruction {
	
	def perform( cpu: CPU ) = {
		val addr = cpu.readWord( BRK_VECTOR )
		
		if (addr > 0) {
			cpu.PC = addr
			// to be completed
			true
		} else
			false
	}
	
}

class SimpleInstruction( computation: CPU => Unit ) extends Instruction {
	
	def perform( cpu: CPU ) = {
		computation( cpu )
		true
	}
		
}

class BranchInstruction( xx: Int, y: Boolean ) extends Instruction {
	
	def perform( cpu: CPU ) = {
		val offset = cpu.nextByte.toByte
		
		if (cpu.status(xx) == y)
			cpu.PC += offset
			
		true
	}
	
}

class AddressModeInstruction( computation: (CPU, Int) => Unit, mode: CPU => Int ) extends Instruction {
	
	def perform( cpu: CPU ) = {
		computation( cpu, mode(cpu) )
		true
	}
	
}