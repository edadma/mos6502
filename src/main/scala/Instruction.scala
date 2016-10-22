package xyz.hyperreal.mos6502


abstract class Instruction extends (CPU => Unit) with Flags

object IllegalInstruction extends Instruction {
	
	def apply( cpu: CPU ) = sys.error( "illegal instruction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}

class SimpleInstruction( computation: CPU => Unit ) extends Instruction {
	
	def apply( cpu: CPU ) = {
		computation( cpu )
	}
	
}

class BranchInstruction( xx: Int, y: Boolean ) extends Instruction {
	
	val flag =
		xx match {
			case 0 => N
			case 1 => V
			case 2 => C
			case 3 => Z
		}
		
	def apply( cpu: CPU ) {
		val offset = cpu.nextByte.toByte
		
		if (cpu.status( flag ) == y)
			cpu.PC += offset
	}
	
}

class AddressModeInstruction( compute: (CPU, Int) => Unit, mode: CPU => Int ) extends Instruction {
	
	def apply( cpu: CPU ) {
		compute( cpu, mode(cpu) )
	}
	
}