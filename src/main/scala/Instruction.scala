package xyz.hyperreal.mos6502


abstract class Instruction extends VectorsAddresses with Flags {
	
	def perform( cpu: CPU ): Boolean
	
}

object IllegalInstruction extends Instruction {
	
	def perform( cpu: CPU ) = sys.error( "illegal instruction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}

object BRK extends Instruction {
	
	def perform( cpu: CPU ) = {
		if (cpu.mem.addressable( BRK_VECTOR )) {
			cpu.PC += 1	// BRK is really a two byte instruction, operand byte is not used
			cpu.push( cpu.PC >> 8 )
			cpu.push( cpu.PC )
			cpu.set( B )
			cpu.push( cpu.S )
			cpu.set( I )
			cpu.PC = cpu.readWord( BRK_VECTOR )
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
	
	val flag =
		xx match {
			case 0 => N
			case 1 => V
			case 2 => C
			case 3 => Z
		}
		
	def perform( cpu: CPU ) = {
		val offset = cpu.nextByte.toByte
		
		if (cpu.status( flag ) == y)
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