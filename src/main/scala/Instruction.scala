package xyz.hyperreal.mos6502


abstract class Instruction extends Vectors {
	
	def perform( cpu: CPU ): Boolean
	
}

object IllegalInstruction extends Instruction {
	
	def perform( cpu: CPU ) = sys.error( "illegal instruction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}

// abstract class Address {
// 	
// 	def readByte: Int
// 	
// 	def writeByte( v: Int )
// 	
// }
// 
// class MemoryAddress( mem: Memory, addr: Int ) extends Address {
// 	
// 	def readByte = mem.readByte( addr )
// 	
// 	def writeByte( v: Int ) = mem.writeByte( addr, v )
// 	
// }
// 
// class AccumulatorAddress( cpu: CPU ) extends Address {
// 	
// 	def readByte = cpu.A
// 	
// 	def writeByte( v: Int ) = cpu.A = v
// 	
// }

object BRK extends Instruction {
	
	def perform( cpu: CPU ) = {
		val addr = cpu.mem.readWord( BRK_VECTOR )
		
		if (addr > 0) {
			cpu.PC = addr
			// to be completed
			true
		} else
			false
	}
	
}

object TXS extends Instruction {
	
	def perform( cpu: CPU ) = {
		cpu.SP = cpu.X + 0x0100
		true
	}
	
}

object BXX extends Instruction {
	
	def perform( cpu: CPU ) = {
		val offset = cpu.nextByte
		
		if (cpu.status(cpu.xx) == cpu.y)
			cpu.PC += offset
			
		true
	}
	
}

abstract class AddressModeInstruction( modes: Seq[AddressMode] ) extends Instruction {
	
	def address( cpu: CPU ) = modes( cpu.bbb )( cpu )
	
}

object Instruction {
	val cc01 = Vector( IndirectXAddressMode, ZeroPageAddressMode, ImmediateAddressMode, AbsoluteAddressMode, IndirectYAddressMode,
											ZeroPageIndexedXAddressMode, AbsoluteIndexedYAddressMode, AbsoluteIndexedXAddressMode )
	val cc10 = Vector( ImmediateAddressMode, ZeroPageAddressMode, AccumulatorAddressMode, AbsoluteAddressMode, IllegalAddressMode,
											ZeroPageIndexedXAddressMode, IllegalAddressMode, AbsoluteIndexedXAddressMode )
	val cc10x = Vector( ImmediateAddressMode, ZeroPageAddressMode, AccumulatorAddressMode, AbsoluteAddressMode, IllegalAddressMode,
											ZeroPageIndexedYAddressMode, IllegalAddressMode, AbsoluteIndexedYAddressMode )
}

object LDA extends AddressModeInstruction( Instruction.cc01 ) {
	
	def perform( cpu: CPU ) = {
		cpu.A = cpu.flags( cpu.read(address(cpu)) )
		true
	}
	
}

object STA extends AddressModeInstruction( Instruction.cc01 ) {
	
	def perform( cpu: CPU ) = {
		cpu.write( address(cpu), cpu.A )
		true
	}
	
}

object ORA extends AddressModeInstruction( Instruction.cc01 ) {
	
	def perform( cpu: CPU ) = {
		cpu.A |= cpu.read( address(cpu) )
		cpu.flags( cpu.A )
		true
	}
	
}

object TODO extends AddressModeInstruction( Instruction.cc01 ) {
	
	def perform( cpu: CPU ) = {
		true
	}
	
}

object INC extends AddressModeInstruction( Instruction.cc10 ) {
	
	def perform( cpu: CPU ) = {
		val addr = address(cpu)
		
		cpu.write( addr, cpu.flags(cpu.read(addr) + 1) )
		true
	}
	
}

object LDX extends AddressModeInstruction( Instruction.cc10x ) {
	
	def perform( cpu: CPU ) = {
		cpu.X = cpu.flags( cpu.read(address(cpu)) )
		true
	}
	
}
