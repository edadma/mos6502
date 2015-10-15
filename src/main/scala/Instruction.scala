package xyz.hyperreal.mos6502


abstract class Instruction {
	
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

object StopInstruction extends Instruction {
	
	def perform( cpu: CPU ) = false
	
}

abstract class AddressModeInstruction( modes: Seq[AddressMode] ) extends Instruction {
	
	def address( cpu: CPU ) = modes( cpu.bbb )( cpu )
	
}

object Instruction {
	val cc01 = Vector( IndirectXAddressMode, ZeroPageAddressMode, ImmediateAddressMode, AbsoluteAddressMode, IndirectYAddressMode,
											ZeroPageIndexedXAddressMode, AbsoluteIndexedYAddressMode, AbsoluteIndexedXAddressMode )
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
