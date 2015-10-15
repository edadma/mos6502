package xyz.hyperreal.mos6502


abstract class Instruction {
	
	val N = 0x80
	val V = 0x40
	val B = 0x01
	val D = 0x08
	val I = 0x04
	val Z = 0x02
	val C = 0x01
	
	def perform( cpu: CPU ): Boolean
	
}

object IllegalInstruction extends Instruction {
	
	def perform( cpu: CPU ) = sys.error( "illegal instruction: " + hexByte(cpu.opcode) + " at " + hexWord(cpu.PC - 1) )
	
}

abstract class InstructionAddress extends (CPU => Int)

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

abstract class AddressModeInstruction( protected val addr: InstructionAddress ) extends Instruction

object cc01 extends InstructionAddress {

	val modes = Vector( IndirectXAddressMode, ZeroPageAddressMode, ImmediateAddressMode, AbsoluteAddressMode, IndirectYAddressMode,
											ZeroPageIndexedXAddressMode, AbsoluteIndexedYAddressMode, AbsoluteIndexedXAddressMode )
	
	def apply( cpu: CPU ) = modes( cpu.bbb ).apply( cpu )
	
}

object LDA extends AddressModeInstruction( cc01 ) {
	
	def perform( cpu: CPU ) = {
		cpu.A = cpu.read( addr(cpu) )
		true
	}
	
}