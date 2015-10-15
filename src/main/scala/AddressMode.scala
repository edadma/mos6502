package xyz.hyperreal.mos6502


trait LogicalAddressModes {
	val ACCUMULATOR = -1
}

abstract class AddressMode extends (CPU => Int) with LogicalAddressModes

object AccumulatorAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = ACCUMULATOR
	
}

object AbsoluteAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = cpu.nextWord
	
}

object ImmediateAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = {
		val res = cpu.PC
		
		cpu.PC += 1
		res
	}
	
}

object ZeroPageAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = cpu.nextByte
	
}

object AbsoluteIndexedXAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = cpu.nextWord + cpu.X
	
}

object AbsoluteIndexedYAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = cpu.nextWord + cpu.Y
	
}

object ZeroPageIndexedXAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = cpu.nextByte + cpu.X
	
}

object ZeroPageIndexedYAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = cpu.nextByte + cpu.Y
	
}

object IndirectXAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = cpu.mem.readWord( cpu.nextByte + cpu.X )
	
}

object IndirectYAddressMode extends AddressMode {
	
	def apply( cpu: CPU ) = cpu.mem.readWord( cpu.nextByte ) + cpu.Y
	
}

