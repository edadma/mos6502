package xyz.hyperreal.mos6502


trait LogicalAddressModes {
	val ACCUMULATOR = -1
}

object AddressModes extends LogicalAddressModes {
	
	def accumulator( cpu: CPU ) = ACCUMULATOR
	
	def absolute( cpu: CPU ) = cpu.nextWord
	
	def immediate( cpu: CPU ) = {
		val res = cpu.PC
		
		cpu.PC += 1
		res
	}
	
	def zeroPage( cpu: CPU ) = cpu.nextByte
	
	def absoluteIndexedX( cpu: CPU ) = cpu.nextWord + cpu.X
	
	def absoluteIndexedY( cpu: CPU ) = cpu.nextWord + cpu.Y
	
	def zeroPageIndexedX( cpu: CPU ) = cpu.nextByte + cpu.X
	
	def zeroPageIndexedY( cpu: CPU ) = cpu.nextByte + cpu.Y
	
	def indirectX( cpu: CPU ) = cpu.readWord( cpu.nextByte + cpu.X )
	
	def indirectY( cpu: CPU ) = cpu.readWord( cpu.nextByte ) + cpu.Y
	
}