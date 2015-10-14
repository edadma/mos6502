package xyz.hyperreal.mos6502


class CPU( mem: Memory ) {
	
	val bitN = 0x80
	val bitV = 0x40
	val bitB = 0x01
	val bitD = 0x08
	val bitI = 0x04
	val bitZ = 0x02
	val bitC = 0x01
	
	var regA = 0
	var regX = 0
	var regY = 0
	var regSP = 0
	var regPC = 0
	var regStatus = 0
	
	
	
}