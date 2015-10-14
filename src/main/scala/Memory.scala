package xyz.hyperreal.mos6502

import collection.mutable.{ArrayBuffer}


trait Addressable {
	
	def start: Int
	
	def size: Int
	
	def readByte( addr: Int ): Int
	
	def writeByte( addr: Int, value: Int )
	
	def readWord( addr: Int ) = {
		readByte( addr ) + readByte( addr + 1 )<<8
	}
	
	def writeWord( addr: Int, value: Int ) {
		writeByte( addr, value&0xFF )
		writeByte( addr, value>>8 )
	}
	
}

class Storage( val start: Int, val size: Int ) extends Addressable {
	
	require( start >= 0 )
	require( size > 0 )
	
	private val mem = new Array[Byte]( size )
	
	def readByte( addr: Int ) = mem( addr - start )
	
	def writeByte( addr: Int, value: Int ) = mem( addr - start ) = value.toByte
	
}

abstract class Port( val start: Int, val size: Int ) extends Addressable {
	
	require( start >= 0 )
	require( size > 0 )

}

abstract class ReadOnlyPort( val start: Int, val size: Int ) extends Addressable {
	
	require( start >= 0 )
	require( size > 0 )
	
	def writeByte( addr: Int, value: Int ) = sys.error( "read only port" )
	
}

abstract class WriteOnlyPort( val start: Int, val size: Int ) extends Addressable {
	
	require( start >= 0 )
	require( size > 0 )
	
	def readByte( addr: Int ) = sys.error( "write only port" )
	
}

class Memory extends Addressable {
	
	private val regions = new ArrayBuffer[Addressable]
	private var first = 0
	private var end = 0
	
	private def find( addr: Int ) =
		regions.indexWhere( r => r.start <= addr && r.start + r.size > addr ) match {
			case -1 => sys.error( addr.toHexString + " is not an addressable memory location" )
			case ind => regions( ind )
		}
		
	def start = first
	
	def size = end - first
	
	def readByte( addr: Int ) = find( addr ).readByte( addr )
	
	def writeByte( addr: Int, value: Int ) = find( addr ).writeByte( addr, value )
	
	def add( region: Addressable ) {
		regions.indexWhere( r => r.start >= region.start + region.size ) match {
			case -1 =>
				regions += region
				end = region.size + region.start
				
				if (regions isEmpty)
					first = region.start
			case ind =>
				regions.insert( ind, region )
				
				if (ind == 0)
					first = region.start
		}
	}
	
}