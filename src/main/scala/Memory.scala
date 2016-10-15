package xyz.hyperreal.mos6502

import collection.mutable.{ArrayBuffer}


trait VectorsAddresses {
	val RESET_VECTOR = 0xFFFC
	val BRK_VECTOR = 0xFFFE
}

trait Addressable {
	
	def start: Int
	
	def size: Int
	
	def readByte( addr: Int ): Int
	
	def writeByte( addr: Int, value: Int )
	
	def readWord( addr: Int ) = {
		readByte( addr ) + (readByte( addr + 1 )<<8)
	}
	
	def writeWord( addr: Int, value: Int ) {
		writeByte( addr, value&0xFF )
		writeByte( addr, value>>8 )
	}
	
}

class RAM( val start: Int, end: Int ) extends Addressable {
	
	require( start >= 0 )
	require( end >= start )
	
	val size = end - start + 1
		
	protected val mem = new Array[Byte]( size )
	
	def readByte( addr: Int ) = mem( addr - start )&0xFF
	
	def writeByte( addr: Int, value: Int ) = mem( addr - start ) = value.toByte
	
	override def toString = s"RAM: ${hexWord(start)}-${hexWord(end)}"
}

class ROM( val start: Int, mem: Seq[Byte] ) extends Addressable {
	
	require( start >= 0 )
	require( mem.size > 0 )
	
	val size = mem.size
	
	def readByte( addr: Int ) = mem( addr - start )&0xFF
	
	def writeByte( addr: Int, value: Int ) = sys.error( "read only memory" )
	
	override def toString = s"ROM: ${hexWord(start)}-${hexWord(start + size - 1)}"
	
}

object ROM {
	def apply( start: Int, mem: Int* ) = new ROM( start, mem map (_.toByte) )
}

abstract class Port extends Addressable {
	
	val start: Int
	val size: Int
	
	override def toString = s"port: ${hexWord(start)}-${hexWord(start + size - 1)}"

}

abstract class SingleAddressPort extends Port {
	
	val size = 1
	
	require( start >= 0 )
	require( size > 0 )
	
}

abstract class ReadOnlyPort extends SingleAddressPort {
	
	def writeByte( addr: Int, value: Int ) = sys.error( "read only port" )
	
}

abstract class WriteOnlyPort extends SingleAddressPort {
	
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
		regions find (r => r.start <= region.start && region.start < r.start + r.size) match {
			case Some(r) => sys.error( hexWord(region.start) + ", " + hexWord(region.size) + " overlaps " + hexWord(r.start) + ", " + hexWord(r.size) )
			case None =>
		}
		
		regions indexWhere (_.start >= region.start + region.size) match {
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
	
	override def toString = regions mkString "\n"
}