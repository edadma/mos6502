package xyz.hyperreal.mos6502

import collection.mutable.{ArrayBuffer}


trait VectorsAddresses {
	val RESET_VECTOR = 0xFFFC
	val BRK_VECTOR = 0xFFFE
}

trait Addressable {

	def name: String
	
	def start: Int
	
	def size: Int
	
	def readByte( addr: Int ): Int
	
	def writeByte( addr: Int, value: Int )
	
	def isRAM = isInstanceOf[RAM]
	
	def isROM = isInstanceOf[ROM]
	
	def isDevice = isInstanceOf[Device]
	
	def program( addr: Int, value: Int ) = writeByte( addr, value )
	
	def readWord( addr: Int ) = readByte( addr ) + (readByte( addr + 1 )<<8)
	
	def writeWord( addr: Int, value: Int ) {
		writeByte( addr, value&0xFF )
		writeByte( addr, value>>8 )
	}
	
}

class RAM( val name: String, val start: Int, end: Int ) extends Addressable {
	
	require( start >= 0 )
	require( end >= start )
	
	val size = end - start + 1
		
	protected val mem = new Array[Byte]( size )
	
	def clear =
		for (i <- 0 until size)
			mem(i) = 0
	
	def readByte( addr: Int ) = mem( addr - start )&0xFF
	
	def writeByte( addr: Int, value: Int ) = mem( addr - start ) = value.toByte
	
	override def toString = s"$name RAM: ${hexWord(start)}-${hexWord(end)}"
}

class ROM( val name: String, val start: Int, end: Int ) extends Addressable {
	
	require( start >= 0 )
	require( end >= start )
	
	val size = end - start + 1
		
	protected val mem = new Array[Byte]( size )
	
	def readByte( addr: Int ) = mem( addr - start )&0xFF
	
	def writeByte( addr: Int, value: Int ) = sys.error( "read only memory: " + (addr&0xffff).toHexString + " (tried to write " + (value&0xff).toHexString + ")" )
	
	override def program( addr: Int, value: Int ) = mem( addr - start ) = value.toByte
	
	override def toString = s"$name ROM: ${hexWord(start)}-${hexWord(start + size - 1)}"
	
}

object ROM {
	def apply( name: String, start: Int, data: Seq[Byte] ) = {
		new ROM( name, start, start + data.length - 1 ) {
			data.copyToArray( mem )
		}
	}
}

trait Device extends Addressable {
	
	def init {}
	
	def disable {}
	
	override def toString = s"$name device: ${hexWord(start)}-${hexWord(start + size - 1)}"

}

abstract class SingleAddressDevice extends Device {
	
	val size = 1
	
	override def toString = s"$name device: ${hexWord(start)}"
	
}

abstract class ReadOnlyDevice extends SingleAddressDevice {
	
	def writeByte( addr: Int, value: Int ) = sys.error( "read only device" )
	
}

abstract class WriteOnlyDevice extends SingleAddressDevice {
	
	def readByte( addr: Int ) = sys.error( "write only device" )
	
}

abstract class Memory extends Addressable {
	
	val name = "System memory"
	protected val regions = new ArrayBuffer[Addressable]
	protected var first = 0
	protected var end = 0
	
	def init
	
	init
	
	protected def lookup( addr: Int ) =
		regions.indexWhere( r => r.start <= addr && r.start + r.size > addr ) match {
			case -1 => None
			case ind => Some( regions(ind) )
		}
	
	protected def find( addr: Int ) =
		lookup( addr ) match {
			case None => sys.error( addr.toHexString + " is not an addressable memory location" )
			case Some( r ) => r
		}
		
	def start = first
	
	def size = end - first
	
	def readByte( addr: Int ) = find( addr ).readByte( addr )
	
	def writeByte( addr: Int, value: Int ) = find( addr ).writeByte( addr, value )
	
	override def program( addr: Int, value: Int ) = find( addr ).program( addr, value )
	
	def addressable( addr: Int ) = lookup( addr ) != None
	
	protected def find( addr: Int, pred: Addressable => Boolean ) =
		lookup( addr ) match {
			case None => false
			case Some( r ) => pred( r )
		}
	
	def device( addr: Int ) = find( addr, _.isDevice )
	
	def memory( addr: Int ) = find( addr, r => r.isRAM || r.isROM )
	
	def remove( name: String ) {
		regions.indexWhere( r => r.name == name ) match {
			case -1 => sys.error( "not found: " + name )
			case ind =>
				if (regions(ind) isDevice)
					regions(ind).asInstanceOf[Device].disable
					
				regions remove ind
		}
	}
	
	def seqDevice = (regions filter (r => r.isDevice)).asInstanceOf[Seq[Device]]
	
	def seqROM = regions filter (r => r.isROM)
	
	def removeROM =
		for (r <- seqROM)
			regions -= r
	
	def removeRAM =
		for (r <- regions filter (r => r.isRAM && !r.isDevice))
			regions -= r
	
	def removeDevices =
		for (r <- seqDevice) {
			r.disable
			regions -= r
		}
		
	def code = {
		val roms = seqROM
		
		if (roms isEmpty)
			0
		else
			roms.head.start
	}
	
	def clearRAM =
		for (r <- regions filter (r => r.isRAM))
			r.asInstanceOf[RAM].clear
			
	def add( region: Addressable ) {
		regions find (r => r.start <= region.start && region.start < r.start + r.size) match {
			case Some(r) => sys.error( hexWord(region.start) + ", " + hexWord(region.size) + " overlaps " + hexWord(r.start) + ", " + hexWord(r.size) )
			case None =>
		}

		regions find (r => r.name == region.name) match {
			case Some(r) => sys.error( "duplicate region: " + region.name )
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