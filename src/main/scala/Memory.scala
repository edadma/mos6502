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
	
	def readByte( addr: Int ) = mem( addr - start )&0xFF
	
	def writeByte( addr: Int, value: Int ) = mem( addr - start ) = value.toByte
	
	override def toString = s"$name RAM: ${hexWord(start)}-${hexWord(end)}"
}

class ROM( val name: String, val start: Int, vals: Seq[Byte] ) extends Addressable {
	
	require( start >= 0 )
	require( vals.size > 0 )
	
	protected val mem = vals.toArray
	val size = mem.size
	
	def readByte( addr: Int ) = mem( addr - start )&0xFF
	
	def writeByte( addr: Int, value: Int ) = sys.error( "read only memory" )
	
	override def program( addr: Int, value: Int ) = mem( addr - start ) = value.toByte
	
	override def toString = s"$name ROM: ${hexWord(start)}-${hexWord(start + size - 1)}"
	
}

object ROM {
	def apply( name: String, start: Int, mem: Int* ) = new ROM( name, start, mem.toIndexedSeq map (_.toByte) )
}

object Vectors {
	def apply( resetVector: Int, brkVector: Int ) = {
		def word2bytes( a: Int ) = IndexedSeq( a.toByte, (a >> 8).toByte )
		
		new ROM( "vector table", 0xFFFC, word2bytes(resetVector) ++ word2bytes(brkVector) )
	}
}

abstract class Port extends Addressable {
	
	val start: Int
	val size: Int
	
	override def toString = s"$name port: ${hexWord(start)}-${hexWord(start + size - 1)}"

}

abstract class SingleAddressPort extends Port {
	
	val size = 1
	
	override def toString = s"$name port: ${hexWord(start)}"
	
}

abstract class ReadOnlyPort extends SingleAddressPort {
	
	def writeByte( addr: Int, value: Int ) = sys.error( "read only port" )
	
}

abstract class WriteOnlyPort extends SingleAddressPort {
	
	def readByte( addr: Int ) = sys.error( "write only port" )
	
}

class Memory extends Addressable {
	
	val name = "System memory"
	private val regions = new ArrayBuffer[Addressable]
	private var first = 0
	private var end = 0
	
	private def lookup( addr: Int ) =
		regions.indexWhere( r => r.start <= addr && r.start + r.size > addr ) match {
			case -1 => None
			case ind => Some( regions(ind) )
		}
	
	private def find( addr: Int ) =
		lookup( addr ) match {
			case None => sys.error( addr.toHexString + " is not an addressable memory location" )
			case Some( r ) => r
		}
		
	def start = first
	
	def size = end - first
	
	def readByte( addr: Int ) = find( addr ).readByte( addr )
	
	def writeByte( addr: Int, value: Int ) = find( addr ).writeByte( addr, value )
	
	def addressable( addr: Int ) = lookup( addr ) != None
	
	def port( addr: Int ) = find( addr ).isInstanceOf[Port]
	
	def remove( name: String ) {
		regions.indexWhere( r => r.name == name ) match {
			case -1 => sys.error( "not found: " + name )
			case ind => regions remove ind
		}
	}
	
	def clear =
		for (r <- regions filter (r => r.isInstanceOf[ROM]))
			regions -= r
			
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