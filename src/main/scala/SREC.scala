package xyz.hyperreal.mos6502

import collection.mutable.{ListBuffer}


object SREC {
	
	def apply( s: io.Source, m: Memory ) {
		def header( bytes: Vector[Byte] ) {
			
		}
		
		def data( addr: Int, bytes: Vector[Byte] ) {
			
		}
		
		def start( addr: Int ) {
			
		}
		
		apply( s, header, data, start )
	}
	
	def apply( s: io.Source, header: Vector[Byte] => Unit, data: (Int, Vector[Byte]) => Unit, start: Int => Unit ) {
		var headerSeen = false
		var count = 0
		
		s.getLines.zipWithIndex foreach {
			case (line, num) =>
				def problem( col: Int, msg: String ) = println( "error on line " + (num + 1) + ": " + msg + '\n' + line + '\n' + " "*col + '^' )
				
				def hexb( index: Int ) = {
					for (i <- index until index + 2)
						if (!("0123456789abcdefABCDEF" contains line(i))) problem( index + i, "non-hexadecial character" )
				
					Integer.parseInt( line.substring(index, index + 2), 16 )
				}
				
				if (line.length < 10) problem( 0, "line too short" )
				
				if (line.length%2 != 0) problem( 0, "line has an odd number of characters" )
				
				if (line(0) != 'S') problem( 0, "expected 'S'" )
				
				val binary = for (i <- 2 until line.length - 2 by 2) yield hexb( i ).toByte
				
				def byte( index: Int ) = (binary(index)&0xFF)
				
				def word( index: Int ) = (byte(index) << 8) | byte(index + 1)
				
				if (((~binary.sum)&0xFF) != hexb( line.length - 2 )) problem( line.length - 2, "incorrect checksum" )
				
				if (binary(0) != binary.length) problem( 2, "incorrect count" )
				
				line(1) match {
					case '0' =>
						if (headerSeen) problem( 1, "duplicate header record" )
						
						if (word(1) != 0) problem( 4, "address field should be 0000 for header" )
						
						headerSeen = true
						header( (binary drop 3).asInstanceOf[Vector[Byte]] )
					case '1' =>
						count += 1
						data( word(1), (binary drop 3).asInstanceOf[Vector[Byte]] )
					case '5' =>
						if (count != word(1))
							problem( 9, "incorrect record count" )
					case '9' => start( word(1) )
					case _ => problem( 1, "unknown record type" )
				}
		}
	}
	
}