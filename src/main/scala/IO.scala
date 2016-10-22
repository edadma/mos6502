package xyz.hyperreal.mos6502


class StdIOChar( val start: Int ) extends SingleAddressDevice {
	
	val name = "stdio-char"
	
	def readByte( addr: Int ) = io.StdIn.readChar.toInt
	
	def writeByte( addr: Int, value: Int ) = print( value.toChar )
	
}

class StdIOInt( val start: Int ) extends SingleAddressDevice {
	
	val name = "stdio-int"
	
	def readByte( addr: Int ) = io.StdIn.readInt
	
	def writeByte( addr: Int, value: Int ) = print( value )
	
}

class StdIOHex( val start: Int ) extends SingleAddressDevice {
	
	val name = "stdio-hex"
	
	def readByte( addr: Int ) = hex( io.StdIn.readLine )
	
	def writeByte( addr: Int, value: Int ) = print( value.toHexString )
	
}

class RNG( val start: Int ) extends ReadOnlyDevice {
	
	val name = "rng"
	
	def readByte( addr: Int ) = util.Random.nextInt( 0x100 )
	
}

// class RBG( val start: Int ) extends ReadOnlyDevice {
// 	
// 	val name = "rbg"
// 	
// 	def readByte( addr: Int ) = if (util.Random.nextInt( 100 ) < 5) 1 else 0
// 	
// }

class VideoRAM( start: Int, width: Int, height: Int, cpu: CPU, palette: Seq[Int] ) extends RAM( "video", start, start + width*height - 1 ) with Device {

	import scala.swing._
	import Swing._
	import BorderPanel.Position._
	
	require( start >= 0 )
	require( width > 0 )
	require( height > 0 )
	require( !palette.isEmpty )
//	require( palette forall {case (r, g, b) => 0 <= r && r <= 255 && 0 <= g && g <= 255 && 0 <= b && b <= 255} )
	
// 	val colors = (palette map {case (r, g, b) => r << 16 | g << 8 | b}).toArray
	val colors = (palette map {c => new Color(c)}).toArray
	val square = 6
	
	val panel = new Panel {
		preferredSize = (width*square, height*square)
		
		override def paintComponent( g: Graphics2D ) {
			for (x <- 0 until width; y <- 0 until height) {
				g.setColor( colors(mem(x + y*width)&0x0F) )
				g.fillRect( x*square, y*square, square, square )
			}
		}
	}
	
	val frame = new Frame {
		title = "Video"
		contents =
			new BorderPanel {
				layout(panel) = Center
				layout(
					Button( "Stop" ) {
						cpu.stop
					}
				) = South
			}
		pack
		
		override def closeOperation = sys.exit
	}
	
	override def init {
		frame.visible = true
	}
	
	override def clear = {
		super.clear
		panel.repaint
	}
	
	override def writeByte( addr: Int, value: Int ) = {
		super.writeByte( addr, value )
		panel.repaint
	}
	
}