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

class KeyPress( val start: Int ) extends SingleAddressDevice {
	
	val name = "keyPress"
	var key = 0
	
	def readByte( addr: Int ) = key
	
	def writeByte( addr: Int, value: Int ) = key = value
	
}

class VideoRAM( start: Int, keyPress: KeyPress, width: Int, height: Int, square: Int, cpu: CPU, palette: Seq[Int] ) extends RAM( "video", start, start + width*height - 1 ) with Device {

	import javax.swing.WindowConstants._
	
	import scala.swing._
	import Swing._
	import BorderPanel.Position._
	import scala.swing.event._
	
	require( start >= 0 )
	require( width > 0 )
	require( height > 0 )
	require( !palette.isEmpty )
	
	val colors = (palette map {c => new Color(c)}).toArray
	val panel = new Panel {
		preferredSize = (width*square, height*square)
		
		if (keyPress ne null) {
			listenTo( keys )
			reactions += {
				case ev: KeyPressed =>
					keyPress.key = ev.peer.getKeyChar
				}
			focusable = true
			requestFocus
		}

		override def paintComponent( g: Graphics2D ) {
			for (x <- 0 until width; y <- 0 until height) {
				g.setColor( colors(mem(x + y*width)&0x0F) )
				g.fillRect( x*square, y*square, square, square )
			}
		}
	}
	
	val frame =
		new Frame {
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
			peer.setDefaultCloseOperation( DO_NOTHING_ON_CLOSE )
		}
	
	override def init {
		frame.visible = true
	}
	
	override def disable {
		frame.visible = false
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