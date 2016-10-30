package xyz.hyperreal.mos6502

import java.io.{RandomAccessFile, PrintWriter}

import jline.console.ConsoleReader


object CharViewer extends App {
	
	val reader = new ConsoleReader
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
	var line: String = null
	
	reader.setBellEnabled( false )
	reader.setPrompt( "> " )
	
	var file: RandomAccessFile = null

	def display( ind: Int ) {
		file.seek( ind*8 )
		
		for (i <- 0 until 8) {
			val l = file.read
			
			for (j <- 7 to 0 by -1)
				print( if (((l >> j)&1) != 0) "*" else " ")
				
			println
		}
	}
	
	def data( ind: Int ) {
		file.seek( ind*8 )
		print( "db " )
		
		for (i <- 0 until 8) {
			print( "$" + ("%02x" format file.read).toUpperCase + (if (i < 7) ", " else "") )
		}
		
		println
	}
	
	while ({line = reader.readLine; line != null}) {
		try {
			line.trim split "\\s+" toList match {
				case "c" :: from :: to :: _ =>
					for (i <- from.toInt to to.toInt)
						display( i )
				case "c" :: ind :: _ => display( ind.toInt )
				case "d" :: from :: to :: _ =>
					for (i <- from.toInt to to.toInt)
						data( i )
				case "d" :: ind :: _ => data( ind.toInt )
				case "h" :: Nil =>
					"""
					|c <index>            display character at <index>
					|c <from> <to>        display character at <from> up to <to>
					|d <index>            display assembly data for <index>
					|d <from> <to>        display assembly data for <from> up to <to>
					|h                    display this help
					|o <file>             open raw binary <file>
					|q                    quit
					""".trim.stripMargin.lines foreach println
				case "o" :: path :: _ =>
					if (file ne null)
						file.close
						
					file = new RandomAccessFile( path, "r" )
					println( "size: " + file.length + " (dec), " + file.length.toHexString + " (hex)" )
				case "q" :: Nil => sys.exit
				case Nil|"" :: _ =>
				case _ => println( "error" )
			}
		} catch {
			case e: Exception => println( e )
		}
		
		out.println
	}
	
}