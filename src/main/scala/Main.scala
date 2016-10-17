package xyz.hyperreal.mos6502

import java.io.{File, PrintWriter}

import jline.console.ConsoleReader

import xyz.hyperreal.options._


object Main extends App with Flags {
	
	val options = new Options( List(), List("-f"), Nil )
	val mem = new Memory
	val cpu = new CPU6502( mem )
	
	mem add new RAM( "zero-page", 0x0000, 0x00DF )
	mem add new RAM( "stack", 0x0100, 0x01FF )
	mem add new VideoRAM( 0x0200 )
	mem add new RAM( "main", 0x1000, 0x2FFF )
	mem add new StdIOChar( 0xE0 )
	mem add new StdIOInt( 0xE1 )
	
	options parse args
	
	options get "-f" match {
		case None => monitor
		case Some( file ) => println( "load " + file )
	}
	
	def hex( s: String ) = Integer.parseInt( s, 16 )
	
	def monitor {
		System.getProperties.setProperty( "jline.shutdownhook", "true" )

		val reader = new ConsoleReader
		val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
		var line: String = null
		var dumpcur = 0
		
		reader.setBellEnabled( false )
		reader.setPrompt( "> " )

		while ({line = reader.readLine; line != null})
		{
			val com = line.trim.split( "\\s+" )
			
			try
			{
				com.head match {
					case "" =>
					case "drop"|"dr" =>
						mem.remove( com(1) )
						out.println( mem )
					case "dump"|"d" =>
						if (com.length > 1)
							dumpcur = hex( com(1) )
							
						val width = 16
						
						def printByte( b: Option[Int] ) =
							if (b == None)
								out.print( "-- " )
							else
								out.print( "%02x ".format(b.get&0xFF).toUpperCase )
						
						def printChar( c: Option[Int] ) = out.print( if (c != None && ' ' <= c.get && c.get <= '~') c.get.asInstanceOf[Char] else '.' )
						
						def read( addr: Int ) =
							if (!mem.addressable( addr ) || mem.port( addr ))
								None
							else
								Some( mem.readByte(addr) )
								
						for (line <- dumpcur until (dumpcur + width*16 min 0x10000) by width) {
							out.print( "%4x  ".format(line).toUpperCase )
							
							for (i <- line until ((line + width) min 0x10000)) {
								if (i%16 == 8)
									out.print( ' ' )
									
								printByte( read(i) )
							}
							
							val bytes = ((line + width) min 0x10000) - line
							
							out.print( " "*((width - bytes)*3 + 1 + (if (bytes < 9) 1 else 0)) )
							
							for (i <- line until ((line + width) min 0x10000))
								printChar( read(i) )
								
							out.println
						}
						
						dumpcur = dumpcur + width*16 min 0x10000
					case "help"|"h" =>
						out.println( "drop (dr) <region>           drop memory <region>" )
						out.println( "dump (d) [<addr>]            dump memory at <addr> or where left off" )
						out.println( "execute (e)                  clear ROM, load <file>, and reset" )
						out.println( "help (h)                     print this summary" )
						out.println( "load (l) <file>              clear ROM and load <file>" )
						out.println( "map (m)                      print memory map" )
						out.println( "quit (q)                     exit the REPL" )
						out.println( "registers (r) [<reg> <val>]  print CPU registers or set <reg>ister to <val>ue" )
						out.println( "reset (re)                   reset CPU and execute instructions from reset vector" )
						out.println( "run (ru) [<start>]           execute instructions starting from current PC or <start>" )
						out.println( "step                         execute only next instruction at current PC" )
					case "load"|"l" =>
						mem.clear
						SREC( mem, new File(com(1)) )
					case "map"|"m" =>
						out.println( mem )
					case "quit"|"q" => sys.exit
					case "registers"|"r" =>
						if (com.length > 2) {
							val n = hex( com(2) )
							
							com(1).toLowerCase match {
								case "a" => cpu.A = n
								case "x" => cpu.X = n
								case "y" => cpu.Y = n
								case "sp" => cpu.SP = n
								case "pc" => cpu.PC = n
								case "n" => cpu.set( N, n )
								case "v" => cpu.set( V, n )
								case "b" => cpu.set( B, n )
								case "d" => cpu.set( D, n )
								case "i" => cpu.set( I, n )
								case "z" => cpu.set( Z, n )
								case "c" => cpu.set( C, n )
							}
						}
						
						out.printf( "A:%s X:%s Y:%s SP:%s PC:%s\n", hexByte(cpu.A), hexByte(cpu.X), hexByte(cpu.Y), hexWord(cpu.SP), hexWord(cpu.PC) )
						out.printf( "N:%s V:%s B:%s D:%s I:%s Z:%s C:%s\n", cpu.read(N).toString, cpu.read(V).toString, cpu.read(B).toString, cpu.read(D).toString, cpu.read(I).toString, cpu.read(Z).toString, cpu.read(C).toString )
					case "run" =>
					case c => out.println( "unrecognized command: " + c )
				}
			}
			catch
			{
				case e: Exception =>
// 								if (trace)
// 									e.printStackTrace( out )
// 								else
						out.println( e )
			}
			
			out.println
		}
	}
}