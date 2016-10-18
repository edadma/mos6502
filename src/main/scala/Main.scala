package xyz.hyperreal.mos6502

import java.io.{File, PrintWriter}

import jline.console.ConsoleReader

import xyz.hyperreal.options._


object Main extends App with Flags {
	
	val mem = new Memory
	val cpu = new CPU6502( mem )
	
	mem add new RAM( "main", 0x0000, 0x7FFF )
	mem add new StdIOChar( 0x8000 )
	mem add new StdIOInt( 0x8001 )
	
	val options = new Options( List(), List("-f"), Nil )
	
	options parse args
	
	options get "-f" match {
		case None => monitor
		case Some( file ) =>
			load( file )
			cpu.reset
			cpu.run
	}
	
	def ishex( s: String ) = !s.isEmpty && s.forall( c => "012345679abcdefABCDEF" contains c )
	
	def hex( s: String ) = Integer.parseInt( s, 16 )
	
	def load( file: String ) {
		mem.clearROM
		SREC( mem, new File(file) )
	}
	
	def monitor {
		val reader = new ConsoleReader
		val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
		var line: String = null
		
		reader.setBellEnabled( false )
		reader.setPrompt( "> " )

		def registers {
			out.printf( "A:%s X:%s Y:%s SP:%s PC:%s\n", hexByte(cpu.A), hexByte(cpu.X), hexByte(cpu.Y), hexWord(cpu.SP), hexWord(cpu.PC) )
			out.printf( "N:%s V:%s B:%s D:%s I:%s Z:%s C:%s\n", Seq(N, V, B, D, I, Z, C) map (cpu.read(_).toString): _* )
			out.println( hexWord(cpu.PC) + ':' + hexByte(mem.readByte(cpu.PC)) )
		}
	
		def dump( start: Int, lines: Int ) {
			val addr = start - start%16
			
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
					
			for (line <- addr until ((addr + 16*lines) min 0x10000) by 16) {
				out.print( "%4x  ".format(line).toUpperCase )
				
				for (i <- line until ((line + 16) min 0x10000)) {
					if (i%16 == 8)
						out.print( ' ' )
						
					printByte( read(i) )
				}
				
				val bytes = ((line + 16) min 0x10000) - line
				
				out.print( " "*((16 - bytes)*3 + 1 + (if (bytes < 9) 1 else 0)) )
				
				for (i <- line until ((line + 16) min 0x10000))
					printChar( read(i) )
					
				out.println
			}
		}
		
		var dumpcur = 0
		
		out.println( "MOS 6502 emulator v0.1" )
		out.println( "Type 'help' for list of commands." )
		out.println
		
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
							
						dump( dumpcur, 8 )
						dumpcur = (dumpcur + 16*8) min 0x10000
					case "execute"|"e" =>
						if (com.length > 1)
							if (ishex( com(1) ))
								cpu.PC = hex( com(1) )
							else {
								load( com(1) )
								cpu.reset
							}
						
						cpu.run
						registers
					case "help"|"h" =>
						"""
						|drop (dr) <region>           drop memory <region>
						|dump (d) [<addr>]            dump memory at <addr> or where left off
						|execute (e) [<start>]        execute instructions starting from current PC or <start>
						|execute (e) <file>           clear ROM, load SREC <file>, reset, run
						|help (h)                     print this summary
						|load (l) <file>              clear ROM, load SREC <file>, reset
						|memory (m)                   print memory map
						|memory (m) <addr> <data>...  write <data> (space separated bytes) to memory at <addr>
						|quit (q)                     exit the REPL
						|registers (r)                print CPU registers
						|registers (r) [<reg> <val>]  set CPU <reg>ister to <val>ue
						|reset (re)                   reset CPU registers setting PC from reset vector
						|step (s) [<start>]           execute only next instruction at current PC or <start>
						""".trim.stripMargin.lines foreach out.println
					case "load"|"l" =>
						load( com(1) )
					case "memory"|"m" =>
						if (com.length > 2) {
							val addr = hex( com(1) )
							
							for ((d, i) <- com drop 2 map (hex) zipWithIndex)
								mem.program( addr + i, d )
								
							dump( addr, (com.length - 2 + addr%16)/16 + 1 )
						} else
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
						
						registers
					case "reset"|"re" =>
						cpu.reset
					case "step"|"s" =>
						if (com.length > 1)
							cpu.PC = hex( com(1) )
							
						cpu.step
						registers
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