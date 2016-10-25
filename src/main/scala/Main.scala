package xyz.hyperreal.mos6502

import java.io.{File, PrintWriter}

import jline.console.ConsoleReader


object Main extends App with Flags {
	
	val mem = new Memory
	val cpu = new CPU6502( mem )
	
	var dumpcur = 0
	var discur = 0
	var symbols = Map[String, Any]()
	var reverseSymbols = Map[Any, String]()
	
	mem add new RAM( "main", 0x0000, 0x5FFF )
	
	var enterREPL = true
	
	Options( args )
	{
		case "-a" :: file :: _ =>
			assemble( file )
			Nil
		case "-ae" :: file :: _ =>
			assemble( file )
			cpu.run
			enterREPL = false
			Nil
		case "-as" :: file :: _ =>
			assemble( file )
			save( file + ".s19" )
			enterREPL = false
			Nil
		case "--help" :: _ =>
			println( "MOS 6502 emulator v0.3" )
			println( "Usage:  --help      display this help and exit" )
			println( "        -l <file>   load SREC <file> and enter REPL" )
			println( "        -le <file>  load SREC <file> and execute" )
			println( "        -a <file>   assemble source <file> and enter REPL" )
			println( "        -ae <file>  assemble source <file> and execute" )
			println( "        -as <file>  assemble source <file> and save SREC" )
			enterREPL = false
			Nil
		case "-l" :: file :: _ =>
			load( file )
			Nil
		case "-le" :: file :: _ =>
			load( file )
			cpu.run
			enterREPL = false
			Nil
		case o :: _ if o startsWith "-" =>
			println( "bad option: " + o )
			enterREPL = false
			Nil
		case _ :: t =>
			t
	}

	if (enterREPL)
		repl
		
	def load( file: String ) {
		mem.removeROM
		SREC( mem, new File(file) )
		discur = mem.code
		cpu.reset
	}

	def save( file: String ) {
		SREC.write( mem, new File(file), file.getBytes.toVector )
	}
	
	def assemble( file: String ) {
		var purged = false
		
		def purge =
			if (!purged) {
				purged = true
				mem.removeDevices
			}
			
		mem.removeROM
		symbols = Assembler( mem, io.Source.fromFile(file) )
		reverseSymbols = symbols map {case (s, t) => (t, s)}
		discur = mem.code
		
		for ((k, v) <- symbols)
			(k, v) match {
				case ("_stdioChar_", p: String) =>
					purge
					mem add new StdIOChar( hex(p) )
				case ("_stdioInt_", p: String) =>
					purge
					mem add new StdIOInt( hex(p) )
				case ("_stdioHex_", p: String) =>
					purge
					mem add new StdIOHex( hex(p) )
				case ("_rng_", p: String) =>
					purge
					mem add new RNG( hex(p) )
				case ("_video_", p: String) =>
					val parms = p split ","
					purge
					mem add new VideoRAM( hex(parms(0)), hex(parms(1)), hex(parms(2)), cpu, (for (i <- 3 to 18) yield hex(parms(i))).toIndexedSeq )
				case ("_ram_", p: String) =>
					mem.removeRAM
					
					val block = """(\p{XDigit}+)\-(\p{XDigit}+)"""r
					
					for ((m, ind) <- block findAllMatchIn p zipWithIndex)
						mem add new RAM( "main" + ind, hex(m group 1), hex(m group 2) )
				case _ =>
			}
			
		cpu.reset
	}
	
	def repl {
		val reader = new ConsoleReader
		val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
		var line: String = null
		var reload = ""
		
		reader.setBellEnabled( false )
		reader.setPrompt( "> " )

		def registers {
			out.printf( "A:%s X:%s Y:%s SP:%s PC:%s\n", hexByte(cpu.A), hexByte(cpu.X), hexByte(cpu.Y), hexWord(cpu.SP), hexWord(cpu.PC) )
			out.printf( "N:%s V:%s B:%s D:%s I:%s Z:%s C:%s\n", Seq(N, V, B, D, I, Z, C) map (cpu.read(_).toString): _* )
			disassemble( cpu.PC, 1 )
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
				if (mem.addressable( addr ) && mem.memory( addr ))
					Some( mem.readByte(addr) )
				else
					None
			
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
		
		def reference( target: Int, zp: Boolean ) =
			reverseSymbols get target match {
				case None => "$" + (if (zp) hexByte( target ) else hexWord( target ))
				case Some( l ) => l
			}
		
		def disassemble( start: Int, lines: Int ): Int = {
			var addr = start
			
			for (_ <- 1 to lines) {
				if (!mem.memory( addr ))
					return addr
					
				val opcode = mem.readByte( addr )
				
				CPU.dis6502 get opcode match {
					case None =>
					case Some( (mnemonic, mode) ) =>
						if (mode != 'implicit && mode != 'accumulator && (!mem.memory( addr + 1 ) || !mem.memory( addr + 2 )))
							return addr
				}
				
				val label =
					(reverseSymbols get addr match {
						case None => ""
						case Some( l ) => l
					})
					
				if (cpu.breakpoints( addr ))
					out.print( Console.BLUE_B )
					
				out.print( hexWord(addr) + "  " + hexByte(opcode) + " " )
				addr += 1
				
				CPU.dis6502 get opcode match {
					case None => out.print( " "*(6 + 2 + 15 + 1) + "---" )
					case Some( (mnemonic, mode) ) =>
						val (display, size) =
							(mode match {
								case 'implicit => ("", 0)
								case 'accumulator => ("A", 0)
								case 'immediate => ("#" + "$" + hexByte(mem.readByte(addr)), 1)
								case 'relative => (reference(mem.readByte(addr).toByte + addr + 1, false), 1)
								case 'indirectX => ("(" + reference(mem.readByte(addr), true) + ",X)", 1)
								case 'indirectY => ("(" + reference(mem.readByte(addr), true) + "),Y", 1)
								case 'zeroPage => (reference(mem.readByte(addr), true), 1)
								case 'zeroPageIndexedX => (reference(mem.readByte(addr), true) + ",X", 1)
								case 'zeroPageIndexedY => (reference(mem.readByte(addr), true) + ",Y", 1)
								case 'direct => (reference(mem.readWord(addr), false), 2)
								case 'directX => (reference(mem.readWord(addr), false) + ",X", 2)
								case 'directY => (reference(mem.readWord(addr), false) + ",Y", 2)
								case 'indirect => ("(" + reference(mem.readWord(addr), false) + ")", 2)
							})
						
						for (i <- 0 until size)
							out.print( hexByte(mem.readByte(addr + i)) + " " )
							
						addr += size
						
						out.print( " "*((2 - size)*3 + 2) )
						out.print( label + " "*(15 - label.length + 1) )
						out.print( mnemonic.toUpperCase + " " )
						out.print( display )
				}

				out.println( Console.RESET )
			}
			
			addr
		}
		
		def target( ref: String ) =
			if (isHex( ref ))
				hex( ref )
			else
				symbols get (if (ref endsWith ":") ref dropRight 1 else ref) match {
					case Some( t: Int ) => t
					case None => sys.error( "unknown label: " + ref )
					case Some( s ) => sys.error( "symbol not an integer: " + s )
				}
				
		out.println( "MOS 6502 emulator v0.3" )
		out.println( "Type 'help' for list of commands." )
		out.println
		
		def interp( command: String ) {
			val com = command.trim.split( "\\s+" )
			
			try {
				com.head match {
					case "assemble"|"a" =>
						reload = command
						assemble( com(1) )
						out.println( mem )
					case "breakpoint"|"b" =>
						if (com.length > 1) {
							if (com(1) == "--")
								cpu.breakpoints = Set[Int]()
							else if (com(1) startsWith "-")
								cpu.breakpoints -= target( com(1) drop 1 )
							else
								cpu.breakpoints += target( com(1) )
						}
						
						println( (cpu.breakpoints.toList map (b => hexWord(b) + (if (reverseSymbols contains b) "/" + reverseSymbols(b) else "")) sorted)
							mkString " " )
					case "disassemble"|"u" =>
						if (com.length > 1)
							discur = target( com(1) )
							
						discur = disassemble( discur, 15 )
					case "drop"|"dr" =>
						mem.remove( com(1) )
						out.println( mem )
					case "dump"|"d" =>
						if (com.length > 1)
							dumpcur = target( com(1) )
							
						dump( dumpcur, 8 )
						dumpcur = (dumpcur + 16*8) min 0x10000
					case "execute"|"e" =>
						if (com.length > 1)
							cpu.PC = target( com(1) )
						
						cpu.run
						registers
					case "help"|"h" =>
						"""
						|assemble (a) <file>            clear ROM, assemble <file>, and reset CPU
						|assemble (a) <org>             clear ROM, assemble REPL input at <org>, and reset CPU
						|breakpoint (b) <addr>*         set/clear breakpoint at <addr>
						|disassemble (u) [<addr>*]      print disassembled code at <addr> or where left off
						|drop (dr) <region>             drop memory <region>
						|dump (d) [<addr>*]             print memory at <addr> or where left off
						|execute (e) [<addr>*]          execute instructions starting from current PC or <addr>
						|help (h)                       print this summary
						|load (l) <file>                clear ROM, load SREC <file>, and reset CPU
						|memory (m)                     print memory map
						|memory (m) <addr>* <data>*...  write <data> (space separated bytes) to memory at <addr>
						|quit (q)                       exit the REPL
						|registers (r)                  print CPU registers
						|registers (r) <reg> <val>*     set CPU <reg>ister to <val>ue
						|reload (rl)                    redo last 'load' or 'assemble' command
						|reset (re)                     reset CPU registers setting PC from reset vector
						|step (s) [<addr>*]             execute only next instruction at current PC or <addr>
						|save (sa) <file>               save all ROM contents to SREC file
						|symbols (sy)                   print symbol table
						|symbols (sy) <symbol> <val>*   add <symbol> with associated <val>ue to symbol table
						|* can either be a hexadecimal value or label (optionally followed by a colon)
						""".trim.stripMargin.lines foreach out.println
					case "load"|"l" =>
						reload = command
						load( com(1) )
					case "memory"|"m" =>
						if (com.length > 2) {
							val addr = target( com(1) )
							
							for ((d, i) <- com drop 2 map (target) zipWithIndex)
								mem.program( addr + i, d )
								
							dump( addr, (com.length - 2 + addr%16)/16 + 1 )
						} else
							out.println( mem )
					case "quit"|"q" => sys.exit
					case "registers"|"r" =>
						if (com.length > 2) {
							val n = target( com(2) )
							
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
					case "reload"|"rl" =>
						interp( reload )
					case "reset"|"re" =>
						cpu.reset
						discur = mem.code
					case "step"|"s" =>
						if (com.length > 1)
							cpu.PC = target( com(1) )
							
						cpu.step
						registers
					case "save"|"sa" =>
						save( com(1) )
					case "symbols"|"sy" =>
						if (com.length > 2)
							symbols += (com(1) -> target( com(2) ))
						else
							out.println( symbols )
					case "" =>
					case c => out.println( "unrecognized command: " + c )
				}
			}
			catch
			{
				case e: Exception =>
					out.println( e )
//					e.printStackTrace( out )
			}
		}
		
		while ({line = reader.readLine; line != null}) {
			interp( line )
			out.println
		}
	}
}