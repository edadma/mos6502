package xyz.hyperreal.mos6502

import java.io.{File, PrintWriter}

import jline.console.ConsoleReader


object Main extends App with Flags {
	
	lazy val emu = new Emulator( "6502" )
	var enterREPL = true
	
	Options( args )
	{
		case "-a" :: file :: _ =>
			assemble( file )
			Nil
		case "-ae" :: file :: _ =>
			assemble( file )
			emu.run
			enterREPL = false
			Nil
		case "-as" :: file :: _ =>
			assemble( file )
			save( file + ".s19" )
			enterREPL = false
			Nil
		case "--help" :: _ =>
			"""
			|MOS 6502 emulator v0.3
			|Usage:  --help      display this help and exit
			|        -l <file>   load SREC <file> and enter REPL
			|        -le <file>  load SREC <file> and execute
			|        -a <file>   assemble source <file> and enter REPL
			|        -ae <file>  assemble source <file> and execute
			|        -as <file>  assemble source <file> and save SREC
			""".trim.stripMargin.lines foreach println
			enterREPL = false
			Nil
		case "-l" :: file :: _ =>
			load( file )
			Nil
		case "-le" :: file :: _ =>
			load( file )
			emu.run
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
		REPL

	def assemble( file: String ) = emu.assemble( io.Source.fromFile(file) )
	
	def load( file: String ) = emu.load( file )

	def save( file: String ) = emu.save( file )
	
	def waitUntilRunning = {
		while (!emu.cpu.isRunning) {}
	}
	
	def waitWhileRunning = {
		while (emu.cpu.isRunning) {}
	}
	
	def REPL {
		val reader = new ConsoleReader
		val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
		var line: String = null
		var reload = ""
		
		reader.setBellEnabled( false )
		reader.setPrompt( "> " )

		emu.reregister( "_stdioInt_",
			(p: String, mem: Memory, cpu: CPU) => {
				mem add new JLineInt( hex(p), reader )
			} )
		emu.reregister( "_stdioHex_",
			(p: String, mem: Memory, cpu: CPU) => {
				mem add new JLineHex( hex(p), reader )
			} )

		def registers = out.println( emu.registers )
	
		def dump( start: Int, lines: Int ) = out.println( emu.dump(start, lines) )
		
		def disassemble( start: Int, lines: Int ) = out.println( emu.disassemble(start, lines) )
		
		def printBreakpoints = out.println( emu.breakpoints map {case (b, l) => hexWord(b) + (if (l != "") "/" + l else "")} mkString " " )
		
		def runAndWait {
			emu.run
			waitUntilRunning
			waitWhileRunning
			registers
		}
		
		out.println( "MOS 6502 Emulator v0.5" )
		out.println( "Type 'help' for list of commands." )
		out.println
		
		def interp( command: String ) {
			val com = command.trim split "\\s+" toList
			
			try {
				com match {
					case List( "assemble"|"a", file ) =>
						reload = command
						assemble( file )
						out.println( emu.mem )
					case List( "breakpoint"|"b" ) =>
						printBreakpoints
					case List( "breakpoint"|"b", "--" ) =>
						emu.clearBreakpoints
						printBreakpoints
					case List( "breakpoint"|"b", bp ) if bp startsWith "-" =>
						emu.clearBreakpoint( emu.target(bp drop 1) )
						printBreakpoints
					case List( "breakpoint"|"b", bp ) =>
						emu.setBreakpoint( emu.target(bp) )
						printBreakpoints
					case List( "disassemble"|"u", addr )  =>
						disassemble( emu.target( addr ), 15 )
					case List( "disassemble"|"u" )  =>
						disassemble( -1, 15 )
					case List( "clear"|"c", addr1, addr2 ) =>
						for (i <- hex( addr1 ) until hex( addr2 ))
							emu.mem.program( i, 0 )
					case List( "clear"|"c" ) =>
						emu.mem.clearRAM
					case List( "drop"|"dr", region ) =>
						emu.mem.remove( region )
						out.println( emu.mem )
					case List( "dump"|"d", addr ) =>
						dump( emu.target(addr), 10 )
					case List( "dump"|"d" ) =>
						dump( -1, 10 )
					case List( "execute"|"e", addr ) =>
						emu.cpu.PC = emu.target( addr )
						emu.run
					case List( "execute"|"e" ) =>
						emu.run
					case List( "execute&wait"|"ew", addr ) =>
						emu.cpu.PC = emu.target( addr )
						runAndWait
					case List( "execute&wait"|"ew" ) =>
						runAndWait
					case List( "help"|"h" ) =>
						"""
						|assemble (a) <file>              clear ROM, assemble <file>, and reset CPU
						|assemble (a) <org>               clear ROM, assemble REPL input at <org>, and reset CPU
						|breakpoint (b) <addr>*           set/clear breakpoint at <addr>
						|disassemble (u) [<addr>*]        print disassembled code at <addr> or where left off
						|clear (c) [<addr1>* <addr2>*]    clear RAM, optionally from <addr1> up to but not including <addr2>
						|drop (dr) <region>               drop memory <region>
						|dump (d) [<addr>*]               print memory at <addr> or where left off
						|execute (e) [<addr>*]            execute instructions starting from current PC or <addr>
						|execute&wait (ew) [<addr>*]      execute instructions starting from current PC or <addr> and wait to finish
						|help (h)                         print this summary
						|load (l) <file>                  clear ROM, load SREC <file>, and reset CPU
						|memory (m)                       print memory map
						|memory (m) <addr>* <data>*...    write <data> (space separated bytes) to memory at <addr>
						|quit (q)                         exit the REPL
						|registers (r)                    print CPU registers
						|registers (r) <reg> <val>*       set CPU <reg>ister to <val>ue
						|reload (rl)                      redo last 'load' or 'assemble' command
						|reset (re)                       reset CPU registers setting PC from reset vector
						|step (s) [<addr>*]               execute only next instruction at current PC or <addr>
						|stop (st)                        stop code execution
						|save (sa) <file>                 save all ROM contents to SREC file
						|symbols (sy)                     print symbol table
						|symbols (sy) <symbol> <val>*     add <symbol> with associated <val>ue to symbol table
						|trace (t) on/off                 turn CPU trace on or off
						|* can either be a hexadecimal value or label (optionally followed by a colon)
						""".trim.stripMargin.lines foreach out.println
					case List( "load"|"l", file ) =>
						reload = command
						load( file )
					case ("memory"|"m") :: addr :: data =>
						val addr1 = emu.target( addr )
						
						for ((d, i) <- data map emu.target zipWithIndex)
							emu.program( addr1 + i, d )
							
						dump( addr1, (data.length + addr1%16)/16 + 1 )
					case List( "memory"|"m" ) =>
						out.println( emu.mem )
					case List( "quit"|"q" ) =>
						emu.stop
						emu.mem.removeDevices
						sys.exit
					case List( "registers"|"r", reg, value ) =>
						val n = emu.target( value )
						
						reg.toLowerCase match {
							case "a" => emu.cpu.A = n
							case "x" => emu.cpu.X = n
							case "y" => emu.cpu.Y = n
							case "sp" => emu.cpu.SP = n
							case "pc" => emu.cpu.PC = n
							case "n" => emu.cpu.set( N, n )
							case "v" => emu.cpu.set( V, n )
							case "b" => emu.cpu.set( B, n )
							case "d" => emu.cpu.set( D, n )
							case "i" => emu.cpu.set( I, n )
							case "z" => emu.cpu.set( Z, n )
							case "c" => emu.cpu.set( C, n )
						}
						
						registers
					case List( "registers"|"r" ) =>
						registers
					case List( "reload"|"rl" ) =>
						interp( reload )
					case List( "reset"|"re" ) =>
						emu.reset
						registers
					case List( "step"|"s", addr ) =>
						emu.cpu.PC = emu.target( addr )
						emu.step
						registers
					case List( "step"|"s" ) =>
						emu.step
						registers
					case List( "stop"|"st" ) =>
						emu.stop
						waitWhileRunning
						registers
					case List( "save"|"sa", file ) =>
						save( file )
					case List( "symbols"|"sy", symbol, value ) =>
						emu.symbols += (symbol -> emu.target( value ))
					case List( "symbols"|"sy" ) =>
						out.println( "name            value segment" )
						out.println( "----            ----- -------" )
						for ((s, v) <- emu.symbols.toList sortBy (_._1))
							v match {
								case str: String => out.printf( "%-15s %-5s\n", s, '"' + str + '"' )
								case addr: Int =>
									val seg =
										emu.segments get addr match {
											case None =>
												if (emu.segments isEmpty)
													""
												else {
													val range = emu.segments.range( emu.segments.min._1, addr )
													
													if (range isEmpty)
														""
													else {
														val (base, (name, len)) = range.max
														
														if (addr < base + len)
															name
														else
															""
													}
												}
											case Some( (name, _) ) => name
										}
										
									out.printf( "%-15s %-5s %s\n", s, hexWord(addr), seg )
							}
					case List( "trace"|"t", "on" ) =>
						emu.cpu.trace = true
					case List( "trace"|"t", "off" ) =>
						emu.cpu.trace = false
					case Nil|List( "" ) =>
					case _ => out.println( "error interpreting command" )
				}
			}
			catch
			{
				case e: Exception =>
//					out.println( e )
					e.printStackTrace( out )
			}
		}
		
		while ({line = reader.readLine; line != null}) {
			interp( line )
			out.println
		}
	}
}