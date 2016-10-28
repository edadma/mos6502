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
						out.println( emu.mem )
					case "breakpoint"|"b" =>
						if (com.length > 1)
							if (com(1) == "--")
								emu.clearBreakpoints
							else if (com(1) startsWith "-")
								emu.clearBreakpoint( emu.target(com(1) drop 1) )
							else
								emu.setBreakpoint( emu.target(com(1)) )
						
						println( emu.breakpoints map {case (b, l) => hexWord(b) + (if (l != "") "/" + l else "")} mkString " " )
					case "disassemble"|"u" =>
						disassemble( (if (com.length > 1) emu.target( com(1) ) else -1), 15 )
					case "clear"|"c" =>
						if (com.length > 2)
							for (i <- hex( com(1) ) until hex( com(2) ))
								emu.mem.program( i, 0 )
						else
							emu.mem.clearRAM
					case "drop"|"dr" =>
						emu.mem.remove( com(1) )
						out.println( emu.mem )
					case "dump"|"d" =>
						val from =
							if (com.length > 1)
								emu.target( com(1) )
							else
								-1
								
						dump( from, 10 )
					case "execute"|"e" =>
						if (com.length > 1)
							emu.cpu.PC = emu.target( com(1) )
						
						emu.run
						registers
					case "help"|"h" =>
						"""
						|assemble (a) <file>            clear ROM, assemble <file>, and reset CPU
						|assemble (a) <org>             clear ROM, assemble REPL input at <org>, and reset CPU
						|breakpoint (b) <addr>*         set/clear breakpoint at <addr>
						|disassemble (u) [<addr>*]      print disassembled code at <addr> or where left off
						|clear (c) [<addr1>* <addr2>*]  clear RAM, optionally from <addr1> up to but not including <addr2>
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
							val addr = emu.target( com(1) )
							
							for ((d, i) <- com drop 2 map (emu.target) zipWithIndex)
								emu.program( addr + i, d )
								
							dump( addr, (com.length - 2 + addr%16)/16 + 1 )
						} else
							out.println( emu.mem )
					case "quit"|"q" => sys.exit
					case "registers"|"r" =>
						if (com.length > 2) {
							val n = emu.target( com(2) )
							
							com(1).toLowerCase match {
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
						}
						
						registers
					case "reload"|"rl" =>
						interp( reload )
					case "reset"|"re" =>
						emu.reset
					case "step"|"s" =>
						if (com.length > 1)
							emu.cpu.PC = emu.target( com(1) )
							
						emu.step
						registers
					case "save"|"sa" =>
						save( com(1) )
					case "symbols"|"sy" =>
						if (com.length > 2)
							emu.symbols += (com(1) -> emu.target( com(2) ))
						else
							out.println( emu.symbols )
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