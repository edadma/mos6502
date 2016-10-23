package xyz.hyperreal.mos6502

import java.io.{File, PrintWriter}

import jline.console.ConsoleReader


object Main extends App with Flags {
	
	val mem = new Memory
	val cpu = new CPU6502( mem )
	
	var dumpcur = 0
	var discur = 0
	var symbols = Map[String, Int]()
	var reverseSymbols = Map[Int, String]()
	
	mem add new RAM( "zp", 0x0000, 0x00FF )
	mem add new RAM( "stack", 0x0100, 0x01FF )
	mem add new RAM( "main", 0x0600, 0x7FFF )
//	mem add new VideoRAM( 0x0200, 32, 32, cpu, Vector(0x000000, 0xffffff, 0x880000, 0xaaffee, 0xcc44cc, 0x00cc55, 0x0000aa, 0xeeee77, 0xdd8855, 0x664400, 0xff7777, 0x333333, 0x777777, 0xaaff66, 0x0088ff, 0xbbbbbb) )
	mem add new StdIOChar( 0x8000 )
	mem add new StdIOInt( 0x8001 )
	mem add new StdIOHex( 0x8002 )
	mem add new RNG( 0x8003 )
	
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
		mem.removeROM
		symbols = Assembler( mem, io.Source.fromFile(file) )
		reverseSymbols = symbols map {case (s, t) => (t, s)}
		discur = mem.code
		cpu.reset
	}
	
	def repl {
		val reader = new ConsoleReader
		val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded(System.out), true )
		var line: String = null
		
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
				if (!mem.addressable( addr ) || mem.device( addr ))
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
		
		def reference( target: Int, zp: Boolean ) =
			reverseSymbols get target match {
				case None => "$" + (if (zp) hexByte( target ) else hexWord( target ))
				case Some( l ) => l
			}
		
		def disassemble( start: Int, lines: Int ): Int = {
			var addr = start
			
			for (_ <- 1 to lines) {
				if (!mem.addressable( addr ))
					return addr
					
				val opcode = mem.readByte( addr )
				
				CPU.dis6502 get opcode match {
					case None =>
					case Some( (mnemonic, mode) ) =>
						if (mode != 'implicit && mode != 'accumulator && (!mem.addressable( addr + 1 ) || !mem.addressable( addr + 2 )))
							return addr
				}
				
				val label =
					(reverseSymbols get addr match {
						case None => ""
						case Some( l ) => l
					})
					
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

				out.println
			}
			
			addr
		}
		
		def target( ref: String ) =
			if (isHex( ref ))
				hex( ref )
			else
				symbols get (if (ref endsWith ":") ref dropRight 1 else ref) match {
					case None => sys.error( "unknown label: " + ref )
					case Some( t ) => t
				}
				
		out.println( "MOS 6502 emulator v0.3" )
		out.println( "Type 'help' for list of commands." )
		out.println
		
		while ({line = reader.readLine; line != null})
		{
			val com = line.trim.split( "\\s+" )
			
			try
			{
				com.head match {
					case "assemble"|"a" =>
						assemble( com(1) )
						out.println( mem )
					case "breakpoint"|"b" =>
						
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
//						|breakpoint (b) <addr>*        set/clear breakpoint at <addr>
						"""
						|assemble (a) <file>            clear ROM, assemble <file>, and reset CPU
						|assemble (a) <org>             clear ROM, assemble REPL input at <org>, and reset CPU
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
						|reset (re)                     reset CPU registers setting PC from reset vector
						|step (s) [<addr>*]             execute only next instruction at current PC or <addr>
						|save (sa) <file>               save all ROM contents to SREC file
						|symbols (sy)                   print symbol table
						|symbols (sy) <symbol> <val>*   add <symbol> with associated <val>ue to symbol table
						|* can either be a hexadecimal value or label (optionally followed by a colon)
						""".trim.stripMargin.lines foreach out.println
					case "load"|"l" =>
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
			}
			
			out.println
		}
	}
}