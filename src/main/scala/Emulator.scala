package xyz.hyperreal.mos6502

import java.io.File

import collection.mutable.HashMap


class Emulator( chip: String ) extends Flags {
	
	val mem =
		new Memory {
			def init {
				removeDevices
				regions.clear
				add( new RAM("main", 0x0000, 0x7FFF) )
				add( new ROM("program", 0x9000, 0xFFFF) )
			}
		}
	val cpu =
		chip match {
			case "6502" => new CPU6502( mem )
		}
	private val registry = new HashMap[String, (String, Memory, CPU) => Unit]
	
	register( "_stdioChar_", (p: String, mem: Memory, cpu: CPU) => mem add new StdIOChar( hex(p) ) )
	register( "_stdioInt_", (p: String, mem: Memory, cpu: CPU) => mem add new StdIOInt( hex(p) ) )
	register( "_stdioHex_", (p: String, mem: Memory, cpu: CPU) => mem add new StdIOHex( hex(p) ) )
	register( "_rng_", (p: String, mem: Memory, cpu: CPU) => mem add new RNG( hex(p) ) )
	register( "_video_",
		(p: String, mem: Memory, cpu: CPU) => {
			val parms = p split ","
			val kp = hex(parms(1))
			val kpd = if (kp == 0) null else new KeyPress( kp )
			
			if (kpd ne null)
				mem add kpd
				
			mem add new VideoRAM( hex(parms(0)), kpd, hex(parms(2)), hex(parms(3)), hex(parms(4)), cpu, (for (i <- 5 to 20) yield hex(parms(i))).toIndexedSeq )
		} )
	register( "_ram_",
		(p: String, mem: Memory, cpu: CPU) => {
			mem.removeRAM
			
			val block = """(\p{XDigit}+)\-(\p{XDigit}+)"""r
			
			for ((m, ind) <- block findAllMatchIn p zipWithIndex)
				mem add new RAM( "main" + ind, hex(m group 1), hex(m group 2) )
		}	)
	register( "_rom_",
		(p: String, mem: Memory, cpu: CPU) => {
			mem.removeROM
			
			val block = """(\p{XDigit}+)\-(\p{XDigit}+)"""r
			
			for ((m, ind) <- block findAllMatchIn p zipWithIndex)
				mem add new ROM( "main" + ind, hex(m group 1), hex(m group 2) )
		}	)
	
	var dumpcur = 0
	var discur = 0
	var symbols = Map[String, Any]()
	var reverseSymbols = Map[Any, String]()
	
	def register( name: String, installer: (String, Memory, CPU) => Unit ) {
		if (registry contains name)
			sys.error( "device installer already registered: " + name )
			
		registry(name) = installer
	}
	
	def degister( name: String ) {
		if (!(registry contains name))
			sys.error( "device installer not registered: " + name )
			
		registry -= name
	}
	
	def reregister( name: String, installer: (String, Memory, CPU) => Unit ) {
		if (!(registry contains name))
			sys.error( "device installer not registered: " + name )
			
		registry(name) = installer
	}
	
	def assemble( src: io.Source ) {
		if (cpu.isRunning)
			sys.error( "can't load while running" )
			
		mem.init
		
		val AssemblerResult(syms, segments) = Assembler( src )
		println( segments )
				
		symbols = syms
		reverseSymbols = symbols map {case (s, t) => (t, s)}
		discur = mem.code
		
		for ((k, v) <- symbols)
			registry get k match {
				case None =>
				case Some( installer: ((String, Memory, CPU) => Unit) ) => installer( v.asInstanceOf[String], mem, cpu )
			}
		
		for ((name, base, data) <- segments)
			for (i <- 0 until data.length)
				mem.program( base + i, data(i) )
			
		clearBreakpoints
		reset
	}

	def run = cpu.run
	
	def reset = {
		cpu.reset
		discur = mem.code
	}
	
	def step = cpu.step
	
	def stop = cpu.stop
	
	def readByte( addr: Int ) = mem.readByte( addr )
	
	def readWord( addr: Int ) = mem.readWord( addr )
	
	def program( addr: Int, b: Int ) = mem.program( addr, b )

	def registers =
		String.format( "A:%s X:%s Y:%s SP:%s PC:%s\n", hexByte(cpu.A), hexByte(cpu.X), hexByte(cpu.Y), hexWord(cpu.SP), hexWord(cpu.PC) ) +
		String.format( "N:%s V:%s B:%s D:%s I:%s Z:%s C:%s\n", Seq(N, V, B, D, I, Z, C) map (cpu.read(_).toString): _* ) +
		disassemble( cpu.PC, 1 )
		
	def display( label: String ) =
		label indexOf '.' match {
			case -1 => label
			case dot => label substring dot
		}
	
	def reference( target: Int, zp: Boolean ) =
		reverseSymbols get target match {
			case None => "$" + (if (zp) hexByte( target ) else hexWord( target ))
			case Some( l ) => display( l )
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
	
	def disassemble( start: Int, lines: Int ): String = {
		val buf = new StringBuilder
		var addr =
			if (start == -1)
				discur
			else
				start
		
		for (_ <- 1 to lines) {
			if (!mem.memory( addr ))
				return buf.toString
				
			val opcode = cpu.readByte( addr )
			
			CPU.dis6502 get opcode match {
				case None =>
				case Some( (mnemonic, mode) ) =>
					if (mode != 'implicit && mode != 'accumulator && (!mem.memory( addr + 1 ) || !mem.memory( addr + 2 )))
						return buf.toString
			}
			
			val label =
				(reverseSymbols get addr match {
					case None => ""
					case Some( l ) => display( l )
				})
				
			if (cpu.breakpoints( addr ))
				buf append Console.BLUE_B
				
			buf append( hexWord(addr) + "  " + hexByte(opcode) + " " )
			addr += 1
			
			CPU.dis6502 get opcode match {
				case None => buf append( " "*(6 + 2 + 15 + 1) + "---" )
				case Some( (mnemonic, mode) ) =>
					val (display, size) =
						(mode match {
							case 'implicit => ("", 0)
							case 'accumulator => ("A", 0)
							case 'immediate => ("#" + "$" + hexByte(cpu.readByte(addr)), 1)
							case 'relative => (reference(cpu.readByte(addr).toByte + addr + 1, false), 1)
							case 'indirectX => ("(" + reference(cpu.readByte(addr), true) + ",X)", 1)
							case 'indirectY => ("(" + reference(cpu.readByte(addr), true) + "),Y", 1)
							case 'zeroPage => (reference(cpu.readByte(addr), true), 1)
							case 'zeroPageIndexedX => (reference(cpu.readByte(addr), true) + ",X", 1)
							case 'zeroPageIndexedY => (reference(cpu.readByte(addr), true) + ",Y", 1)
							case 'direct => (reference(cpu.readWord(addr), false), 2)
							case 'directX => (reference(cpu.readWord(addr), false) + ",X", 2)
							case 'directY => (reference(cpu.readWord(addr), false) + ",Y", 2)
							case 'indirect => ("(" + reference(cpu.readWord(addr), false) + ")", 2)
						})
					
					for (i <- 0 until size)
						buf append( hexByte(cpu.readByte(addr + i)) + " " )
						
					addr += size
					
					buf append( " "*((2 - size)*3 + 2) )
					buf append( label + " "*(15 - label.length + 1) )
					buf append( mnemonic.toUpperCase + " " )
					buf append( display )
			}

			buf append( Console.RESET )
			buf += '\n'
		}
		
		discur = addr
		buf.toString dropRight 1
	}
		
	def load( file: String ) {
		if (cpu.isRunning)
			sys.error( "can't load while running" )
			
		mem.init
		SREC( mem, new File(file) )
		discur = mem.code
		clearBreakpoints
		reset
	}

	def save( file: String ) = 	SREC.write( mem, new File(file), file.getBytes.toVector )
	
	def dump( start: Int, lines: Int ) = {
		val buf = new StringBuilder
		val addr =
			if (start == -1)
				dumpcur - dumpcur%16
			else
				start - start%16
		
		def printByte( b: Option[Int] ) =
			if (b == None)
				buf.append( "-- " )
			else
				buf.append( "%02x ".format(b.get&0xFF).toUpperCase )
		
		def printChar( c: Option[Int] ) = buf.append( if (c != None && ' ' <= c.get && c.get <= '~') c.get.asInstanceOf[Char] else '.' )
		
		def read( addr: Int ) =
			if (mem.addressable( addr ) && mem.memory( addr ))
				Some( mem.readByte(addr) )
			else
				None
		
		for (line <- addr until ((addr + 16*lines) min 0x10000) by 16) {
			buf.append( "%4x  ".format(line).toUpperCase )
			
			for (i <- line until ((line + 16) min 0x10000)) {
				if (i%16 == 8)
					buf.append( ' ' )
					
				printByte( read(i) )
			}
			
			val bytes = ((line + 16) min 0x10000) - line
			
			buf.append( " "*((16 - bytes)*3 + 1 + (if (bytes < 9) 1 else 0)) )
			
			for (i <- line until ((line + 16) min 0x10000))
				printChar( read(i) )
				
			buf += '\n'
		}
		
		dumpcur = (addr + 16*8) min 0x10000
		buf.toString dropRight 1
	}
	
	def clearBreakpoints = cpu.breakpoints = Set[Int]()
	
	def setBreakpoint( addr: Int ) = cpu.breakpoints += addr
	
	def clearBreakpoint( addr: Int ) = cpu.breakpoints -= addr
	
	def breakpoints = cpu.breakpoints.toList map (b => (b, (if (reverseSymbols contains b) reverseSymbols(b) else "")))
}