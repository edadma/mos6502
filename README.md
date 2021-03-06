MOS 6502 Microprocessor Emulator
================================

Have fun playing with your own virtual 6502 based computer!


Description
-----------

*mos6502* is an emulator for the venerable MOS Technology 6502 microprocessor written in Scala. The emulator is fully functional. It includes a basic assembler and disassembler. Programs (in 6502) can be executed from the command line, or the REPL can be used. There are a few working examples under the `code` folder that can either be executed directly from the command line or loaded into the REPL.

It's true, there are many 6502 emulators out there, but this one is written in Scala and is therefore easy and fun to extend should you wish to add some more hardware to your virtual computer.

All the examples in the `code` folder with a file name ending with `.asm` will assemble using the built-in assembler. Of course, the emulator's built-in assembler will have slightly different syntax than other assemblers for assembly directives. If a more powerful assembler is needed, there is one called `crasm` which is available under Ubuntu/Mint that generates SREC (Motorola S-Record) files using it's `-o` option. Those files can then be executed using the emlulator's `-le` option or loaded into the REPL using the `l` command. The `cc65` tools may also be used in conjunction with a program called `srecord` to get an SREC file that can be loaded.


Try it out
----------

The following code (assembled using the built-in assembler)

	; install required i/o devices
	;
	_stdioChar_ = "8000" ; add a character i/o "device" and memory map it to address $8000
	_stdioInt_  = "8001" ; add an integer i/o "device" and memory map it to address $8001

	; memory mapped i/o
	;
	CHIO    EQU $8000    ; character i/o port
	INTIO   EQU $8001    ; integer i/o port

	; zero page variables
	;
	COUNTER RB           ; counter variable

	; program ROM
	;
	        ORG $9000    ; program starts at $9000
	START
	        LDA #0       ; start counter off with 0
	        STA COUNTER
	.1      INC COUNTER  ; bump the counter
	        LDA COUNTER
	        CMP #6       ; is counter less than 6
	        BNE .2       ; if so, print
	        BRK          ; otherwise, stop
	.2      STA INTIO    ; send counter value to integer i/o port
	        LDA #'\n'    ; now print a line feed
	        STA CHIO
	        JMP .1

	        ORG $FFFC    ; reset vector
	        DW  START    ; CPU will start executing at 9000

will print

	1
	2
	3
	4
	5

The easiest way to try this out is to follow the [build](https://github.com/edadma/mos6502#build-and-run-the-repl) instructions below. Then, once inside the REPL, type

	a code/count.asm
	ew

Of course, when doing this in the REPL you will also see

	A:06 X:00 Y:00 SP:01FD PC:900D
	N:0 V:0 B:0 D:0 I:0 Z:1 C:1
	900D  8D 01 80   .2              STA INTIO

as well, showing you the state of the CPU after execution.

If you now type the command `u 9000` to disassemble the program from memory, you should see

	9000  A9 00      START           LDA #0
	9002  85 00                      STA COUNTER
	9004  E6 00      .1              INC COUNTER
	9006  A5 00                      LDA COUNTER
	9008  C9 06                      CMP #6
	900A  D0 01                      BNE .2
	900C  00                         BRK 
	900D  8D 01 80   .2              STA INTIO
	9010  A9 0A                      LDA #$0A
	9012  8D 00 80                   STA CHIO
	9015  4C 04 90                   JMP .1

Notice that the REPL uses the symbol information (if available) from the assembler to provide a more readable disassembly.


License
-------

*mos6502* is distributed under the MIT License, meaning that you are free to use it in your free or proprietary software.


Documentation
-------------

- [Scaladoc library documentation](http://edadma.github.io/mos6502)
- Type `java -jar mos6502-0.3.jar --help` for executable options
- Type `help` inside the REPL for commands

Usage
-----

### Executable

If you just want to download the executable so that you can run 6502 programs or use the REPL, you can download it from [here](https://dl.bintray.com/edadma/generic/mos6502-0.3.jar). *You do not need* the Scala library for it to work because the JAR already contains all dependencies. You just need Java 7+ installed.

Run it as a normal Java executable JAR with the command `java -jar mos6502-0.3.jar` in the folder where you downloaded the file.

### Library

Use the following definition to use *mos6502* in your Maven project:

	<repository>
		<id>hyperreal</id>
		<url>https://dl.bintray.com/edadma/maven</url>
	</repository>

	<dependency>
		<groupId>xyz.hyperreal</groupId>
		<artifactId>mos6502</artifactId>
		<version>0.3</version>
	</dependency>

Add the following to your `build.sbt` file to use *mos6502* in your SBT project:

	resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

	libraryDependencies += "xyz.hyperreal" %% "mos6502" % "0.3"


Building
--------

### Requirements

- Java 7+
- SBT 0.13.12+
- Scala 2.11.8+

### Build and Run the REPL

	git clone git://github.com/edadma/mos6502.git
	cd mos6502
	sbt run