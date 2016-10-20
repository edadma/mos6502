MOS 6502 Microprocessor Emulator
================================

Have fun building your own virtual 6502 based computer.


Description
-----------

*mos6502* is an emulator for the venerable MOS Technology 6502 microprocessor written in Scala. The emulator is fully functional. It includes a basic assembler and disassembler. Programs (in 6502) can be executed from the command line, or the REPL can be used. There are a few working examples under the `code` folder that can either be executed directly or loaded into the REPL.

All the examples with a file name ending with `.asm` will assemble using the built-in assembler. Of course, the emulator's built-in assembler will have slightly different syntax than other assemblers for assembly directives. If a more powerful assembler is needed, there is one called `crasm` which is available under Ubuntu/Mint that generates SREC (Motorola S-Record) files using it's `-o` option. Those files can then be executed using the emlulator's `-le` option or loaded into the REPL using the `l` command. The `cc65` tools may also be used in conjunction with a program called `srecords` to get an SREC file that can be loaded.

### Example

The following code (assembled using built-in assembler)

    COUNTER RB          ; zero page counter variable

            ORG $8000   ; memory mapped i/o 

    COUT    RB          ; character i/o port
    IOUT    RB          ; integer i/o port

            ORG $9000   ; ROM

            LDA #0      ; start counter off with 0
            STA COUNTER
    LOOP    INC COUNTER ; bump the counter
            LDA COUNTER
            CMP #6      ; is counter less than 6
            BNE PRINT   ; if so, print
            BRK         ; otherwise, stop
    PRINT   STA IOUT    ; send counter value to integer i/o port
            LDA #'\n'   ; now print a line feed
            STA COUT
            JMP LOOP

            ORG $FFFC   ; reset vector
            DW  $9000   ; CPU will start executing at 9000

will print

	1
	2
	3
	4
	5

The easiest way to try this out is to follow the build instructions below

License
-------

*mos6502* is distributed under the MIT License, meaning that you are free to use it in your free or proprietary software.


Documentation
-------------

Scaladoc library documentation can be found at http://edadma.github.io/mos6502.


Usage
-----
	
### Executable

If you just want to download the executable so that you can run 6502 programs or use the REPL, you can download it from [here]. *You do not need* the Scala library for it to work because the JAR already contains all dependencies. You just need Java 7+ installed.

Run it as a normal Java executable JAR with the command `java -jar [emulator].jar` in the folder where you downloaded the file.

### Library

Use the following definition to use *mos6502* in your Maven project:

	<repository>
		<id>hyperreal</id>
		<url>https://dl.bintray.com/edadma/maven</url>
	</repository>

	<dependency>
		<groupId>xyz.hyperreal</groupId>
		<artifactId>mos6502</artifactId>
		<version>0.1</version>
	</dependency>

Add the following to your `build.sbt` file to use *mos6502* in your SBT project:

	resolvers += "Hyperreal Repository" at "https://dl.bintray.com/edadma/maven"

	libraryDependencies += "xyz.hyperreal" %% "mos6502" % "0.1"


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