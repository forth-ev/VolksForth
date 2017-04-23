VolksForth Atari 8-bit Readme

Version 1.1
13. August 2006 (cas)

VolksForth is a 16bit Forth System
produced by the german Forth 
Gesellschaft e.V. Main development 
of this system was done between 1985
until 1989. The VolksForth Project was
revived in 2005 with the goal to 
produce a managable Forthsystem for 
computer systems with restricted system
resources. 

Some modern Forth Systems were influenced
by or were derived from VolksForth
(GNU-Forth, bigForth).

The current Version of VolskForth is 3.81.
Work on Version 3.90 has started.

At this time VolksForth is available 
for this Systems:

VolksForth MS-DOS (Intel x86 
                   Architecture 
                   i8086-ia64)

VolksForth 6502 (Commodore 64, 
                 Commodore Plus 4, 
                 Atari 8bit, Apple I)

VolksForth Z80 (CP/M, 
                Schneider CPC CP/M)

VolksForth 68000 (Atari ST)

VolksForth is in work for this Systems:

VolksForth MS-DOS (Atari Portfolio)
VolksForth 6502 (Apple II)
VolksForth Z80 (Schneider CPC AMSDOS)
VolksForth 68000 (Mac Classic)

Copyright

The VolksForth Sources are made avail-
able under the terms of the BSD License 
http://www.opensource.org/licenses/bsd-license.php

The Handbook is Copyright (c) 
1985 - 2006 Forth Gesellschaft e.V. 
( Klaus Schleisiek, Ulrich Hoffmann, 
Bernd Pennemann, Georg Rehfeld and 
Dietrich Weineck).

The Handbook, binary Files and Source-
code for VolksForth as well as Informa-
tion about Forth Gesellschaft are 
available on the Forth Gesellschaft 
Webserver at http://www.forth-ev.de/

(most of the Information is still 
in german. We are planning to provide 
future versions with full englisch 
documentation)

Information and Help about the Program-
ming Language Forth can be found in the
Internet, starting with the Website of
the Forthgesellschaft, or in the Usenet
Forum de.comp.lang.forth 
(via Google Groups: 
http://groups.google.de/group/de.comp.lang.forth )

Details on VolksForth 6502 
(Atari 8bit)

* Requirements
 Atari 8bit with 48 KB RAM, Floppy 
 Atari 800, 800XL, Atari 130 XE
 600XL (+ Atari 1064), 

* Files 

 This is list of VolksForth Files in
 this Distribution.

 DISK 1:

 DOS.SYS
 DUP.SYS     - Atari DOS 2.5
 VFORTH.COM  - the plain volksForth 
               kernel
 4TH.COM     - volksForth binary
 STAR4TH.COM - volksForth with
               SPARTA/REAL/BEWE
               DOS Extensions
 DEBUG.COM   - volksForth with 
               Debug Tools 
               (Tracer etc)
 CALL.F      - Sourcecode for CALL,
               to call Machine- 
               language Suproutines
 CREATE.F    - Sourcecode for CREATE
 DIR.F       - Sourcecode for DIR and 
               DIR" Commands, to list
               Diskdirectories
 AS65.F      - Sourcecode for the
               resident 6502 Assembler
 TAS65.F     - Sourcecode for the
               transient 6502 Assembler
               (living in HEAP)
 SAVESYS.F   - Sourcecode for
               SAVE-SYSTEM command 
 SIEVE.F     - Primes Sieve Benchmark
 4TH.F       - Build volksForth binary
               from plain kernel
 README.TXT  - This Text
 COPYING     - License Information 

Disk 2
 DOS.SYS
 DUP.SYS     - Atari DOS 2.5
 4TH.COM     - volksForth binary
 COPYING     - License Information 
 DEBUG.F     - Script to build an
               volksForth with 
               DEBUGGER
 TAS65.F     - Sourcecode for the
               transient 6502 Assembler
               (living in HEAP)
 CREATE.F    - Sourcecode for CREATE
 TOOLS.F     - Debugging Tools
 TRACER.F    - Interactive Tracer
 DEBUGT.F    - more Debugging Tools
 SEE.F       - build Decompiler without
               Disassembler
 SEE2.F      - built Decompiler with
               Disassembler
 DISAS.F     - 6502 Disassembler
 DECOMP.F    - volksForth Decompiler
 DIS.F       - build 6502 Disassembler
 MTASK.F     - volksForth Multitasker
 MTDEMO.F    - Multitask Demo
               "RatRace"
 CALL.F      - Sourcecode for CALL,
               to call Machine- 
               language Suproutines
 SPARTA.F    - Sparta/Real/BEWE-DOS
               Support
 LAUNCH.F    - Sparta DOS CLI
               Support
 SPAR4TH.F   - Build volksForth with
               Sparta-DOS Support
 TEST.F
 TEST2.F
 TEST3.F     - Test Files for
               nested INCLUDE" 
 
* Editor
 This verion of volksForth for Atari 
 8bit does not contain an Editor for
 Sourcecode. An Forth Editor will be
 supplied for Version 3.90. We 
 recommend Ken Siders KEDIT
 ( http://atari.ksiders.tzo.com/ )
 or the CompyShop Editor.

* Emulator
 VolksForth 6502 Atari 8bit 3.81 has 
 been tested in the Atari 8bit Emulator
 "Atari800" (atari800.sourceforge.net)

The Forth Live-Linux CD-ROM (available
in the Downloadsection of the Forth
Gesellschaft Website) includes the
current Versions of VolksForth direct
usable without Installation including
the Handbooks as PDF-Files.

Have fun with VolksForth
the VolksForth Team


