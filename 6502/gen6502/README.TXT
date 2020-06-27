VolksForth (generic 6502 Version) Readme

Version 1.0
26. January 2006 (cas)

VolksForth is a 16bit Forth System produced by the german Forth Gesellschaft
e.V. Main development of this system was done between 1985 until 1989. The
VolksForth Project was revived in 2005 with the goal to produce a managable
Forthsystem for computer systems with restricted system resources. 

Some modern Forth Systems were influenced by or were derived from VolksForth
(GNU-Forth, bigForth).

The current Version of VolskForth is 3.81. Work on Version 3.90 has started.

At this time VolksForth is available for this Systems:

VolksForth MS-DOS (Intel x86 Architecture i8086-ia64)
VolksForth 6502 (Commodore 64, Commodore Plus 4, generic, Atari XL, Apple 1)
VolksForth Z80 (CP/M, Schneider CPC)
VolksForth 68000 (Atari ST)

Copyright

The VolksForth Sources are made available under the terms of the
BSD Lizenz - http://www.opensource.org/licenses/bsd-license.php

The Handbook is Copyright (c) 1985 - 2005 Forth Gesellschaft
e.V. ( Klaus Schleisiek, Ulrich Hoffmann, Bernd Pennemann, Georg Rehfeld
and Dietrich Weineck).

The Handbook, binary Files and Sourcecode for VolksForth as well as Information
about Forth Gesellschaft are available on the Forth Gesellschaft Webserver at
http://www.forth-ev.de/

(most of the Information is still in german. We are planning to provide future
versions with englisch documentation)

Information and Help about the Programming Language Forth can be found in the
Internet, starting with the Website of the Forthgesellschaft, or in the Usenet Forum
de.comp.lang.forth (via Google Groups: http://groups.google.de/group/de.comp.lang.forth )

Details on VolksForth 6502 (generic)

* Requirements
  the source is for a generic 6502 System and needs to be adjusted to run on real headware

* Files 

  This is list of VolksForth Files in this Distribution. 

  2words.fb          - additional words to handle 32bit values
  4th.prg            - Atari ST volksFORTH 3.81 (needed for Crosscompilation)
  6502f83.fb         - 6502 volksFORTH Kernel Source
  as65.fb            - 6502 Forth Assembler (resident and transident version)
  assemble.fb        - volksFORTH M68000 Assembler 
  ccompile.fb        - Crosscompile Script
  crostarg.fb        - Target Crosscompiler to create a new 6502 Forth System
  ediicon.rsc        - GEM Resource File needed for Atari ST volksFORTH
  systemio.fb        - Systemdependent IO definitions
  tasker.fb          - Multitask Extensions for volksFORTH
  tools.fb           - Development Tools (Debugger, Tracer)

* How to crosscompile a new 6502 volksFORTH binary?

** Requirements: Crosscompilation to a 6502 Target is currently only possible from Atari ST volksFORTH. We
   are working on getting Crosscompilation working again on 6502 (native Metacompilation) and on other
   volksFORTH Platforms (like MS-DOS). So you need a read Atari ST or an Atari ST Emulator.

** Emulator
   VolksForth Atari ST 3.81 has been tested in the Atari ST Emulator "HATARI"
   (hatari.sourceforge.net) with EmuTos (emutos.sourceforge.net). VolksForth 3.81 has
   some display problems with screen resolutions above 640x480. We try to solve this in
   later versions.

  1) boot the Atari ST or Atari ST Emulator
  2) start volksFORTH Atari ST (4th.prg) from GEM or EMUTOS
  3) customize the sourcecode in 6502F83.FB and SYSTEMIO.FB (see instructions below)
  4) configure FORTH to use only one IO-Diskbuffer (important!)
  5) start crosscompilations process with "include ccompile.fb"
  6) you should see a new file "f6502.com" in the directory

* How to customize the 6502 volksFORTH Sourcecode for a new target system?

  1) open the volksFORTH kernel source "use 6502F83.FB"
  2) change the constant "BASEADDR" in screen 1 (target memory address)
  3) change the bootlabel in Screen 3
  4) adjust the zero page locations in Screen 4 (default ZP $20 to $47)
  5) adjust memory locations for data stack (S0, default $5000) 
     and return Stack (R0, default $550) in Screen 8
  6) (optional) adjust memory location for Terminal Input Buffer (>TIB, default $100) 
     if needed in Screen 54
  7) adjust Boot Message in "(COLD" in Screen 121 (optional)
  8) adjust memory for data stack and return stack, and disk buffers (LIMIT) in 
     Screen 127
  9) close file 6502F83.FB
 10) open file SYSTEMIO.FB "use SYSTEMIO.FB"
 11) change Definition for 65KEY?. This definition should leave TRUE on the Stack
     (TOS <> 0) if a Keyevent is waiting. This Word should never block (multitasking!)
 12) change Definition for GETKEY to place the ASCII value of last keypress on the Stack.
     This Word might block.
 13) change definitions CURON and CUROFF to display Cursor or hide Cursor. If this functions
     are not available, make these Words a NOOP.
 14) change constants for #BS (ASCII Backspace) #CR (ASCII Carriage Return) and 
     #ESC (ASCII Escape Key)
 15) change definition (EMIT to print the ASCII Char from the top of Stack to the Screen
 16) change definition for 65PAGE to clear screen and move cursor to home position
 17) change definition for 65AT to move cursor to the row and column values on the stack
 18) change definition for 65AT? to query the row and column values for the cursor and
     place them on the stack
 19) change constants B/BLK (Bytes per Block/physical Sector) and BLK/DRV (Number of Blocks
     per Driver, each Block is 1024 Bytes)
 20) implement a R/W Definition for your Disk Drive / Storage System
 21) compile (see above) and test
 22) if successfull, please report back to the project team and consider releasing your
     code to the public.

Have fun with VolksForth
the VolksForth Team


