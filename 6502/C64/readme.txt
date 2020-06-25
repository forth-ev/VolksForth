volksFORTH Readme

Version 1.2
18th August 2006 (cas)

volksFORTH is a 16bit Forth System produced by the german Forth Gesellschaft e.V. Major development of this system was done between 1985 until 1989. The volksFORTH Project was revived in 2005 with the goal to produce a manageable Forthsystem for computer systems with restricted system resources.

Some modern Forth Systems were influenced by or were derived from volksFORTH (GNU-Forth, bigForth).

The current Version of VolksForth is 3.81. Work on Version 3.90 has started.

At this time volksFORTH is available for this Systems:

volksFORTH MS-DOS (Intel x86 Architecture i8086-ia64)
volksFORTH 6502 (Commodore 64, Commodore Plus 4, Commodre C16, Atari XL/XE, Apple I)
volksFORTH Z80 (CP/M, Amstrad CPC)
volksFORTH 68000 (Atari ST)

VolksForth is in work for this Systems:

VolksForth MS-DOS (Atari Portfolio)
VolksForth 6502 (Apple II, Commodore PET)
VolksForth Z80 (Schneider CPC AMSDOS)
VolksForth 68000 (Mac Classic)

Copyright

The volksFORTH Sources are made available under the terms of the
BSD License - http://www.opensource.org/licenses/bsd-license.php

The Handbook is Copyright (c) 1985 - 2006 Forth Gesellschaft
e.V. ( Klaus Schleisiek, Ulrich Hoffmann, Bernd Pennemann, Georg Rehfeld
and Dietrich Weineck).

The Handbook, binary Files and Source-code for volksFORTH as well as Information about
Forth Gesellschaft are available on the Forth Gesellschaft Web-server at
http://www.forth-ev.de/

(most of the Information is still in german. We are planning to provide future versions with english documentation)

Information and Help about the Programming Language Forth can be found on the Internet,
starting with the Website of the Forthgesellschaft, or in the Usenet Forum de.comp.lang.forth (via Google Groups: http://groups.google.de/group/de.comp.lang.forth )

Authors of volksForth/ultraForth are

   - Bernd Pennemann,
   - Claus Vogt,
   - Dietrich Weineck,
   - Georg Rehfeld,
   - Klaus Schleisieck,
   - Ulrich Hoffmann,
   - Ewald Rieger,
   - Carsten Strotmann.

Details for volksFORTH C64/C16/Plus4

* System Prerequisites
   C=64                              with Floppy Drive
   Plus4                             with Floppy Drive
   C16 or C116 with 32kB or 64kB     with Floppy Drive
   C16 or C116 with 64kB or Plus4    with Cassetterecorder

The original C16 with 16Kb cannot run volksForth.

We recommend a Floppy Drive.


* Installation
  These Disk-Image files can be found in the Distribution:

  vforth_1.d64 - Diskimage Disk 1 for Emulator usage
                 C64 volksForth Binary
                 C16 volksForth Binary
                 Sourcecode
  vforth_2.d64 - Diskimage Disk 2 for Emulator usage
                 complete volksForth 6502 C64/C16 Sourcecode
                 (can be compiled with Target Compiler)
  vforth_3.d64 - Diskimage Disk 3 for Emulator usage
                 Assembler, Disassembler, Editor
  vforth_4.d64 - Diskimage Disk 4 for Emulator usage
                 Grafic, Demos, Tools, Decompiler
  tc38q.d64    - Sourcecode and Manual for Target Compiler
  file-words.d64 - Some words to facilitate the pending transition
                 from direct disk/d64 access blocks/screens to
                 stream files as the primary sources from which
                 C64 VolksForth is built.

  This version of volksForth is still using a traditional Forth-Block
  filesystem on the Disks. The upcoming version 3.90 will have a ANSI-FORTH
  compatible File-Interface to access normal Files on Disk. The Forth-Block
  routines will still be available as an optional package.

* Using volksFORTH in an Emulator
  * volksForth 3.81 6502 for C=64, C16 and Plus4 has been tested in these
    Emulators:
    * VICE  - http://www.viceteam.org/
    * Frodo - http://frodo.cebix.net/
    (without the good work of the VICE people we wouldn't be able to re-relase
     this software)

* Automated build using VICE:
  The Makefile and the build scripts in the emulator/ subdir require
  the file format conversion tools from the tools/ subdir of
  https://github.com/pzembrod/cc64 to be installed.

* Website:
  VolksForth is available on SourceForge
  http://volksForth.sf.net
  and on the Website of Forth Gesellschaft (German Chapter of Forth Interst Group, FIG)
  http://www.forth-ev.de

The Forth Live-Linux CD-ROM (available in the Download-section of the Forth Gesellschaft Website) includes the current Versions of volksFORTH direct usable without Installation including the Handbooks as PDF-Files.

Have fun with volksFORTH
the volksFORTH Team

