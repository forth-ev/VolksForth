This is volksForth 3.8 for the Apple 1

It is a very early version. It can be loaded using a hexdump to the
Apple 1 monitor. The code loads and starts to address 0x300.

There is no I/O defined. It should be possible to bootstrap a simple
serial line I/O with Forth blocks using this image.

The sourcecode can be recompiled using the volksForth target
compiler. Currently only the 6502 target compiler for the Atari ST
works. I'm workin on getting the 6502 target compiler working again,
as well as a target compiler in CP/M and MS-DOS volksForth.

Additional information about volksForth can be found at
http://volksforth.sf.net and http://fossil.forth-ev.de/volksforth

If you have any questions about volksForth please contact me at
carsten@strotmann.de

Germany, April 5 2013
Carsten Strotmann
