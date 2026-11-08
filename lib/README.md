# VolksForth Library

This directory aims to be a home for sources for VolksForth that aren't
directly or indirectly part of one of the different platform kernels,
but have been written either for one specific VolksForth platform,
like the terminal program Term4 for MSDOS VolksForth, or a mouse driver
for the 1351 mouse under C64 VolksForth, or for VolksForth in general,
like an Eratostenes sieve benchmark.

The initial plan is to organize this by platform. We'll see how well
this works and how usable and browsable it all turns out to be.

## MSDOS

### Term4

Written by Claus Vogt and described in the
[2026-03 issue](https://forth-ev.de/wiki/res/lib/exe/fetch.php/vd-archiv:4d2026-03.pdf)
of the German
[Vierte Dimension periodical](https://forth-ev.de/wiki/vd-archiv),
this is a tool for driving the
serial interface of a PC, directly on the 8250 chip level. It's not strictly
a terminal program, as it doesn't handle VT100 or ANSI escape sequences.
Its origin is in the space of device automation - think lab and industry.
As yet its documentation and UI are in German.
