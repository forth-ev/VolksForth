
include makefile.fs
include log2file.fs
logopen

include ans-shim.fs
: \vf  [compile] \ ; immediate

\ \atari marks tests that fail with VolksForth 3.85.2
: \atari  [compile] \ ; immediate

include prelim.fs
include tester.fs

\ 1 verbose !
include core.fs

logclose
