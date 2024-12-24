
include log2file.fs
logopen output.log

include ans-shim.fs
: \vf  [compile] \ ; immediate

include prelim.fs
include tester.fs

\ 1 verbose !
include core.fs

logclose
