
\ include logfile.fs
logopen

include ans-shim.fs .( hello tib) cr
: \vf  [compile] \ ; immediate

include prelim.fs
include tester.fs

\ 1 verbose !
include core.fs

logclose
