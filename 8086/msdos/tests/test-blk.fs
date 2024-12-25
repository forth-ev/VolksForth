
include log2file.fs
logopen output.log

include ans-shim.fs
: \vf  [compile] \ ; immediate

include prelim.fs
include tester.fs
\ 1 verbose !
include core.fs
include coreplus.fs

include util.fs
include errorrep.fs

include coreext.fs
include doubltst.fs

: flush  logclose flush logreopen ;
include block.fs

REPORT-ERRORS

logclose

