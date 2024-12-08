
include log2file.fb
logopen

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

REPORT-ERRORS

logclose
