
include log2file.fb  \ so that include with block file gets tested
' noop Is .status
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
include doubltst.fs

include block.fs

REPORT-ERRORS

logclose

