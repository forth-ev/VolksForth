
include log2file.fb  \ so that include with block file gets tested
' noop Is .status
logopen

include ans-shim.fth
: \vf  [compile] \ ; immediate

include prelim.fth
include tester.fth
\ 1 verbose !
include core.fr
include coreplus.fth

include util.fth
include errorrep.fth

include coreext.fth
include doubltst.fth

include block.fth

REPORT-ERRORS

logclose

