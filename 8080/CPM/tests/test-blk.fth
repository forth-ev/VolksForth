
include log2file.fth
logopen output.log

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

: flush  logclose flush logreopen ;
include block.fth

REPORT-ERRORS

logclose

