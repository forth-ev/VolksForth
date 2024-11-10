
include log2file.fb
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

REPORT-ERRORS

logclose
