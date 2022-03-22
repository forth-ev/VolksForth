
include log2file.fth
logopen output.log

include ans-shim.fth
: \vf  [compile] \ ; immediate

include prelimtest.fth
include tester.fth
\ 1 verbose !
include core.fr
include coreplus.fth

include util.fth
include errorrep.fth

include coreext.fth
include double.fth

REPORT-ERRORS

logclose
