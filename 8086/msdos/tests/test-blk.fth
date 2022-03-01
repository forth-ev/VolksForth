
include log2file.fth
logopen test.log

include ans-shim.fth
: \vf  [compile] \ ; immediate

include prelimtest.fth
include tester.fth
\ 1 verbose !
include core.fr
include coreplustest.fth

include utilities.fth
include errorreport.fth

include coreexttest.fth
include doubletest.fth
1 drive include blocktest.fth

REPORT-ERRORS

logclose

dos s0:notdone