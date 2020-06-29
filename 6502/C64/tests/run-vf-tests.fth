
: \vf  [compile] \ ; immediate

include ans-shim.fth

include prelimtest.fth

include tester.fth

\ 1 verbose !

include core.fr

include coreplustest.fth

include utilities.fth
include errorreport.fth

include coreexttest.fth

include doubletest.fth

REPORT-ERRORS
