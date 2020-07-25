
include vf-cbm-dos.fth

include logtofile.fth

logopen" test.log"

: \vf  [compile] \ ; immediate

include ans-shim.fth

include prelimtest.fth

include tester.fth

\ 1 verbose !

include core.fr

include coreplustest.fth

\ The C16 VolksForth has LIMIT at $8000.
\ More tests than up to here fill the dictionary.

(64 include utilities.fth C)
(64 include errorreport.fth C)

(64 include coreexttest.fth C)

(64 include doubletest.fth C)

(64 1 drive C)
(64 include blocktest.fth C)

(64 REPORT-ERRORS C)

logclose

dos s0:notdone
