
include vf-cbm-dos.fth
include logtofile.fth
logopen" test.log"

include ans-shim.fth
: \vf  [compile] \ ; immediate

include prelimtest.fth
include tester.fth
\ 1 verbose !
include core.fr
include coreplustest.fth

\ The C16 VolksForth has LIMIT at $8000.
\ More tests than up to here fill the dictionary.

logclose

dos s0:notdone
