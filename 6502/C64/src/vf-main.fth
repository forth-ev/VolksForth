

include vf-pr-target.fth

Onlyforth

(C64 $801 ) (C16 $1001 ) dup displace !

Target definitions   here!

include vf-sys-indep.fth

$7E $93 thru          \ CBM-Interface
(c16+    $94 load )    \ c16init RamIRQ

include vf-finalize.fth

Assembler nonrelocate

.unresolved

' .blk is .status

include vf-pr-target.fth

cr .( for manual saving:)
cr .( save-target volksforth83)
cr
quit
