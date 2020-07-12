

include vf-pr-target.fth

include vf-sys-indep.fth

$7E $93 thru          \ CBM-Interface
(c16+    $94 load )    \ c16init RamIRQ

include vf-finalize.fth

' .blk is .status

include vf-pr-target.fth

cr .( for manual saving:)
cr .( save-target volksforth83)
cr
quit
