

include vf-pr-target.fth

Onlyforth

(C64 $801 ) (C16 $1001 ) dup displace !

Target definitions   here!

include vf-blk-10-7d.fth

Assembler nonrelocate

.unresolved

' .blk is .status

include vf-pr-target.fth

cr .( for manual saving:)
cr .( save-target volksforth83)
cr
quit
