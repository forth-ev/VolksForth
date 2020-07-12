
hex

\ load transient part of target compiler
2 drive 27 30 thru


Onlyforth hex

\ clear memory and clr labels  .status
include vf-tc-prep.fth

\ Host and target settings and display
cr .( Host is: )
    (64  .( C64) C)
    (16  .( C16) C)

: )     ; immediate
: (C    ; immediate

: (C64  ; immediate
: (C16  [compile] ( ; immediate
: (C16+ [compile] ( ; immediate
: (C16- [compile] ( ; immediate
\ ) - just to unconfuse my editor
include vf-pr-target.fth

\ The actual volksForth sources
include vf-sys-indep.fth
include vf-sys-c64.fth
include vf-finalize.fth

include vf-pr-target.fth
quit
