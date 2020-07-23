
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

: (C16  ; immediate
: (C16- ; immediate
: (C64  [compile] ( ; immediate
: (C16+ [compile] ( ; immediate
\ ) - just to unconfuse my editor
include vf-pr-target.fth

\ The actual volksForth sources
include vf-head-c16.fth
include vf-cbm-core.fth
include vf-sys-c16.fth
include vf-cbm-file.fth
include vf-finalize.fth

include vf-pr-target.fth
quit
