
: )     ; immediate
: (C    ; immediate

: (C16  ; immediate
: (C16+ ; immediate
: (C64  [compile] ( ; immediate
: (C16- [compile] ( ; immediate

include vf-pr-target.fth

\ C16+ jsr tweak
include vf-c16+jsr.fth
