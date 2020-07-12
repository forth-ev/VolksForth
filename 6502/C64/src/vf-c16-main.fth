
hex

\ load transient part of target compiler
2 drive 27 30 thru

1 drive

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
: (C16+ ; immediate
: (C64  [compile] ( ; immediate
: (C16- [compile] ( ; immediate
\ ) - just to unconfuse my editor
include vf-pr-target.fth

\ The actual volksForth sources
\ including some initial C16 tweaks

Assembler also definitions
\needs C16+Jsr          8 load
' C16+Jsr Is Jsr

include vf-sys-indep.fth
$7E $93 thru          \ CBM-Interface
(c16+    $94 load )    \ c16init RamIRQ
include vf-finalize.fth

include vf-pr-target.fth
quit
