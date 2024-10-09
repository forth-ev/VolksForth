\ *** Block No. 116, Hexblock 74

\ Rest of Standard-System                      04Oct87   07Oct87

\ 2 +load  \ Operating System

Host    ' Transient 8 + @  Transient Forth Context @ 6 + !

Target Forth also definitions

Vocabulary Assembler  Assembler definitions
Transient Assembler
>Next Constant >Next
hpush Constant hpush
dpush Constant dpush

Target Forth also definitions
: forth-83 ;     \ last word in Dictionary

\ *** Block No. 117, Hexblock 75

\ System patchup                                         04Oct87

$EF00  r0 !
$EB00  s0 !
s0 @ 6 +  origin 2+ !  \ link Maintask to itself

\ s0 und r0 werden beim Booten neu an die Speichergroesse
\ angepasst. Ebenso der Multi-Tasker-Link auf die Maintask

here dp !

Host  Tudp @       Target  udp !
Host  Tvoc-link @  Target  voc-link !
Host move-threads

