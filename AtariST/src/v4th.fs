
\ *** Block No. 1, Hexblock 1

\ Atari 520 ST    Forth loadscreen                     cas201301
\ volksFORTH-83 was developed by K. Schleisiek, B. Pennemann
\ G. Rehfeld & D. Weineck
\ Atari ST - Version by D. Weineck
\ Atari ST/STE/TT/Falcon/FireBee Version by C. Strotmann

Onlyforth

        0 dup displace !
Target definitions here!

use forth83.fb

   $83 load
 2 $75 thru

Code restart      here >restart !
   ' (restart >body FP D) IP lea   bootsystem bra   end-code

$78 $82 +thru        \ Atari 520 ST Interface
include tfileint.fs

   
Host    ' Transient 8 + @  Transient Forth context @ 6 + !
\ Tlatest aus Transient wird Tlatest in Forth

Target Forth also definitions
: forth-83 ;     \ last word in Dictionary

   $77 load

.( before unresolve) cr
cr .unresolved  ' .blk is .status
.( after unresolve) cr
