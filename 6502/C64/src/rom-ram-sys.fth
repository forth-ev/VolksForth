\ *** Block No. 2, Hexblock 2

\ rom ram sys                 cas16aug06
\              Shadow with Ctrl+W--->

\ needed for jumps
\ in the ROM Area

Assembler also definitions
(16 \ Switch Bank 8000-FFFF
: rom here 9 + $8000 u> abort" not here"
       $ff3e sta ;
: ram  $ff3f sta ;
: sys rom jsr ram ;
\  if suffering from abort" not here"
\  see next screen Screen --> C)


(64 \ Switch Bank A000-BFFF
: rom here 9 + $A000 u> abort" not here"
      $37 # lda 1 sta ;
: ram $36 # lda 1 sta ;
C)





\ *** Block No. 3, Hexblock 3

\ sysMacro Long               cas16aug06

(64  .( not for C64 !) \\ C)

\ for advanced users, use macros

here $8000 $20 - u> ?exit \ not possible


' 0 | Alias ???

Label long   ROM
Label long1  ??? jsr  RAM  rts end-code

| : sysMacro ( adr -- )
 $100 u/mod  pha  # lda  long1 2+ sta
 # lda  long1 1+ sta  pla  long jsr ;

: sys ( adr -- ) \ for Jsr to ROM
 here 9 + $8000 u>
 IF  sysMacro  ELSE  sys  THEN ;
