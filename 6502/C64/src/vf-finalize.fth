
\ *** Block No. 123, Hexblock 7b
7b fthpage

\ The remainder to do after loading the
\ system-dependent part of the sources.

Host  ' Transient 8 + @
  Transient  Forth  Context @ 6 + !
Target

Forth also definitions

(C16 : (64 ) \ jumps belhind C)
(C64 : (16 )
 BEGIN name count dup 0=
 abort" C) missing"  2 = >r
 @ [ Ascii C Ascii ) $100 * + ] Literal
 = r> and UNTIL ; immediate

: C)  ; immediate

(C16 : (16 ) (C64 : (64 ) ; immediate

: forth-83 ;  \ last word in Dictionary



\ *** Block No. 124, Hexblock 7c
7c fthpage

( System dependent Constants      bp/ks)

Vocabulary Assembler
Assembler definitions
Transient  Assembler

PushA  Constant PushA
       \ put A sign-extended on stack
Push0A Constant Push0A
       \ put A on stack
Push   Constant Push
       \ MSB in A and LSB on jsr-stack

RP     Constant RP
UP     Constant UP
SP     Constant SP
IP     Constant IP
N      Constant N
Puta   Constant Puta
W      Constant W
Setup  Constant Setup
Next   Constant Next
xyNext Constant xyNext
(2drop Constant Poptwo
(drop  Constant Pop

\ *** Block No. 125, Hexblock 7d
7d fthpage

\ System patchup              clv06aug87

Forth definitions

(C64  C000 ' limit >body !  7B00 s0 !  7F00 r0 ! )

(C16  8000 ' limit >body !  7700 s0 !  7b00 r0 ! )

\ (C16+ fd00 ' limit >body !
\       7B00 s0 !  7F00 r0 ! )

s0 @ dup s0 2- !      6 + s0 7 - !
here dp !

Host  Tudp @          Target  udp !
Host  Tvoc-link @     Target  voc-link !
Host  move-threads

\ Final part of loadscreen

Assembler nonrelocate

.unresolved
