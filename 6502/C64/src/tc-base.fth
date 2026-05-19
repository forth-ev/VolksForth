
\ loadfile for the resident part of the target compiler.

' list alias edit

: .blk  ( -)
 blk @ ?dup IF  ."  Blk " u. ?cr  THEN ;

' .blk is .status

hex

2 drive

\ *** Block No. 16, Hexblock 10

\ Target compiler loadscr      11jul20pz
\ Idea and first Implementation by ks/bp
\ Implemented on 6502  by ks/bp
\ volksFORTH83-Version by bp/we

Onlyforth hex
: (blk@  blk @ ;
Defer blk@  ' (blk@ is blk@

\needs (16   .( ?! (16 (64 ?! C) quit
Assembler \needs nonrelocate include tc6502asm.fth
Assembler nonrelocate

Variable Image      C000 Image !

Vocabulary Ttools
Vocabulary Defining

include tc-main.fth
\ 11 20 thru   \ Target compiler
\ 21 23 thru   \ Target Tools
\ 24 26 thru   \ Redefinitions
clear

