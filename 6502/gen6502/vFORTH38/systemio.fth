
\ *** Block No. 0, Hexblock 0

\ System depended IO definitions for 6502 target     cas 26jan06
















\ *** Block No. 1, Hexblock 1

\ loadscreen fuer generic System IO                  cas 26jan06


 1  9 +thru

\\ This example IO definitions are based on serial communication

The definitions needs to be adapted for each system









\ *** Block No. 2, Hexblock 2

\ 65KEY? GETKEY              25JAN85RE)                er14dez88

CODE 65KEY? ( -- FLAG)  $C0EA jsr push0a jmp end-code

CODE GETKEY  ( -- 8B)   $C0A6 jsr push0a jmp end-code

CODE CURON   ( --)  NEXT JMP END-CODE

CODE CUROFF   ( --) NEXT JMP END-CODE

: 65KEY  ( -- 8B)
    CURON BEGIN PAUSE 65KEY?  UNTIL CUROFF GETKEY ;





\ *** Block No. 3, Hexblock 3

\ DECODE EXPECT KEYBOARD      BP28MAY85)
08 CONSTANT #BS   0D CONSTANT #CR  &27 CONSTANT #ESC

: 65DECODE  ( ADDR CNT1 KEY -- ADDR CNT2)
   #BS CASE?  IF  DUP  IF DEL 1- THEN EXIT  THEN
   #CR CASE?  IF  DUP SPAN ! EXIT THEN
   >R  2DUP +  R@ SWAP C!  R> EMIT  1+ ;

: 65EXPECT ( ADDR LEN1 -- )  SPAN !  0
   BEGIN  DUP SPAN @  U<
   WHILE  KEY  DECODE
   REPEAT 2DROP SPACE ;

INPUT: KEYBOARD   [ HERE INPUT ! ]
 65KEY 65KEY? 65DECODE 65EXPECT [


\ *** Block No. 4, Hexblock 4

\ send? (emit 65emit        er14dez88                cas 26jan06

| $8001 Constant aciasr
| $8000 Constant aciaio

| Code send? ( -- flg )
   aciasr lda  pha $08 # and  0= not ?[ $c058 jsr  ]?
               pla $10 # and  push0a jmp  end-code

Code (emit ( 8b -- )  SP X) LDA aciaio sta (drop jmp end-code







\ *** Block No. 5, Hexblock 5

\ EMIT CR DEL PAGE AT AT?     25JAN85RE)             cas 26jan06

| Variable out    0 out !         | &80 Constant c/row

: 65emit   ( 8b -- ) BEGIN pause send? UNTIL 1 out +! (emit ;

: 65CR     #CR 65emit  out @  c/row /  1+  c/row *  out ! ;

: 65DEL    #bs 65emit  SPACE  #bs 65emit -2 out +! ;

: 65PAGE   .( insert code for page )  out off ;

: 65at ( row col -- )
    .( insert code for at ) swap  c/row * + out ! ;
: 65AT?  ( -- ROW COL ) out @  c/row /mod  &24 min swap ;


\ *** Block No. 6, Hexblock 6

\ 65type                                             cas 26jan06

: 65type ( adr len -- ) bounds ?DO I c@ emit LOOP ;














\ *** Block No. 7, Hexblock 7

\ TYPE DISPLAY (BYE       BP  28MAY85RE)               er14dez88

OUTPUT: DISPLAY   [ HERE OUTPUT ! ]
 65EMIT 65CR 65TYPE 65DEL 65PAGE 65AT 65AT? [


| : (bye ;










\ *** Block No. 8, Hexblock 8

\ B/BLK DRIVE >DRIVE DRVINIT  28MAY85RE)             cas 26jan06

$400 CONSTANT B/BLK    \ Bytes per physical Sector

$0AA CONSTANT BLK/DRV  \ number of Blocks per Drive

| VARIABLE (DRV    0 (DRV !

| : DISK ( -- DEV.NO )   (DRV @ 8 + ;

: DRIVE  ( DRV# -- )      BLK/DRV *  OFFSET ! ;






\ *** Block No. 9, Hexblock 9

\                                                    cas 26jan06
: >DRIVE ( BLOCK DRV# -- BLOCK' )
    BLK/DRV * +   OFFSET @ - ;
: DRV?    ( BLOCK -- DRV# )
    OFFSET @ + BLK/DRV / ;

: DRVINIT  NOOP ;
.( for read and write errorhandler is needed )
| : readserial ( adr blk -- )
     &27 emit .( rb ) space base push decimal . cr
     $400 bounds DO key I c! LOOP ;

| : writeserial ( adr blk -- )
     &27 emit .( wb ) space base push decimal . cr
     $400 bounds DO I c@ emit LOOP ;


\ *** Block No. 10, Hexblock a

\  (r/w                                                er14dez88

:  (R/W  ( ADR BLK FILE R/WF -- FLAG)
   swap abort" no file"
   IF readserial ELSE writeserial THEN false ;

' (R/W  IS   R/W









