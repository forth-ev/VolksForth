
\ *** Block No. 0, Hexblock 0

\ System depended IO definitions for 6502 target     cas 26jan06
















\ *** Block No. 1, Hexblock 1

\ loadscreen for py65                            CAS  06JULY2024


 1  9 +thru

\\ The following IO definitions are for the neo6502 board
   at https://github.com/TheByteAttic/CERBERUS2100

   A char can can be read by memory mapped IO at $0201
   Chars must be written to the screenmem at $FB00
   The VolksForth needs to implement its own screen driver






\ *** Block No. 2, Hexblock 2

\ 65KEY? GETKEY              25JAN85RE)         cas   06july2024

CODE 65KEY? ( -- FLAG)  $0200 lda  push0a jmp  end-code

CODE GETKEY  ( -- 8B)   $0 # lda   $0200 sta
                        $0201 lda
                        push0a jmp  end-code
| VARIABLE CURMSK     $80 CURMSK !
: CURON   ( --) $80 CURMSK ! ;
: CUROFF  ( --) $00 CURMSK ! ;
: SCRADR ( -- addr ) SDRY @ &40 * SDRX @ + $F800 + ;
: 65KEY  ( -- 8B)
          scradr c@ curmsk @ xor scradr c!
          BEGIN PAUSE 65KEY?  UNTIL
          scradr c@ curmsk @ xor scradr c! GETKEY ;


\ *** Block No. 3, Hexblock 3

\ DECODE EXPECT KEYBOARD      BP28MAY85)          cas 18juli2020
7F CONSTANT #BS   0D CONSTANT #CR  &27 CONSTANT #ESC
                  0A CONSTANT #LF
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

\ send? (emit 65emit                            cas   06july2024

 VARIABLE SDRX  0 SDRX !
 VARIABLE SDRY  0 SDRY !
 : scroll?
      SDRY @ &30 = IF
        $F828 $F800 &1160 cmove ( scroll screen mem 1 up )
        $FC88 &40 &32 fill      ( erase last line )
        &29 SDRY ! THEN ;

 : (emit ( 8b -- )
    scradr c!    1 SDRX +!
    SDRX C@ &40 = IF SDRX OFF 1 SDRY +! scroll?
    THEN
 ;


\ *** Block No. 5, Hexblock 5

\ EMIT CR DEL PAGE AT AT?     25JAN85RE)        cas   06jyly2024

| Variable out    0 out !         | &40 Constant c/row

: 65emit   ( 8b -- ) pause  1 out +! (emit ;

: 65CR     1 SDRY +!  SDRX OFF scroll?
           out @  c/row /  1+  c/row *  out ! ;

: 65DEL    SDRX @ IF -1 SDRX +!   &32 65emit
           -1 SDRX +!  -2 out +!  THEN ;
: 65PAGE   $F800 &1200 &32 fill   out off  SDRX OFF  SDRY OFF ;

: 65at ( row col -- )
    dup SDRX !  swap DUP SDRY !   c/row * + out ! ;
: 65AT?  ( -- ROW COL ) out @  c/row /mod  &24 min swap ;

\ *** Block No. 6, Hexblock 6

\ 65type                                           cas 15jul2020

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

\                                                 cas 18juli2020
: >DRIVE ( BLOCK DRV# -- BLOCK' )
    BLK/DRV * +   OFFSET @ - ;
: DRV?    ( BLOCK -- DRV# )
    OFFSET @ + BLK/DRV / ;

: DRVINIT  NOOP ;

: READBLOCK ( ADR BLK )
  $f011 ! $f013 ! 01 $f010 c! ;

: WRITEBLOCK ( ADR BLK )
  $f011 ! $f013 ! 02 $f010 c! ;




\ *** Block No. 10, Hexblock a

\  (r/w                                           cas 18juli2020

:  (R/W  ( ADR BLK FILE R/WF -- FLAG)
   swap abort" no file"
   IF readblock  ELSE writeblock  THEN false ;

' (R/W  IS   R/W









