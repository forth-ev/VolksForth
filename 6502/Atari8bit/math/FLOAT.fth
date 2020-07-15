\ Floating Point Extension
\ using Atari 8bit ROM FP Routines
\ based on FIG Forth APX20029

\needs CALL INCLUDE" D:CALL.FTH"

CR .( loading Floating Point ext. )

VOCABULARY FMATH
FMATH ALSO DEFINITIONS

$D4 CONSTANT FR0
$E0 CONSTANT FR1
$FC CONSTANT FLPTR
$F3 CONSTANT INBUF
$F2 CONSTANT CIX

| : XCALL CALL DROP ;

| : AFP    $D800 XCALL ;
| : FASC   $D8E6 XCALL ;
| : IFP    $D9AA XCALL ;
| : FPI    $D9D2 XCALL ;
| : FADD   $DA66 XCALL ;
| : FSUB   $DA60 XCALL ;
| : FMUL   $DADB XCALL ;
| : FDIV   $DB28 XCALL ;
| : FLG    $DECD XCALL ;
| : FLG10  $DED1 XCALL ;
| : FEX    $DDC0 XCALL ;
| : FEX10  $DDCC XCALL ;
| : FPOLY  $DD40 XCALL ;

: F@ ( addr -- fp )
  >R R@ @ R@ 2+ @ R> 4 + @ ;

: F! ( fp addr -- )
  >R R@ 4 + ! R@ 2+ ! R> ! ;

: F.TY ( -- )
  BEGIN
    INBUF @ C@ DUP $7F AND EMIT
    1 INBUF +!
  $80 > UNTIL ;

: FSWAP ( fp1 fp2 -- fp2 fp1 )
  5 ROLL 5 ROLL 5 ROLL ;

: FDROP ( fp -- )
  2DROP DROP ;

: FDUP  ( fp -- fp fp )
  2 PICK 2 PICK 2 PICK ;

: FOVER ( fp1 fp2 -- fp1 fp2 fp1 )
  5 PICK 5 PICK 5 PICK ;

: F. ( fp -- )
  FR0 F@ FSWAP FR0 F!
  FASC F.TY SPACE
  FR0 F! ;

: F? ( addr -- )
  F@ F. ;

: <F ( fp1 fp2 -- )
  FR1 F! FR0 F! ;

: F> ( -- fp1 )
  FR0 F@ ;

: FS ( fp -- )
  FR0 F! ;

: F+ <F FADD F> ;
: F- <F FSUB F> ;
: F* <F FMUL F> ;
: F/ <F FDIV F> ;

: FLOAT ( n -- fp )
  FR0 ! IFP F> ;

: FIX   ( fp -- n )
  FS FPI FR0 @ ;

: FLOG    FS FLG   F> ;
: FLOG10  FS FLG10 F> ;
: FEXP    FS FEX   F> ;
: FEXP10  FS FEX10 F> ;

: ASCF ( addr -- fp )
  INBUF ! 0 CIX C! AFP F> ;

: F0= OR OR 0= ;
: F=  F- F0= ;
: F<  F- 2DROP $80 AND 0 > ;

: F, ( fp -- )
  ROT , SWAP , , ;

: FCONSTANT
  CREATE F, DOES> F@ ;
: FVARIABLE
  CREATE 6 ALLOT DOES> ;

| : FLIT
    R> DUP 6 + >R F@ ;
: FLITERAL
  COMPILE FLIT F, ;
: FLOATING
  BL WORD 1+
  ASCF FLITERAL ; IMMEDIATE
: [FLOATING] [COMPILE] FLOATING ; IMMEDIATE

CR .( Floating Point ext. loaded. ) CR

ONLYFORTH
