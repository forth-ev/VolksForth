
\ *** Block No. 0, Hexblock 0

\  ###  Main Directory  ###  cclv05jan87

Version History     $02
(C16 (C64 )         $03
Relocate System     $04
Special-Assembler   $05
savesystem          $0E
Target-compiler     $10
free                $34

Target Compiler Man $56















\ *** Block No. 1, Hexblock 1

\  ###  Main Directory  ###  cclv05jan87

Version History     $02
(C16 (C64 )         $03
Relocate System     $04
Special-Assembler   $05
savesystem          $0E
Target-compiler     $10
free                $34

Target Compiler Man $56















\ *** Block No. 2, Hexblock 2

\                            cclv06dec88

cas18aug06 - english translation
clv06dec88 - rewritten Manual

clv/re apr-oct87
 for rev 3.8 - C16/C64
 Scr 3,4,c,10,12,1b,2f


















\ *** Block No. 3, Hexblock 3

                              clv2:jul87

























\ *** Block No. 4, Hexblock 4

\ Relocating a system        clv2:jull87

$9400         $0400
( stacklength rstacklength -)
 empty hex
 over + origin +  origin 0A + ! \ r0
 origin +  dup    origin   1+ ! \ task
             6 -  origin  8 + ! \ s0
 (16 $c000 ' limit >body ! C)
 cold
\\ symbolic map of system
up@ origin - is stacklength
r0 @ up@ -   is rstacklength

disk-buffer  limit        first @
rstack       r0 @         rp@

user, warm   up@ udp @ +  up@
(heap)       up@          heap
stack        s0 @         sp@

system       here         origin 0FE +
user, cold   origin 0FE + origin
screen       0800         0400
page 0-3     0400         0000

\ *** Block No. 5, Hexblock 5

( Forth-6502 Assembler             WFR )
( Basis: Forth Dimensions VOL III No. 5)

Onlyforth  Assembler also definitions

1 8  +thru




















\ *** Block No. 6, Hexblock 6

( Forth-83 6502-Assembler              )

: end-code   context 2- @  context ! ;

Create index
0909 , 1505 , 0115 , 8011 ,
8009 , 1D0D , 8019 , 8080 ,
0080 , 1404 , 8014 , 8080 ,
8080 , 1C0C , 801C , 2C80 ,

| Variable mode

: Mode:  ( n -)   Create c,
  Does>  ( -)     c@ mode ! ;

0   Mode: .A        1    Mode: #
2 | Mode: mem       3    Mode: ,X
4   Mode: ,Y        5    Mode: X)
6   Mode: )Y       0F    Mode: )







\ *** Block No. 7, Hexblock 7

( Code generating primitives  27jun85we)

Variable >codes

| Create nrc ] c, , c@ here allot ! c! [

: nonrelocate   nrc >codes ! ;

nonrelocate

| : >exec Create c,
         Does> c@ >codes @ + @ execute ;

|  0 >exec >c,       |  2 >exec >,
|  4 >exec >c@       |  6 >exec >here
|  8 >exec >allot    | 0A >exec >!
| 0C >exec >c!









\ *** Block No. 8, Hexblock 8

( upmode  cpu                          )

| : upmode ( addr0 f0 - addr1 f1)
 IF mode @  8 or mode !   THEN
 1 mode @  0F and ?dup IF
 0 DO  dup +  LOOP THEN
 over 1+ @ and 0= ;

: cpu  ( 8b -)   Create  c,
  Does>  ( -)    c@ >c, mem ;

 00 cpu brk  18 cpu clc  D8 cpu cld
 58 cpu cli  B8 cpu clv  CA cpu dex
 88 cpu dey  E8 cpu inx  C8 cpu iny
 EA cpu nop  48 cpu pha  08 cpu php
 68 cpu pla  28 cpu plp  40 cpu rti
 60 cpu rts  38 cpu sec  F8 cpu sed
 78 cpu sei  AA cpu tax  A8 cpu tay
 BA cpu tsx  8A cpu txa  9A cpu txs
 98 cpu tya






\ *** Block No. 9, Hexblock 9

( m/cpu                                )

: m/cpu  ( mode opcode -)  Create c, ,
 Does>
 dup 1+ @ 80 and IF 10 mode +! THEN
 over FF00 and upmode upmode
 IF mem true Abort" invalid" THEN
 c@ mode @ index + c@ + >c, mode @ 7 and
 IF mode @  0F and 7 <
  IF >c, ELSE >, THEN THEN mem ;

 1C6E 60 m/cpu adc   1C6E 20 m/cpu and
 1C6E C0 m/cpu cmp   1C6E 40 m/cpu eor
 1C6E A0 m/cpu lda   1C6E 00 m/cpu ora
 1C6E E0 m/cpu sbc   1C6C 80 m/cpu sta
 0D0D 01 m/cpu asl   0C0C C1 m/cpu dec
 0C0C E1 m/cpu inc   0D0D 41 m/cpu lsr
 0D0D 21 m/cpu rol   0D0D 61 m/cpu ror
 0414 81 m/cpu stx   0486 E0 m/cpu cpx
 0486 C0 m/cpu cpy   1496 A2 m/cpu ldx
 0C8E A0 m/cpu ldy   048C 80 m/cpu sty
 0480 14 m/cpu jsr   8480 40 m/cpu jmp
 0484 20 m/cpu bit



\ *** Block No. 10, Hexblock a

( Assembler conditionals               )

| : range?   ( branch -- branch )
 dup abs  07F u> Abort" out of range " ;

: [[  ( BEGIN)  >here ;

: ?]  ( UNTIL)  >c, >here 1+ -
                range? >c, ;

: ?[  ( IF)     >c,  >here 0 >c, ;

: ?[[ ( WHILE)  ?[ swap ;

: ]?  ( THEN)   >here over >c@
                IF swap >!
 ELSE over 1+ - range? swap >c! THEN ;

: ][  ( ELSE)   >here 1+   1 jmp
 swap >here over 1+ - range?  swap >c! ;

: ]]  ( AGAIN)  jmp ;

: ]]? ( REPEAT) jmp ]? ;


\ *** Block No. 11, Hexblock b

( Assembler conditionals               )

90 Constant CS    B0 Constant CC
D0 Constant 0=    F0 Constant 0<>
10 Constant 0<    30 Constant 0>=
50 Constant VS    70 Constant VC

: not    20 [ Forth ] xor ;

: beq    0<> ?] ;   : bmi   0>= ?] ;
: bne    0=  ?] ;   : bpl   0<  ?] ;
: bcc    CS  ?] ;   : bvc   VS  ?] ;
: bcs    CC  ?] ;   : bvs   VC  ?] ;













\ *** Block No. 12, Hexblock c

\ 2/w/inc/dec c16 ram/rom..  cclv2:jul87

: 2inc
 dup lda  clc  2 # adc
 dup sta  CS ?[  swap 1+ inc  ]?  ;
: 2dec
 dup lda  sec  2 # sbc
 dup sta  CC ?[  swap 1+ dec  ]?  ;

: winc
 dup inc  0= ?[  swap 1+ inc  ]?  ;
: wdec
 dup lda  0= ?[  over 1+ dec  ]?  dec  ;

: ;c:
 recover jsr  end-code ]  0 last !  0 ;

(16 \ C16+Macros for Bankswitching
: ram $ff3f sta ;   : rom $ff3e sta ;
' Jsr Alias NormJsr   Defer Jsr

: C16+Jsr dup 0c000 u>
 IF rom NormJsr ram ELSE NormJsr THEN ;
' C16+Jsr Is Jsr  C)


\ *** Block No. 13, Hexblock d

( Assembler ;code Code Label  03feb85bp)

Onlyforth

: Assembler
 Assembler   [ Assembler ] mem ;

: ;Code
 [compile] Does>  -3 allot
 [compile] ;      -2 allot   Assembler ;
immediate

: Code  Create here dup 2- ! Assembler ;

: >label  ( adr -)
 here | Create  swap , 4 hallot
        heap 1 and hallot   \ 6502-align
        here 4 - heap  4  cmove
        heap last @ count 01F and + !
 dp !
        Does>  ( - adr)   @  ;

: Label
 [ Assembler ]  >here >label Assembler ;


\ *** Block No. 14, Hexblock e

\ cSave cLoad..               clv08aug87

\needs Code   .( ?! Code ?!) \\
Assembler
(16 \needs rom    .( ?! rom ?!) \\  C)

Onlyforth
$FF90 >label setMsg   $90 >label status
$FFBA >label setlfs $FFBD >label setNam
$FFD8 >label BSAVE  $FFD5 >label BLOAD
Label slPars
 setup jsr (16 rom C)
 $80 # lda setMsg jsr 0 # lda status sta
 N lda sec 8 # sbc  (drv    sta
       CC ?[ dex ]? (drv 1+ stx
 N ldx     N  1+ ldy 1 #  lda setlfs jsr
 N 4 + ldx N 5 + ldy N 2+ lda setnam jsr
 N 6 + ldx N 7 + ldy
 rts end-code

Label slErr \ AR=Kernalerror
 CC ?[ 0 # lda ]? pha
 status lda $BF # and
 (16 ram C) push jmp end-code
                                   -->

\ *** Block No. 15, Hexblock f

\ savesystem                   02oct87re

Onlyforth

Code cSave ( f t+1 Name Nlen dev--err)
 5 # lda    SLPars jsr
 N 8 + # lda bsave jsr
 slErr jmp end-code

: savesystem \ -- Name must follow
 \ Forth-Kernal a la boot:
   scr push 1 scr ! r#  push 0 r# !
 \ Editor  a la boot
   [ Editor ]
   stamp$ push stamp$ off
   (pad   push (pad   off
 \ nun geht's los
   save
   origin $17 - here     0 parse
   8 csave abort" Save-Error" ;






\ *** Block No. 16, Hexblock 10

\ Target compiler loadscr      11jul20pz
\ Idea and first Implementation by ks/bp
\ Implemented on 6502  by ks/bp
\ volksFORTH83-Version by bp/we

Onlyforth hex
: (blk@  blk @ ;
Defer blk@  ' (blk@ is blk@

\needs (16   .( ?! (16 (64 ?! C) quit
Assembler \needs nonrelocate 5 load
Assembler nonrelocate

Variable Image      C000 Image !

Vocabulary Ttools
Vocabulary Defining


 1 10 +thru   \ Target compiler
11 13 +thru   \ Target Tools
14 16 +thru   \ Redefinitions
clear
\ hex 17 20 +thru   \ predefinitions


\ *** Block No. 17, Hexblock 11

\ Target header pointers     bp27jun85we

Variable tdp
: there  tdp @ ;

Variable displace
Variable ?thead       0 ?thead !
Variable tlast        0 tlast !
Variable glast'       0 glast' !
Variable tdoes>
Variable >in:
Variable tvoc         0 tvoc !
Variable tvoc-link    0 tvoc-link !













\ *** Block No. 18, Hexblock 12

\ Image and byteorder        clv2:jull87

Code romoff (64
 sei 034 # lda 01 sta C) Next jmp

Code romon  (64
 036 # lda 01 sta cli C) Next jmp

Code >byte   ( 16b - 8bl 8bh)
 SP )Y lda pha txa SP )Y sta
 SP 2dec txa
 SP )Y sta pla PutA jmp

Code byte>   ( 8bl 8bh - 16b)
 SP X) lda pha SP 2inc pla SP )Y sta
 Next jmp end-code

: >image    ( addr1 - addr2)
 displace @  -  image @  + ;

: >heap  ( from quan -)
 heap over - 1 and +      \ 6502-align
 dup hallot heap swap cmove ;



\ *** Block No. 19, Hexblock 13

\ Ghost-creating             bp27jun85we

0 | Constant <forw>  0 | Constant <res>

| : Make.ghost  ( - cfa.ghost)
 here State @
 IF   Context @
 ELSE Current
 THEN @ dup @ ,
 name  dup  c@ 1 01F uwithin
            not abort" inval.Gname"
       1 over +!  c@ 1+ allot
 here 2 pick - -rot
 <forw> , 0 , 0 ,
 over 2+ c@ 1 and 1 xor >r
 over r@ -  here over - >heap
 heap r@ + swap !       Dp !
 heap r> + +  ;








\ *** Block No. 20, Hexblock 14

\ ghost words                ks27jun85we

: gfind  ( string - cfa tf / string ff)
 dup >r  1 over +!  find  -1 r> +! ;

: ghost   ( - cfa)
 >in @  name  gfind
  IF nip exit THEN
  drop >in ! Make.ghost ;

: Word,  ghost  execute ;

: gdoes>  ( cfa.ghost - cfa.does)
 4 + dup @
  IF @ exit THEN
  here dup <forw> , 0 , 4 >heap
  DP !  heap dup rot ! ;









\ *** Block No. 21, Hexblock 15

\ ghost utilities            ks27jun85we

: g'   name gfind 0= abort" ?" ;

: '.
 g' dup @ <forw> case?
   IF ."  forw"
   ELSE <res> - abort" ??" ."  res" THEN
  2+ dup @ 5 u.r
  2+ @ ?dup
   IF dup @ <forw> case?
    IF ."  fdef"
    ELSE <res> - abort"  ??" ."  rdef"
    THEN
    2+ @ 5 u.r THEN ;


' ' Alias h'








\ *** Block No. 22, Hexblock 16

\ .unresolved                bp27jun85we

| : forward? ( cfa - cfa / exit&true)
 dup @ <forw> =
 over 2+ @ and
 IF drop True rdrop exit THEN ;

| : unresolved? ( addr - f)
 2+ dup c@ 01F and over + c@ BL =
 IF name> forward? 4 + @
    dup IF forward? THEN
 THEN drop  False ;

| : unresolved-words
 BEGIN @ ?dup WHILE dup unresolved?
    IF dup  2+ .name ?cr THEN
 REPEAT ;

: .unresolved
 voc-link @
 BEGIN dup 4 - unresolved-words
 @ ?dup 0= UNTIL ;




\ *** Block No. 23, Hexblock 17

\ Ext. vocs for t-compilat.  bp27jun85we

: Vocabulary
Vocabulary 0 , here tvoc @ , tvoc ! ;

Vocabulary Transient    0 tvoc !

Only definitions Forth also

: T Transient ;   immediate
: H Forth     ;   immediate

definitions













\ *** Block No. 24, Hexblock 18

\ Transient primitives       ks04jul85we

Transient definitions

: c@    H >image romoff c@ romon ;

: c!    H >image romoff c! romon ;

: @     T dup c@ swap 1+ c@ byte> ;

: !     >r  >byte r@ 1+ T c!  r> c! ;

: cmove ( from.mem to.target quan -)
    bounds ?DO
    dup H c@ I T c! H 1+ LOOP drop ;

: here  there ;

: allot Tdp +! ;

: c,    T here c! 1 allot H ;

: ,     T here !  2 allot H ;



\ *** Block No. 25, Hexblock 19

\ Transient primitives       bp27jun85we

: ,"    Ascii " parse dup T c,
        under there swap cmove allot H ;

: fill  ( addr quan 8b -)
        -rot bounds
        ?DO dup  I T c! H LOOP drop ;

: erase  0  T fill ;
: blank  BL T fill ;
: here!  H tdp ! ;














\ *** Block No. 26, Hexblock 1a

\ Resolving                  ks29jun85we

Forth definitions

: resolve  ( cfa.ghost cfa.target -)
 over dup @ <res> =
   IF space dup >name .name ." exists  "
      2+ ! drop exit THEN
 >r >r 2+ @ ?dup
 IF BEGIN dup T @ H
           2dup = abort" resolve loop"
       r@ rot T ! H ?dup 0= UNTIL THEN
 r> r>  <res> over !  2+ ! ;

: resdoes>  ( cfa.ghost cfa.target -)
 swap gdoes> dup @ <res> =
 IF 2+ ! exit THEN swap resolve ;

] Does> [ here 3 - 0 ]
        dup @ there  rot ! T , H ;
  ' <forw> >body !

] Does> [ here 3 - 0 ]
        @ T , H ;
  ' <res>  >body !

\ *** Block No. 27, Hexblock 1b

\ move-threads  6502-align   clv24.3.87)

: move-threads
 Tvoc @  Tvoc-link @
 BEGIN over ?dup WHILE 2- @  over 2- T !
       @ H  swap @ swap
 REPEAT
 error" some undef. Target-Vocs left"
 drop ;

 : tlatest   ( - addr)
   Current @ 6 +  ;

 : 6502-talign  ( supposed cfa -- )
 0FF and 0FF =  IF  1 T allot H  THEN ;

: save-target   \ name must follow
 08 02 busopen
 0 parse bustype " ,p,w" count bustype
 busoff
 08 02 busout
 displace @ 100 u/mod swap bus! bus!
 there displace @
   DO I T c@ H bus! LOOP
 08 02 busclose ;

\ *** Block No. 28, Hexblock 1c

\ compiling names into targ.   11jul20pz

: (theader
 ?thead @  IF 1 ?thead +!
     there 6502-talign exit THEN
 >in @ name swap >in !
 dup c@ 1 020 uwithin not
                  abort" inval. Tname"
 dup  c@ 5 +  there +  6502-talign
 blk@  T , H
 there tlatest dup @  T , H !
 there dup tlast !  over
 c@ 1+ dup  T allot cmove H  ;


: Theader    tlast off
 (theader Ghost
 dup glast' ! there resolve ;








\ *** Block No. 29, Hexblock 1d

\ prebuild defining words    bp27jun85we

| : executable?   ( adr - adr f)  dup ;

| : tpfa,  there , ;


| : (prebuild   ( cfa.adr -)
 >in @  Create  >in !  here 2- ! ;

: prebuild   ( adr 0.from.: - 0)
 0 ?pairs
 executable? dup >r
 IF [compile] Literal
     compile  (prebuild
 ELSE drop  THEN
 compile Theader  Ghost gdoes> ,
 r>  IF compile tpfa, THEN 0 ;
immediate restrict







\ *** Block No. 30, Hexblock 1e

\ code portion of def.words  bp27jun85we

: dummy   0 ;


: Do>     ( - adr.of.jmp.dodoes> 0)
          [compile] does>  here 3 -
           compile  @   0  ]  ;


















\ *** Block No. 31, Hexblock 1f

\ the 6502 Assembler         bp27jun85we

| Create relocate
 ] T c, , c@ here allot ! c!   H [

Transient definitions

: Assembler  H
 [ Assembler ] relocate >codes !
 Assembler ;

: >label  ( 16b -) H
 >in @ name gfind  rot >in !
  IF over resolve dup THEN
 drop Constant   ;

: Label  H there T >label  Assembler H ;

: Code  H
 Theader there 2+ T , Assembler H   ;






\ *** Block No. 32, Hexblock 20

\ immed. restr. ' | compile  bp27jun85we

: ?pairs    ( n1 n2 -- ) H
 - abort" unstructured" ;

: >mark     ( - addr)  H there T 0 , H ;
: >resolve  ( addr -)  H
            there over - swap  T ! H ;
: <mark     ( - addr)  H there ;
: <resolve  ( addr -)  H there - T , H ;

: immediate   H Tlast @ ?dup
 IF dup T c@  040 or swap c! H THEN ;

: restrict    H Tlast @ ?dup
 IF dup T c@  080 or swap c! H THEN ;

: '   ( <name> - cfa)  H
 g' dup @ <res> -   abort" ?" 2+ @ ;

: |  H ?thead @ ?exit ?thead on ;

: compile  H Ghost , ;
 immediate  restrict


\ *** Block No. 33, Hexblock 21

\ Target tools               ks27jun85we

Onlyforth Ttools also definitions

| : ttype   ( adr n -)
 bounds ?DO I T c@ H  emit LOOP ;

: .name    ( nfa -)
 ?dup
 IF dup 1+ swap T c@ H 01F and ttype
 ELSE ." ??? " THEN  space ?cr ;

| : nfa? ( cfa lfa - nfa / cfa ff)
   BEGIN  dup WHILE  2dup 2+ dup
            T c@ H 01F and + 1+ =
             IF 2+ nip exit THEN
            T @ H  REPEAT ;

: >name  ( cfa - nfa / ff)
 Tvoc BEGIN @ dup WHILE under 2- @ nfa?
        ?dup IF nip exit THEN
      swap  REPEAT  nip  ;




\ *** Block No. 34, Hexblock 22

\ Ttools for decompiling     ks29jun85we

| : ?:  dup          4 u.r ." :"  ;
| : @?  dup  T @  H  6 u.r  ;
| : c?  dup  T c@ H  3  .r  ;

: s  ( adr - adr+)
 ?: space  c?  3 spaces
 dup 1+ over T c@ H ttype dup
 T c@ H + 1+ ;

: n  ( adr - adr+2)
 ?: @? 2 spaces dup
 T @ H [ Ttools ] >name .name H 2+ ;

: d  ( adr n - adr+n)
 2dup swap ?: swap 0
   DO c? 1+ LOOP 2 spaces -rot ttype ;








\ *** Block No. 35, Hexblock 23

\ Tools for decompiling      bp29jun85we

: l  ( adr - adr+2)
 ?: 5 spaces @? 2+  ;

: c  ( adr - adr+1)   1 d ;

: b  ( adr - adr+1)
 ?: @? dup T @ H over + 5 u.r 2+ ;

: dump   ( adr n -)
 bounds ?DO cr I 8 d drop stop?
   IF LEAVE THEN 8 +LOOP ;

: view
 T ' H  [ Ttools ] >name
 ?dup IF 4 - T @ H edit THEN ;









\ *** Block No. 36, Hexblock 24

\ reinterpretation def.-words  27jun85we

Onlyforth

: redefinition
 tdoes> @
 IF >in push
    [ ' >interpret >body ] Literal push
   State push  Context push
   >in: @ >in !
   name [ ' Transient 2+ ] Literal
   (find nip 0=
   IF
    cr ." Redefinition: " here .name
   >in: @ >in ! : Defining interpret
   THEN
 THEN
 0 tdoes> ! ;








\ *** Block No. 37, Hexblock 25

\ Create..does> structure    bp27jun85we

| : (;tcode
 Tlast @ dup T c@ + 1+ !  H rdrop ;

| : changecfa
 compile lit tdoes> @  ,
 compile (;tcode ;

Defining definitions

: ;code   0 ?pairs
 changecfa  reveal  rdrop  ;
 immediate restrict

Defining  ' ;code  Alias  does>
 immediate restrict

: ;   [compile] ; rdrop ;
 immediate restrict






\ *** Block No. 38, Hexblock 26

\ redefinition conditionals  bp27jun85we

' DO    Alias  DO     immediate restrict
' ?DO   Alias  ?DO    immediate restrict
' LOOP  Alias  LOOP   immediate restrict
' IF    Alias  IF     immediate restrict
' THEN  Alias  THEN   immediate restrict
' ELSE  Alias  ELSE   immediate restrict
' BEGIN Alias  BEGIN  immediate restrict
' UNTIL Alias  UNTIL  immediate restrict
' WHILE Alias  WHILE  immediate restrict
' REPEAT Alias  REPEAT
 immediate restrict













\ *** Block No. 39, Hexblock 27

\ clear Liter. Ascii ['] ."  bp27jun85we

Onlyforth Transient definitions

: clear  True abort" There are ghosts" ;

: Literal  ( n -)  H dup $FF00 and
 IF T compile lit ,
 ELSE compile clit c, H THEN ; immediate

: Ascii  H BL word 1+ c@
 State @
  IF T [compile]  Literal H THEN ;
 immediate

: [']     T ' [compile] Literal H ;
          immediate restrict

: "       T compile ("   ," H  ;
          immediate restrict

: ."      T compile (."  ," H  ;
          immediate restrict



\ *** Block No. 40, Hexblock 28

\ Target compilation  ]  [   bp03jul85we

Forth definitions

: tcompile
 ?stack  >in @
 name find ?dup
 IF 0> IF nip execute   >interpret THEN
    drop dup >in !  name THEN
 gfind IF nip execute   >interpret THEN
 nullstring? IF drop exit THEN
 number? ?dup
  IF 0> IF swap T [compile] Literal THEN
      [compile] Literal H drop
      >interpret THEN
 drop >in ! Word,
 >interpret ; -2 allot

Transient definitions

: ]    H  State on
 ['] tcompile is >interpret ;




\ *** Block No. 41, Hexblock 29

\ Target conditionals        bp27jun85we

: IF      T compile ?branch >mark H 1 ;
          immediate restrict
: THEN    abs 1 T ?pairs >resolve H ;
          immediate restrict
: ELSE    T 1 ?pairs compile branch
          >mark swap >resolve H -1 ;
          immediate restrict
: BEGIN   T <mark H 2 ;
          immediate restrict
: WHILE   T 2 ?pairs  2 compile ?branch
          >mark -2  H 2swap ;
          immediate restrict

| : (repeat  T 2 ?pairs  <resolve H
 BEGIN dup -2 = WHILE drop T >resolve H
 REPEAT ;

: UNTIL   T compile ?branch (repeat H  ;
          immediate restrict
: REPEAT  T compile branch  (repeat H  ;
          immediate restrict



\ *** Block No. 42, Hexblock 2a

\ Target conditionals        bp27jun85we

: DO     T compile (do  >mark H 3 ;
         immediate restrict
: ?DO    T compile (?do >mark H 3 ;
         immediate restrict
: LOOP   T 3 ?pairs  compile (loop
         compile endloop >resolve H ;
         immediate restrict
: +LOOP  T 3 ?pairs  compile (+loop
         compile endloop >resolve H ;
         immediate restrict














\ *** Block No. 43, Hexblock 2b

\ predefinitions             bp27jun85we

: abort"    T compile (abort" ," H ;
 immediate

: error"    T compile (err"   ," H ;
 immediate




Forth definitions

Variable torigin
Variable tudp       0 tudp !

: >user  T c@ H torigin @ + ;









\ *** Block No. 44, Hexblock 2c

\ Datatypes                  bp27jun85we

Transient definitions

: origin!  H torigin !  ;

: user' ( - 8b)   T ' 2 + c@ H ;

: uallot  ( n -) H tudp @ swap tudp +! ;


        Do> >user ;
: User  prebuild User  2 T uallot c, ;

           Do> ;
: Create   prebuild Create  ;










\ *** Block No. 45, Hexblock 2d

\ Datatypes                  bp27jun85we

           Do> T @ H ;
: Constant prebuild Constant  T , ;

: Variable Create 2 T allot ;


 dummy
: Vocabulary
 H >in @  Vocabulary  >in !
 T prebuild Vocabulary  0 , 0 ,
 here H tvoc-link @ T ,  H tvoc-link ! ;

          Do>  ;
: Defer   prebuild Defer 2 T allot ;

: Is  T ' H >body  State @
   IF T compile (is  ,
 H ELSE T ! H THEN  ;   immediate






\ *** Block No. 46, Hexblock 2e

\ target defining words      bp27jun85we

| : dodoes>
 T compile (;code
 H Glast' @  there  resdoes>
 there tdoes> ! ;

: ;code 0 T ?pairs dodoes>  Assembler
 H [compile] [  redefinition ;
 immediate restrict

: does>
 T dodoes>
 $4C c, compile (dodoes> H ;
 immediate restrict

 dummy
: :  H  tdoes> off  >in @ >in: !
 T prebuild : H current @ context !
 T ] H 0 ;






\ *** Block No. 47, Hexblock 2f

\ :  Alias  ;                  02oct87re

: Alias ( n -- )  H Tlast off
 (theader  Ghost  over resolve
 tlast @ T c@ H 20 or tlast @ T c! ,
 H ;

: ;  T 0 ?pairs
 compile unnest [compile] [
 H redefinition  ;
 immediate  restrict

 dummy
: Input:  H  tdoes> off  >in @ >in: !
 T prebuild Input:
 H current @ context !  T ] H 0 ;

 dummy
: Output:  H  tdoes> off  >in @ >in: !
 T prebuild Output:
 H current @ context !  T ] H 0 ;





\ *** Block No. 48, Hexblock 30

\ predefinitions             bp03jul85we

: compile   T compile compile H ;
            immediate  restrict

: Host
 H Onlyforth Ttools also ;

: Compiler
 T Host H Transient also definitions ;

: [compile] H Word, ; immediate restrict

: Onlypatch H there 3 - 0 tdoes> ! 0 ;

Onlyforth

: Target
 Onlyforth Transient also definitions ;


Transient definitions  Ghost c, drop




\ *** Block No. 49, Hexblock 31



























\ *** Block No. 50, Hexblock 32



























\ *** Block No. 51, Hexblock 33



























\ *** Block No. 52, Hexblock 34



























\ *** Block No. 53, Hexblock 35



























\ *** Block No. 54, Hexblock 36



























\ *** Block No. 55, Hexblock 37



























\ *** Block No. 56, Hexblock 38



























\ *** Block No. 57, Hexblock 39



























\ *** Block No. 58, Hexblock 3a



























\ *** Block No. 59, Hexblock 3b



























\ *** Block No. 60, Hexblock 3c



























\ *** Block No. 61, Hexblock 3d



























\ *** Block No. 62, Hexblock 3e



























\ *** Block No. 63, Hexblock 3f



























\ *** Block No. 64, Hexblock 40



























\ *** Block No. 65, Hexblock 41



























\ *** Block No. 66, Hexblock 42



























\ *** Block No. 67, Hexblock 43



























\ *** Block No. 68, Hexblock 44



























\ *** Block No. 69, Hexblock 45



























\ *** Block No. 70, Hexblock 46



























\ *** Block No. 71, Hexblock 47



























\ *** Block No. 72, Hexblock 48



























\ *** Block No. 73, Hexblock 49



























\ *** Block No. 74, Hexblock 4a



























\ *** Block No. 75, Hexblock 4b



























\ *** Block No. 76, Hexblock 4c



























\ *** Block No. 77, Hexblock 4d



























\ *** Block No. 78, Hexblock 4e



























\ *** Block No. 79, Hexblock 4f



























\ *** Block No. 80, Hexblock 50



























\ *** Block No. 81, Hexblock 51



























\ *** Block No. 82, Hexblock 52



























\ *** Block No. 83, Hexblock 53



























\ *** Block No. 84, Hexblock 54



























\ *** Block No. 85, Hexblock 55



























\ *** Block No. 86, Hexblock 56

\ Target Compiler Manual      clv06dec88

Target-Compiler volksFORTH 3.8 6502









(c) volksFORTH-Developers 1985-2006
    and Forth Gesellschaft e.V.
    http://www.forth-ev.de











\ *** Block No. 87, Hexblock 57

\ ..Gebrauchsanweisung..      clv05jan87

1. Introduction
   The Targetcompiler is weird,
   kryptic and sometimes dangerous.
   If an error occurs, reboot machine
   and start from scratch.



















\ *** Block No. 88, Hexblock 58

\ ..Manual                    clv06dec88

2. Load

2.1. Relocate System
   The Relocate Screen on this Disk
   creats an Environment with Stacks
   Stacklen  $9400
   rStacklen $0400

   Limit must be $c000 which is default
   for C64, for C16 this value must
   be down-patched
2.1. load save-system from disk 1
2.2. load Editor (if needed)
2.3. Load Targetcompiler with Loadscr.
     first only the resident part
2.4. execute savesystem
2.5. load Tragetcompiler predefinitions
     (see bottom of Loadscreen)






\ *** Block No. 89, Hexblock 59

  a


                     volksforth 3.8TC  u3
 2a    

          t and Targetmachine
       the Sources are prepared for
       C16 & C64. Change the definitions
       of (C16 (C64 (see Screen and
       Handbook)
     - Blk ## here #### there ####
       will be printed as status msg.
     - various <name> exists
       Messages are ok
     - at the End  SAVESYSTEM <name>
       will be printed. Change Disk!
       and then press <return> to save
       new Forth System







\ *** Block No. 90, Hexblock 5a

\\ ..Example session          clv06dec88

3. Example for CBM Plus 4


   Used Disks:
   1of4 .. 4of4 - volksForth Disks 3.8
   TCq          - TargetComp Source
   TCf          -     "    Files (empty)

3.1. Create a Targetcompiler System


   Switch on Plus4, insert Disk 1of4

   DIRECTORY
   -> ...

   LOAD "C16ULTRAFORTH83",8
   -> searching... loading...ready.

   RUN
   -> ultraFORTH ... ok



\ *** Block No. 91, Hexblock 5b

\\ ..Example Session          clv06dec88

    insert Disk TCq
    4 load flush  \ relocate
    -> volksFORTH83 ... ok

    insert Disk 3of4
    19 load flush \ Editor
    -> blk 4 blk 5 ...... ok \ appr5 min

    insert Disk 1of4
    26 load flush \ savesystem

    insert Disk TCq
    $10 load flush \ TC-resident Part

    insert Disk TCf
    savesystem @:vf-tc-3.8
    -> ok  If Floppy flashes: Error
    \ @: is: overwrite if needed
    \ this file will later replace
    \ the running Forth System




\ *** Block No. 92, Hexblock 5c

\\ ..Example Session          clv06dec88
3.2. Compiling a new System
     first as for 3.1.
     or:
     LOAD "vf-tc-3.8",8
     -> searching...loading..ready.

     RUN
     -> volksFORTH83 ... ok

     insert Disk TCq
     hex 27 30 thru flush
        \ load transient Part of TC

     insert Disk 2of4 (Forth Source)
     $09 l \ edit Screen 9
             (c64 (c16 (c16+ (c16-
             (comment in or out depend.
             on Target-Maschine

     $f load \ compile system
     insert TCf or blank disk
     savetarget c16ultraforth83
 or  savetarget c64ultraforth83
- ENDE -

\ *** Block No. 93, Hexblock 5d



























\ *** Block No. 94, Hexblock 5e



























\ *** Block No. 95, Hexblock 5f



























\ *** Block No. 96, Hexblock 60



























\ *** Block No. 97, Hexblock 61



























\ *** Block No. 98, Hexblock 62



























\ *** Block No. 99, Hexblock 63



























\ *** Block No. 100, Hexblock 64



























\ *** Block No. 101, Hexblock 65

\\ Terms:
Host      'normal' Forth-System in
          Machine
Target    System to be compiled
Transient Vocabulary for T-Compilation
          in Host

needs the Specialassembler for TC
'normal' Assembler

Startaddress of Target Systems in Host






Tools for Target-System
Words for Redefinitions







\ *** Block No. 102, Hexblock 66

\\

Target-dp
here in Target

Startaddress of Target-Systems
If 0, we create header in Target
nfa of last created word
cfa of last created ghost
cfa of Code for Does>-Parts
Address of last : in Block
Tvoc-Link for Host
Tvoc-Link for Target













\ *** Block No. 103, Hexblock 67

\\

switches to RAM under OS


switches Back











calculates physical Address from
target Address

cmove to Heap with automatic hallot
>image must be adjusted when changing
memory managent for target system

same with c@ and c!

\ *** Block No. 104, Hexblock 68

\\

<forw> points to code for forward
<res>  points to code for resolved

    if compiling, Ghost will be linked
under(!) last Context Word
else appended to Current as normal
lfa Ghost points to last word  (s.a)
get name and checks
            for valid length
 enlarges name by one blank
calculates namelength ( len start link)
cf.Ghost, cfa.Target, Ptr to Does>.cfa
6502-align
cmoved Ghost on Heap
last word points not to Ghost
caclculates cfa.ghost

Last (Current @) will be linked to ; in
reveal





\ *** Block No. 105, Hexblock 69

\\

search for ghost



search for ghost
if found, finish
if not found, it will be created

stores cfa.ghost ( <forw> or <res> )


Adr of Ptr to Does>.cfa
if existing, cfa of Does>
else create PTR and see above



>heap starts on even addresses
=> 6502-align





\ *** Block No. 106, Hexblock 6a

\\

gets cfa of Ghost


get state of ghost, if forward reference
or already resolved (with cfa.Target)


same for possib.  Does>-Parts







while Target-Compilation ' will be done
for Ghosts







\ *** Block No. 107, Hexblock 6b

\\


if cf =<forw>
and an Address exists in Target System
exit unresolved? with True-Flag


checks if name is a Ghost
gets cfa and checks for unresolved
 same with Does>-Part



print for Vocabulary all
unresolved words



search through all Vocabularies
print non-resolved words





\ *** Block No. 108, Hexblock 6c

\\

Vocabulary structure:
Name           Bytes
Code           fuer Vocabularys
Latest          0 1
Cold-Latest     2 3   normal
Voc-link        4 5
-------------------
Tlatest         6 7   additional
Tvoc-link       8 9


T and H are Immediate and replacing
             [ Transient ]
        or   [ Forth ]  H => Host










\ *** Block No. 109, Hexblock 6d

\\  Words for Target-Compilation

Words for virtual Target-Memory

T c@ acces RAM below OS

T c! also
All following words use c@ and c!




cmove works only from Host to
Target, not other direction






On changes to the virtual Memory-
Management, c@, c! and >image
must be adjusted!



\ *** Block No. 110, Hexblock 6e

\\  Words for Target-Compilation

























\ *** Block No. 111, Hexblock 6f

\\



resolves forward references
checks, if already resolved
print warning if yes
set cfa.target to new value, End
Forwardreference available ?
Yes, get address in Target
 Cancel, if pointing to itself
and set cfa.target until End
cfa.target to cfa.ghost and
resolved as cf.target

same for Does>-Parts


Does> compiles a JMP (dodoes>
(dodoes> brings the on
cfa.ghost folllowing address, which is
cfa.target, on the stack
cfa.ghost is either  <forw> or <res>
cfa.target is the cfa in Target-System
also <forw> or already valid

\ *** Block No. 112, Hexblock 70

\\


for all Vocabluaries in Transient and
Target set Target-Cold-Latest
to Transient-Tlatest

Error, if Tvoc-link also points to other
Vocabularys


Points to Tlatest in Transient-Vocs




saves a Targetsystem as Programm-
file on Disk (C64-Special!!)








\ *** Block No. 113, Hexblock 71

\\


if 0, Header, else ?thead increment
     6502-align and Ende
gets Name an converts to capital
Error on wrong length

calulates cfa and 6502-align
Blocknumber for view
link in of new name in Current
Tlast for immediate and restric create
save Name in Target


         set Tlast to 0
create Header, create Ghost, if new,
cfa.ghost to Glast' and resolve








\ *** Block No. 114, Hexblock 72

\\

on Create, User, Constant, Variable
and Defer, not for : und Vocabulary
compiling Ptr to pfa.target in Host


creates Header in Host with cfa.adr
This points to a Does>-Part in
Target (s.below)

from:
if created word should be executable
in current, create a header else not
with corresponding cfa
else not
create header in Target and compile
as cfa of Does>-Part
store this address in Host as pdf


prebuild is a Defining-Word for
Defining-Words !!



\ *** Block No. 115, Hexblock 73

\\

results that with follwing
Defining-Word created words cannot be
executed
Special-Does> for Words created in
Current, gives pfa in Target !



Do> ... ;  : ... Build ... ; same as
Create ... Does>

: Do>     [compile] does> here 3 - 0 ] ;

: (build  Create here 2- ! ;
: Build  (cfa 0 - 0)
          0 ?pairs  [compile] Literal
          compile (build 0 ;







\ *** Block No. 116, Hexblock 74

\\


Assembler assembles in Target now




enables Assembler and relocate



if label already exists as Ghost,
  resolve forward reference
as Constant in Host

Label points to there

Special-Code for Target







\ *** Block No. 117, Hexblock 75

\\
Controlstructures for Target-System















' in Transient accesses Ghosts and
gets cfa.target

The next word will be created without
header
works on Host, not on Target



\ *** Block No. 118, Hexblock 76

\\     Tools for Target-System
    similar to normal tools


prints n chars at addr


prints name of word, if nfa <>0


else  ???

checks, if lfa nfa of cfa is and
returns nfa, else  cfa and ff




converts cfa in nfa, if possible







\ *** Block No. 119, Hexblock 77

\\     Tools for Target-System
    similar to normal tools




prints string at adr and adjusts adr




print name of compile word and adjust
adr


prints n bytes from adr and adjusts adr










\ *** Block No. 120, Hexblock 78

\\     Tools for Target-System









prints n Bytes at adr, like d, but nice
formatted


displays Sourcecodescreen of word











\ *** Block No. 121, Hexblock 79

\\

allows execution of new created defining
words during target compilation

is ;code or does> ?
 yes, save systemstate

 >in to begin of last Colondef.

  no as predefinition in
  Transient available?
  yes, print "Redefinition: " and last
      Names
  >in adjust, Forth-: execute and switch
  on Definining as Context

  reset tdoes>








\ *** Block No. 122, Hexblock 7a

\\

changes the cfa of lst defined word
to Does>-Part in Target


compiles, which compiles the adr of
Does>-part and (;code when executed


;code and Does> must be defined in redef

same as Do> for prebuild
stores last word in Host and jumps
in redefinition behind interpret

Structur of a in Host created word:
lfa\name\cfa to jmp (doedoes\pfa to
Does>-Part in Target

Words, created by Redefinition of
Defining Words, return their
PFA, when executed in Host



\ *** Block No. 123, Hexblock 7b

\\

Forth-Controllstructures, because
Transient will find words for Target






















\ *** Block No. 124, Hexblock 7c

\\         Predefinitions
Words that must be executable in
Transient























\ *** Block No. 125, Hexblock 7d

\\    main Compileloop





search for names in Transient and Forth
found, execute, if immediate
   else reset  >in
search Ghost and exdcute  cfa

Number?, if yes, execute Literal (T!)



create new ghost and compile forward
reference



enable Compiler, set >interpret
from tcompile




\ *** Block No. 126, Hexblock 7e

\\ Conditionals for  Target-Compilation

























\ *** Block No. 127, Hexblock 7f

\\ Conditionals for  Target-Compilation

























\ *** Block No. 128, Hexblock 80

\\

Immediate-Words for Target










origin in target
udp    in target

calculates Address in User-Area from
Address of Offset








\ *** Block No. 129, Hexblock 81

\\










User-Variable are also executable in
Transient and return Target-Addresses


Witg Create compiled Words are execut-
able in Transient and return the
pfa.target








\ *** Block No. 130, Hexblock 82

\\

Also Constant and Variable can be execut
ed in Transient and rezturn the Target
Values




Vocabularys are executable in transient


Vocabulary 'name' created:

1. A Vocabulary Entry in Current with
   5 fields connected on tvoc
2. A Vocabulary Entry in Current with
   3 Fields connected on tvoc-link
3. A Ghost







\ *** Block No. 131, Hexblock 83

\\


creates a Variable-header

same as Is in Forth



creates Code kile ;code in Forth

Forwardrefernce for Does>-Part will
resolved and tdoes> set for redefinition

like ;code in Forth, but with redefiniti
on


like Does> in Forth, but with dodoes>







\ *** Block No. 132, Hexblock 84

\\


disable redefinition
 create small entry in Transient
 for with : created words
 set Context to first fix Voc
create Header in Target
and resolve forward-refernce with
Alias-cfa
same as hide in Forth

same as  ; in Forth, and redefinition
is started












\ *** Block No. 133, Hexblock 85

\\

creates compile as forwardrefernce !!


set order to:
Ttools Ttools Forth Only

as Host, order:
Transient Transient Ttools Forth Only

compils the cfa.target of Ghosts

Special-Code for Only-Vocabulary


set order for Target-Compilation:
Transient Transient Forth Only

Thanks to Klaus for 'punctuation?' !!!






\ *** Block No. 134, Hexblock 86



























\ *** Block No. 135, Hexblock 87



























\ *** Block No. 136, Hexblock 88



























\ *** Block No. 137, Hexblock 89



























\ *** Block No. 138, Hexblock 8a



























\ *** Block No. 139, Hexblock 8b



























\ *** Block No. 140, Hexblock 8c



























\ *** Block No. 141, Hexblock 8d



























\ *** Block No. 142, Hexblock 8e



























\ *** Block No. 143, Hexblock 8f



























\ *** Block No. 144, Hexblock 90



























\ *** Block No. 145, Hexblock 91



























\ *** Block No. 146, Hexblock 92



























\ *** Block No. 147, Hexblock 93



























\ *** Block No. 148, Hexblock 94



























\ *** Block No. 149, Hexblock 95



























\ *** Block No. 150, Hexblock 96



























\ *** Block No. 151, Hexblock 97



























\ *** Block No. 152, Hexblock 98



























\ *** Block No. 153, Hexblock 99



























\ *** Block No. 154, Hexblock 9a



























\ *** Block No. 155, Hexblock 9b



























\ *** Block No. 156, Hexblock 9c



























\ *** Block No. 157, Hexblock 9d



























\ *** Block No. 158, Hexblock 9e



























\ *** Block No. 159, Hexblock 9f



























\ *** Block No. 160, Hexblock a0



























\ *** Block No. 161, Hexblock a1



























\ *** Block No. 162, Hexblock a2



























\ *** Block No. 163, Hexblock a3



























\ *** Block No. 164, Hexblock a4



























\ *** Block No. 165, Hexblock a5



























\ *** Block No. 166, Hexblock a6



























\ *** Block No. 167, Hexblock a7



























\ *** Block No. 168, Hexblock a8



























\ *** Block No. 169, Hexblock a9


























