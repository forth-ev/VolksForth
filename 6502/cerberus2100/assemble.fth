
\ *** Block No. 0, Hexblock 0

\\                     *** Assembler ***               25may86we

Dieses File enthält den 68000-Assembler für volksFORTH-83.
Der Assembler basiert auf dem von Michael Perry für F83 entwik-
kelten, enthält aber einige zusätzliche Features.
Wegen der Heap-Struktur im volksFORTH sind z.B. echte Labels
verwendbar. Aus Geschwindigkeitsgründen enthält der Assembler
kaum Fehlerüberprüfung, es empfiehlt sich daher, nach getaner
Tat die Code-Worte mit einem Disassembler zu überprüfen.

Screen $11 enthält einen Loadscreen, mit dem man der kompletten
Assembler auf den Heap laden kann, damit er während der Kompila-
tionszeit zur Verfügung steht, aber keinen Platz im Dictionary
verbraucht. Mit  CLEAR  oder  SAVE  wird der Assembler entfernt,
wenn er nicht mehr benötigt wird.


\ *** Block No. 1, Hexblock 1

\ 68000 Assembler Load Screen                          26oct86we

Onlyforth
Vocabulary Assembler   Assembler also definitions

: end-code   context 2- @  context ! ;
' swap  | Alias  *swap

base @  4 $11 +thru  base !

: reg)        size push   .l 0 *swap  FP DI) ;
: Next        .w IP )+ D7 move   D7 reg) D6 move   D6 reg) jmp
              >here next-link @ ,  next-link !  ;

2 3 +thru     Onlyforth


\ *** Block No. 2, Hexblock 2

\ Internal Assembler                                   09sep86we

Onlyforth

here
    $1300 hallot    heap dp !    -1 +load
dp !










\ *** Block No. 3, Hexblock 3

\ Extended adressing modes                             09sep86we

: R#)    ( addr -- )        size push
   [ Forth ] dup  0< IF  [ Assembler ] .w # D6 move   D6 reg)
   [ Forth ] exit  THEN     .w FP D) ;


| : inrange?   ( addr -- offset f )    [ Forth ]
    >here 2+ -  >here 0< IF  dup  $FFFE >here - < exit  THEN
                             dup  >here negate  > ;
: pcrel)       ( addr -- )     \ pc-relativ adressing mode
   inrange? [ Forth ] 0= abort" out of range"   pcd) ;

: ;c:         0 recover R#) jsr  end-code ] ;



\ *** Block No. 4, Hexblock 4

\ Assembler Forth words                                09sep86we
Forth definitions
: Assembler     Assembler [ Assembler ] .w ;
: Code          Create  here dup 2- ! Assembler ;

| : (;code      r> last @ name> ! ;
: ;Code         0 ?pairs  compile (;code  [compile] [  reveal
                Assembler ;    immediate restrict

: >label   ( addr -- )      here  | Create  swap ,  immediate
                4 hallot   >here 4- heap 4 cmove
                heap  last @   count $1F and + even !    dp !
                Does>  ( -- addr ) @
                 state @ IF  [compile] Literal  THEN ;
: Label        [ Assembler ] >here  [ Forth ] 1 and
               [ Assembler ] >allot    >here >label  Assembler ;

\ *** Block No. 5, Hexblock 5

\ Code generating primitives                           26oct86we

Variable >codes
| Create nrc ] c, , c@ here allot ! c! [

: nonrelocate   nrc >codes ! ;      nonrelocate

| : >exec   Create  c,
            Does>  c@  >codes @  +  @  execute ;

|   0 >exec >c,       |  2 >exec >,       |   4 >exec >c@
|   6 >exec >here     |  8 >exec >allot   | $0A >exec >!
| $0C >exec >c!




\ *** Block No. 6, Hexblock 6

\ 68000 Meta Assembler                                 04sep86we

| : ?,          IF >, THEN >, ;
| : 2,          >, >, ;
8 base !
Variable size
: .b            10000 size ! ;
: .w            30100 size ! ; .w
: .l            24600 size ! ;

| : Sz          Constant    Does> @  size @  and  or ;
00300 | Sz sz3            00400 | Sz sz4
04000 | Sz sz40           30000 | Sz sz300

| : long?       size @  24600 = ;
| : -sz1        long? IF  100 or  THEN ;

\ *** Block No. 7, Hexblock 7

\ addressing modes                                     09sep86we

| : Regs   10 0 DO  dup 1001 I * or  Constant  LOOP  drop ;
| : Mode    Constant    Does>  @ *swap  7007 and  or ;
0000 Regs     D0   D1   D2   D3   D4   D5   D6   D7
0110 Regs     A0   A1   A2   A3   A4   A5   A6   A7
0220 Mode     )         \ address register indirect
0330 Mode     )+        \ adr reg ind post-increment
0440 Mode     -)        \ adr reg ind pre-decrement
0550 Mode     D)        \ adr reg ind displaced
0660 Mode     (DI)      \ adr reg ind displaced indexed s.u.
0770 Constant #)        \ immediate address
1771 Constant L#)       \ immediate long address
2772 Constant pcD)      \ pc relative displaced
3773 Constant (pcDI)    \ pc relative displaced indexed
4774 Constant #         \ immediate data

\ *** Block No. 8, Hexblock 8

\ fields and register assignments                      08sep86we

| : Field      Constant   Does>  @ and ;
7000 | Field rd           0007 | Field rs
0070 | Field ms           0077 | Field eas
0377 | Field low
| : dn?   ( ea -- ea flag )           dup  ms 0= ;
| : src   ( ea instr -- ea instr' )   over  eas or ;
| : dst   ( ea instr -- ea instr' )   *swap  rd  or ;

| : ??dn  ( mod -- mod )  dn? 0= abort" needs Data-Register" ;
| : ??an  ( mod -- mod )  dup ms 1 =
                            abort" needs Adress-Register" ;

A6 Constant SP      A5 Constant RP      A4 Constant IP
A3 Constant FP

\ *** Block No. 9, Hexblock 9

\ extended addressing                                  09sep86we
: DI)        (DI)  size @ *swap ;
: pcDI)    (pcDI)  size @ *swap ;

| : double?  ( mode -- flag)    dup  L#) =  *swap
                                # =  long? and  or ;
| : index?   ( {n} mode -- {m} mode )
   dup >r  dup 0770 and  A0 (DI) =  *swap  (pcDI) =  or
   IF   size @ >r  size !
        dup  rd 10 *  *swap  ms  IF  100000 or  THEN
        sz40 *swap low or   r> size !
   THEN  r> ;

| : more?   ( ea -- ea flag )   dup  ms 0040 > ;
| : ,more   ( ea -- )           more?
   IF  index?  double?  ?,  ELSE  drop  THEN ;

\ *** Block No. 10, Hexblock a

\ extended addressing  extras                          09sep86we

| Create extra   here 5 dup allot erase \ temporary storage area

| : extra?   ( {n} mode -- mode )   more?
    IF  >r  r@ index?  double?  extra 1+ *swap
        IF under ! 2+ !  2  ELSE  ! 1 THEN  extra c!  r>
    ELSE   0 extra !
    THEN  ;

| : ,extra   ( -- )   extra c@  ?dup
   IF   extra 1+ *swap 1 =
     IF  @ >,  ELSE  dup 2+ @ *swap @  2,  THEN  extra 5 erase
   THEN ;



\ *** Block No. 11, Hexblock b

\ immediates & address register specific               15jan86we
| : Imm   Constant      Does>  @ >r  extra? eas r> or
                              sz3 >, long? ?,  ,extra ; ( n ea)
0000 Imm ori            1000 Imm andi
2000 Imm subi           3000 Imm addi
5000 Imm eori           6000 Imm cmpi
| : Immsr   Constant    Does> @ sz3 2, ; ( n )
001074 Immsr andi>sr
005074 Immsr eori>sr
000074 Immsr ori>sr
| : Iq    Constant Does>  @ >r  extra?  eas *swap rs 1000 * or
                          r> or sz3 >, ,extra ;  ( n ea )
050000 Iq addq          050400 Iq subq
| : Ieaa   Constant  Does> @ dst src sz4 >, ,more ; ( ea an )
150300 Ieaa adda        130300 Ieaa cmpa
040700 Ieaa lea         110300 Ieaa suba

\ *** Block No. 12, Hexblock c

\ shifts, rotates, and bit manipulation                15jan86we
| : Isr    Constant  Does> @ >r dn?
   IF  *swap dn? IF  r> 40 or >r  ELSE drop *swap 1000 * THEN
       rd *swap rs or r> or 160000 or sz3 >,
   ELSE  dup eas 300 or r@ 400 and or r> 70 and 100 * or
         160000 or >, ,more
   THEN ;  ( dm dn ) ( m # dn ) ( ea )
400 Isr asl             000 Isr asr
410 Isr lsl             010 Isr lsr
420 Isr roxl            020 Isr roxr
430 Isr rol             030 Isr ror
| : Ibit   Constant  does> @ >r  extra?  dn?
   IF  rd src 400  ELSE  drop dup eas 4000  THEN
   or r> or >, ,extra ,more ;  ( ea dn ) ( ea n # )
000 Ibit btst           100 Ibit bchg
200 Ibit bclr           300 Ibit bset

\ *** Block No. 13, Hexblock d

\ branch, loop, and set conditionals                   15jan86we

| : Setclass    ' *swap 0 DO I over execute LOOP drop ;
| : Ibra   400 * 060000 or Constant    ( label )
          Does> @ *swap >here  2+ - dup abs 200 <
                IF  low or >,  ELSE  *swap 2,  THEN  ;
20 Setclass Ibra   bra bsr bhi bls bcc bcs bne beq
                   bvc bvs bpl bmi bge blt bgt ble
| : Idbr  400 * 050310 or Constant    ( label \ dn - )
          Does> @ *swap rs or >, >here - >, ;
20 Setclass Idbr   dxit dbra dbhi dbls dbcc dbcs dbne dbeq
                   dbvc dbvs dbpl dbmi dbge dblt dbgt dble
| : Iset    400 * 050300 or Constant    ( ea )
            Does> @ src >, ,more  ;
20 Setclass Iset   set sno shi sls scc scs sne seq
                   svc svs spl smi sge slt sgt sle

\ *** Block No. 14, Hexblock e

\ moves                                                15jan86we

: move       extra? 7700 and src sz300 >,
             ,more ,extra ;  ( ea ea )
: moveq      ??dn  rd *swap low or 070000 or >, ;  ( n dn )
: move>usp   ??an  rs 047140 or >, ;  ( an )
: move<usp   ??an  rs 047150 or >, ;  ( an )
: movem>
   extra? eas   044200 or -sz1 >, >, ,extra ;  ( n ea )
: movem<
   extra? eas   046200 or -sz1 >, >, ,extra ;  ( n ea )
: movep      dn? IF    rd *swap rs or 410 or
                 ELSE   rs rot rd or 610 or THEN  -sz1 2, ;
   ( dm d an ) ( d an dm )
: lmove      7700 and *swap eas or 20000 or >, ;
  ( long reg move )

\ *** Block No. 15, Hexblock f

\ odds and ends                                        15jan86we

: cmpm   rd *swap rs or 130410 or sz3 >, ;  ( an@+ am@+ )
: exg   dn? IF   *swap dn?  IF  140500 ELSE 140610 THEN >r
            ELSE *swap dn?  IF  140610 ELSE 140510 THEN >r *swap
            THEN  rs dst r> or >, ;  ( rn rm )
: ext    ??dn  rs 044200 or -sz1 >, ; ( dn )
: swap   ??dn  rs 044100 or >, ; ( dn )
: stop   47162 2, ; ( n )
: trap   17 and 47100 or >, ; ( n )
: link   ??an  rs 047120 or 2, ; ( n an )
: unlk   ??an  rs 047130 or >, ; ( an )
: eor    extra? eas dst sz3 130400 or >, ,extra ;  ( dn ea )
: cmp    ??dn  130000 dst src sz3 >, ,more ;  ( ea dn )



\ *** Block No. 16, Hexblock 10

\ arithmetic and logic                                 15jan86we
| : Ibcd   Constant  Does> @ dst over rs or  *swap ms
        IF  10 or  THEN  >, ;  ( dn dm ) ( an@- am@- )
140400 Ibcd abcd         100400 Ibcd sbcd
| : Idd   Constant  Does> @ dst over rs or  *swap ms
        IF  10 or  THEN  sz3 >, ;  ( dn dm ) ( an@- am@- )
150400 Idd addx         110400 Idd subx
| : Idea   Constant  Does> @ >r dn?  ( ea dn ) ( dn ea )
    IF  rd src r> or sz3 >, ,more
    ELSE  extra? eas dst 400 or r> or sz3 >, ,extra  THEN ;
150000 Idea add         110000 Idea sub
140000 Idea and         100000 Idea or
| : Iead   Constant  Does> @ >r  ??dn  r>  dst src
                           >, ,more ; ( ea dn)
040600 Iead chk       100300 Iead divu        100700 Iead divs
140300 Iead mulu      140700 Iead muls

\ *** Block No. 17, Hexblock 11

\ arithmetic and control                               15jan86we

| : Iea    Constant  Does>  @ src >, ,more ;  ( ea )
047200 Iea jsr          047300 Iea jmp
042300 Iea move>ccr
040300 Iea move<sr      043300 Iea move>sr
044000 Iea nbcd         044100 Iea pea
045300 Iea tas
| : Ieas  Constant   Does>  @ src sz3 >, ,more ;  ( ea )
041000 Ieas clr         043000 Ieas not
042000 Ieas neg         040000 Ieas negx
045000 Ieas tst
| : Icon  Constant   Does>  @  >, ;
47160 Icon reset        47161 Icon nop
47163 Icon rte          47165 Icon rts
47166 Icon trapv        47167 Icon rtr

\ *** Block No. 18, Hexblock 12

\ structured conditionals  +/- 256 bytes               15jan86we
: THEN   >here over 2+  -  *swap 1+ >c! ;
: IF      >, >here 2-  ;   hex
: ELSE    6000 IF  *swap  THEN ;
: BEGIN   >here  ;
: UNTIL   >, >here - >here 1-  >c!  ;
: AGAIN   6000 UNTIL ;
: WHILE   IF *swap ;
: REPEAT  AGAIN THEN ;
: DO      >here *swap ;
: LOOP    dbra ;
6600 Constant 0=   6700 Constant 0<>
6A00 Constant 0<   6B00 Constant 0>=
6C00 Constant <    6D00 Constant >=
6E00 Constant <=   6F00 Constant >
6500 Constant CC   6400 Constant CS
