
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
