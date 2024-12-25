
\ *** Block No. 0, Hexblock 0

 \                                                       05Jul86
















\ *** Block No. 1, Hexblock 1

\ Target compiler loadscr                             UH 07Jun86
\ Idea and first Implementation by ks/bp
\ Implemented on 6502  by ks/bp
\ ultraFORTH83-Version by bp/we
\ Atari 520 ST - Version by we
\ CP/M 2.2 Version by UH

Onlyforth hex     Assembler nonrelocate
Vocabulary Ttools
Vocabulary Defining
 1 10 +thru   \ Target compiler
11 13 +thru   \ Target Tools
14 16 +thru   \ Redefinitions
save  17 20 +thru  \ Predefinitions

Onlyforth

\ *** Block No. 2, Hexblock 2

\ Target header pointers                              UH 26Mar88

Create   lastname $20 allot
Variable tdp                  : there  tdp @ ;
Variable displace
Variable image
Variable ?thead               ?thead off
Variable tlast                tlast  off
Variable glast'               glast' off
Variable tdoes>
Variable >in:
Variable tvoc                 tvoc off
Variable tvoc-link            tvoc-link off
0 | Constant <forw>
0 | Constant <res>
| : Is> ( cfa -- )  [compile] Does> here 3 - swap >body ! 0 ] ;

\ *** Block No. 3, Hexblock 3

\ Image and byteorder                                 UH 26Mar88

Code c+! ( 8b addr -- )
   H pop   D pop   E A mov   M add   A M mov   Next   end-code

Code /block  ( addr -- +n blk )
   H pop    L E mov    H A mov    3 ani    A D mov
   H A mov   $FC ani    rrc rrc  A L mov   0 H mvi   dpush jmp
end-code

: >image    ( addr1 - addr2 )
   displace @ ( -  /block   image @ +  block ) + ;

: >heap  ( from quan - )  dup hallot heap swap cmove ;
\\ : c+!   ( 8b addr -- )        dup c@ rot + swap c! ;
   : /block ( addr -- +n blk )  b/blk /mod ;

\ *** Block No. 4, Hexblock 4

\ Ghost-creating                                      UH 26Mar88

| : (make.ghost  ( str -- cfa.ghost )  dp push
     count  dup 1 $1F uwithin not Abort" invalid Ghostname"
     here 2+ place
     here state @          \ address of link field
     IF  context @  ELSE current  THEN  @  under @ ,  \ link
     1 here c+!  here c@  allot  bl c,  \ name
     here over - swap  \ offset to codefield
     <forw> , 0 , 0 ,           \ code and parameter field
     here over - >heap          \ move to heap
     heap rot !                 \ link
     heap  +  ;                 \ codefield address

| : Make.Ghost ( -- cfa.ghost )  name (make.ghost ;


\ *** Block No. 5, Hexblock 5

\ ghost words                                         UH 28Apr88

: gfind  ( string - cfa tf / string ff )
   >r bl r@ count + c!  1 r@ c+!  r@ find  -1 r> c+! ;

: (ghost   ( string -- cfa )  gfind  ?exit  (make.ghost ;

: ghost ( -- cfa )  name (ghost ;

: gdoes>  ( cfa.ghost - cfa.does )  dp push
    4+ dup @    IF  @ exit  THEN   \ defined
    here <forw> , 0 ,  4 >heap     \ forward-chain
    heap dup rot ! ;               \ forward-link




\ *** Block No. 6, Hexblock 6

\ ghost utilities                                    2UH 26Mar88

: g' ( -- cfa.ghost )  name gfind 0= abort" ?" ;

| : .ghost-type ( cfa.ghost -- ) @
      <forw> case? IF ." forward" exit THEN
      <res> - Abort" type unknown" ." resolved " ;

| : .does-type ( cfa.does -- )  @
      <forw> case? IF ." forward-define" exit THEN
      <res> - Abort" does-type unknown"  ." resolved-define" ;

: '. ( -- ) g'         dup .ghost-type  dup 2+ @ 5 u.r
     4+ @ ?dup 0=exit  dup  .does-type      2+ @ 5 u.r  ;

' ' Alias h'

\ *** Block No. 7, Hexblock 7

\ .unresolved                                         UH 26Mar88

| : forward? ( cfa -- f )  dup @  <forw> =  swap 2+ @ and ;
| : ghost?   ( nfa -- f )  count $1F and + 1- c@ bl = ;

| : unresolved? ( addr - f ) 2+
    dup     ghost? not IF drop false exit THEN
    name> dup forward? IF drop true  exit THEN
    4+ @      forward? ;

| : unresolved-words  ( thread -- )  BEGIN @ ?dup WHILE
     dup unresolved?  IF dup 2+ .name ?cr  THEN REPEAT ;

: .unresolved ( -- ) voc-link @
    BEGIN dup 4- unresolved-words  @ ?dup 0= UNTIL ;


\ *** Block No. 8, Hexblock 8

\ Extending Vocabularys for Target-Compilation       2UH 26Mar88

: Vocabulary       Vocabulary 0 , here tvoc @ , tvoc ! ;

Vocabulary Transient    tvoc off

Root definitions

: T Transient ;   immediate
: H Forth     ;   immediate

OnlyForth





\ *** Block No. 9, Hexblock 9

\ Transient primitives                                UH 26Mar88

Code byte>   ( 8bl 8bh -- 16b )
   D pop  H pop  E H mov  hpush jmp end-code
Code >byte   ( 16b -- 8bh 8bl )
   H pop  H E mov  0 H mvi  H D mov  dpush jmp end-code

Transient definitions
: c@ ( addr -- 8b )  H >image  c@ ;
: c! ( 8b addr -- )  H >image  c!  ( update ) ;
: @  ( addr -- n  )  dup T c@ H  swap 1+  T c@ H  byte> ;
: !  ( n addr --  )  >r  >byte r@  T c! H  r> 1+  T c! H ;
: cmove ( from.mem to.target quan -)
     bounds ?DO  dup  H c@   I  T c! H  1+ LOOP drop ;
: on  ( addr -- )  true  swap T ! H ;
: off ( addr -- )  false swap T ! H ;

\ *** Block No. 10, Hexblock a

\ Transient primitives                                UH 26Mar88

: here  ( -- taddr )   there ;
: allot ( n -- )  Tdp +! ;
: c, ( c -- )  T here c! 1 allot H ;
: ,  ( n -- )  T here  ! 2 allot H ;

: ," ( -- )  Ascii " parse
     dup T c,  under  here swap cmove  allot H ;

: fill ( addr len c -- )
   -rot bounds ?DO dup I  T c! H  LOOP drop ;

: erase ( addr len -- )  0  T fill H ;
: blank ( addr len -- )  bl T fill H ;
: here! ( addr -- )  H tdp ! ;

\ *** Block No. 11, Hexblock b

\ Resolving                                           UH 26Mar88

Forth definitions

: resolve ( cfa.ghost cfa.target -- )
   2dup  swap >body   dup @ >r   !   over @ <res> =
   IF drop >name space .name ." exists" ?cr rdrop exit THEN
   r> swap >r  <res> rot !  ?dup 0= IF rdrop exit THEN
   BEGIN dup T @ H 2dup = abort" resolve loop"
         r@ rot T ! H  ?dup 0= UNTIL rdrop ;

: resdoes> ( cfa.ghost cfa.target -- )
    swap gdoes> dup @ <res> = IF 2+ ! exit THEN swap resolve ;

' <forw> Is> ( -- )  dup @ there rot ! T , H ; \ forward link
' <res>  Is> ( -- )  @ T , H ;  \ compile target.cfa

\ *** Block No. 12, Hexblock c

\ move-threads                                        UH 26Mar88

: move-threads           Tvoc @ Tvoc-link @
  BEGIN  over ?dup
  WHILE  2- @ over 2- T ! @ H swap @ swap  REPEAT
  error" some undef. Target-Vocs left" drop ;

| : tlatest ( - addr)       Current @  6  + ;


: save-target \ filename
    $100 dup >image  there rot -  savefile ;





\ *** Block No. 13, Hexblock d

\ compiling names into targ.                          UH 26Mar88

| : viewfield ( -- n ) H blk @  $200 + ; \ in File #1

: (theader  ( -- ) ?thead @ IF  1 ?thead +! exit THEN
   >in push
   name dup c@  1 $20 uwithin  not abort" invalid Targetname"
   viewfield T ,
   H there  tlatest @ T , H  tlatest !   \ link
   there dup tlast !
   over c@ 1+   dup T allot   cmove H ;

: Theader ( -- )  tlast off
    (theader Ghost dup glast' !  there resolve ;



\ *** Block No. 14, Hexblock e

\ prebuild defining words                          bp2UH 26Mar88

| : executable? ( adr - adr f )       dup ;
| : tpfa,                             there , ;

| : (prebuild ( cfa.adr -- )  >in push  Create  here 2- ! ;

: prebuild ( adr 0.from.: - 0 )  0 ?pairs
    executable? dup >r
    IF  [compile] Literal compile (prebuild  ELSE  drop  THEN
    compile Theader  Ghost gdoes> ,
    r> IF  compile tpfa,  THEN  0 ;    immediate restrict





\ *** Block No. 15, Hexblock f

\ code portion of def.words                        bp2UH 26Mar88

: dummy      0 ;

: DO> ( - adr.of.jmp.dodoes> 0 )
    [compile] Does>  here 3 -  compile @  0 ] ;











\ *** Block No. 16, Hexblock 10

\ The Target-Assembler                                UH 26Mar88


Forth definitions
| Create relocate  ]  T c, , c@ here allot ! c! H  [

Transient definitions

: Assembler      H [ Assembler ] relocate >codes ! Assembler ;
: >label ( 16b -)   H >in @ name gfind rot >in !
                    IF over resolve dup THEN drop Constant ;
: Label          H there T >label Assembler H ;
: Code           H Theader there 2+ T , Assembler H ;




\ *** Block No. 17, Hexblock 11

\ immed. restr. ' \ compile                        bp2UH 26Mar88

: ?pairs   ( n1 n2 -- )    H - abort" unstructured" ;
: >mark    ( - addr)       H there T 0 , H ;
: >resolve ( addr -)       H there over - swap T ! H ;
: <mark    ( - addr)       H there ;
: <resolve ( addr -)       H there - T , H ;
: immediate  H Tlast @ ?dup 0=exit  dup T c@ $40 or swap c! H ;
: restrict   H Tlast @ ?dup 0=exit  dup T c@ $80 or swap c! H ;
: ' ( <name> - cfa)        H g' dup @ <res> - abort" ?" 2+ @ ;
: |                        H ?thead @ ?exit ?thead on ;
: compile                  H Ghost , ; immediate restrict





\ *** Block No. 18, Hexblock 12

\ Target tools                                        UH 26Mar88
Onlyforth Ttools also definitions

| : ttype ( adr n -)   bounds ?DO I T c@ H  dup
   bl > IF  emit  ELSE  drop ascii . emit  THEN  LOOP ;

: .name ( nfa -)       ?dup IF dup 1+ swap T c@ H $1F and ttype
                            ELSE ." ??? " THEN space ?cr ;

| : nfa? ( cfa lfa - nfa / cfa ff)
  BEGIN  dup  WHILE  2dup 2+ dup T c@ H $1F and + 1+ =
     IF  2+ nip exit  THEN   T @ H REPEAT ;

: >name ( cfa - nfa / ff)
   Tvoc BEGIN  @ dup  WHILE  under 2- @ nfa? ?dup
     IF nip exit THEN   swap REPEAT  nip ;

\ *** Block No. 19, Hexblock 13

\ Ttools for decompiling                           ks29jun85we

| : ?:        dup 4 u.r ." :" ;
| : @?        dup T @ H 6 u.r ;
| : c?        dup T c@ H 3 .r ;

: s ( adr - adr+)         ?: space c? 3 spaces
 dup 1+ over T c@ H ttype dup T c@ H + 1+ ;

: n ( adr - adr+2)        ?: @? 2 spaces
 dup T @ H [ Ttools ] >name .name H 2+ ;

: d ( adr n - adr+n)      2dup swap ?: swap 0 DO  c? 1+  LOOP
                          2 spaces -rot ttype ;



\ *** Block No. 20, Hexblock 14

\ Tools for decompiling                            bp204dec85we

: l ( adr - adr+2)        ?: 5 spaces @? 2+ ;

: c ( adr - adr+1)        1 d ;

: b ( adr - adr+1)        ?: @? dup T @ H over + 5 u.r  2+ ;

: dump ( adr n -)         bounds ?DO cr I 10 d drop stop?
                            IF LEAVE THEN 10 +LOOP ;

: view                    T ' H [ Ttools ] >name ?dup
                            IF  4 - T @ H list  THEN ;




\ *** Block No. 21, Hexblock 15

\ reinterpretation def.-words                         UH 26Mar88

Onlyforth

: redefinition ( -- )  tdoes> @ 0=exit
   >in push [ ' parser >body ] Literal push
   state push   context push
   >in: @ >in ! name [ ' Transient 2+ ] Literal (find nip ?exit
   cr ." Redefinition: " here .name
   >in: @ >in ! : Defining interpret  tdoes> off ;







\ *** Block No. 22, Hexblock 16

\ Create..does> structure                                27Apr86

| : (;tcode      Tlast @ dup T c@ + 1+ ! H rdrop ;

| : changecfa    compile lit tdoes> @ , compile (;tcode ;

Defining definitions

: ;code          0 ?pairs changecfa reveal rdrop rdrop ;
                                     immediate restrict

Defining ' ;code Alias does>         immediate restrict

: ;              [compile] ; rdrop rdrop ; immediate restrict



\ *** Block No. 23, Hexblock 17

\ redefinition conditionals                        bp27jun85we

' DO     Alias DO        immediate restrict
' ?DO    Alias ?DO       immediate restrict
' LOOP   Alias LOOP      immediate restrict
' IF     Alias IF        immediate restrict
' THEN   Alias THEN      immediate restrict
' ELSE   Alias ELSE      immediate restrict
' BEGIN  Alias BEGIN     immediate restrict
' UNTIL  Alias UNTIL     immediate restrict
' WHILE  Alias WHILE     immediate restrict
' REPEAT Alias REPEAT    immediate restrict





\ *** Block No. 24, Hexblock 18

\ clear Liter. Ascii ['] ."                           UH 26Mar88

Onlyforth Transient definitions

: clear  True abort" There are ghosts" ;
: Literal ( n -) H dup $FF00 and IF T compile lit , H exit THEN
                   T compile clit c, H ; immediate
: Ascii  H bl word 1+ c@
          state @ 0=exit T [compile] Literal H ; immediate
: [']   T ' [compile] Literal H ; immediate restrict
: "     T compile (" ," H ;       immediate restrict
: ."    T compile (." ," H ;      immediate restrict

: even   H ; immediate   \ machen nichts beim 8080
: align  H ; immediate
: halign H ; immediate

\ *** Block No. 25, Hexblock 19

\ Target compilation  ]  [                         bp0UH 26Mar88

Forth definitions

: tcompile ( str -- )  count lastname place
   lastname find ?dup
   IF 0> IF  execute exit  THEN  drop lastname THEN
   gfind IF  execute exit  THEN
   number? ?dup
   IF 0> IF  swap T [compile] Literal  THEN
                    [compile] Literal H exit THEN
   (ghost execute ;

Transient definitions
: ]           H State on ['] tcompile is parser ;


\ *** Block No. 26, Hexblock 1a

\ Target conditionals                              bp27jun85we

: IF           T compile ?branch >mark H 1 ; immediate restrict
: THEN         abs 1 T ?pairs >resolve H ;   immediate restrict
: ELSE         T 1 ?pairs compile branch >mark swap >resolve
               H -1 ;                        immediate restrict
: BEGIN        T <mark H 2 ;                 immediate restrict
: WHILE        T 2 ?pairs 2 compile ?branch >mark -2 H 2swap ;
                                             immediate restrict
| : (repeat    T 2 ?pairs <resolve H BEGIN dup -2 =
                    WHILE drop T >resolve H REPEAT ;
: UNTIL        T compile ?branch (repeat H ; immediate restrict
: REPEAT       T compile branch (repeat H ;  immediate restrict




\ *** Block No. 27, Hexblock 1b

\ Target conditionals                              bp27jun85we

: DO             T compile (do >mark H 3 ;  immediate restrict
: ?DO            T compile (?do >mark H 3 ; immediate restrict
: LOOP           T 3 ?pairs compile (loop compile endloop
                 >resolve H ;               immediate restrict
: +LOOP          T 3 ?pairs compile (+loop compile endloop
                 >resolve H ;               immediate restrict









\ *** Block No. 28, Hexblock 1c

\ predefinitions                                   bp27jun85we

: abort"                T compile (abort" ," H ; immediate
: error"                T compile (err" ," H ;   immediate

Forth definitions

Variable torigin
Variable tudp           0 tudp !

: >user                 T c@ H torigin @ + ;






\ *** Block No. 29, Hexblock 1d

\ Datatypes                                        bp2UH 07Nov87

Transient definitions
: origin!            H torigin ! ;
: user' ( - 8b)      T ' 2 + c@ H ;
: uallot ( n -)      H tudp @ swap tudp +! ;

                     DO> >user ;
: User               prebuild User 2 T uallot c, ;

                     DO> ;
: Create             prebuild (create ;

                     DO> T @ H ;
: Constant           prebuild Constant T , ;
: Variable           Create 2 T allot ;

\ *** Block No. 30, Hexblock 1e

\ Datatypes                                           UH 07Nov87

dummy
: Vocabulary
 H >in @ Vocabulary >in ! T prebuild Vocabulary 0 , 0 ,
 here H tvoc-link @ T , H tvoc-link ! ;


                dummy
: (create       prebuild (create ;







\ *** Block No. 31, Hexblock 1f

\ target defining words                                  27Apr86

              Do> ;
: Defer       prebuild Defer 2 T allot ;
: Is          T ' H >body State @ IF T compile (is , H
                                  ELSE T ! H THEN ; immediate
| : dodoes>   T compile (;code H Glast' @
              there resdoes> there tdoes> ! ;

: ;code       0 T ?pairs dodoes> Assembler H [compile] [
              redefinition ; immediate restrict
: does>       T dodoes> $CD c,
              compile (dodoes> H ;      immediate restrict




\ *** Block No. 32, Hexblock 20

\ :  Alias  ;                                        bUH 07Jun86

dummy
: :      H tdoes> off >in @ >in: ! T prebuild :
         H current @ context ! T ] H 0 ;

: Create:  Create  H current @ context ! T ] H 0 ;

: Alias ( n -- )       H Tlast off (theader Ghost over resolve
                       tlast @ T c@ H 20 or tlast @ T c! , H ;

: ;      T 0 ?pairs compile unnest [compile] [ H redefinition ;
                                            immediate restrict




\ *** Block No. 33, Hexblock 21

\ predefinitions                                      UH 26Mar88

: compile             T compile compile H ; immediate restrict
: Host                H Onlyforth Ttools also ;
: Compiler            T Host H Transient also definitions ;
: [compile]           H ghost execute ; immediate restrict
\  : Onlypatch           H there 3 - 0 tdoes> ! 0 ;

Onlyforth
: Target              Onlyforth Transient also definitions ;

Transient definitions
Ghost c, drop



