
\ *** Block No. 0, Hexblock 0

\\         *** volksFORTH-84 Target-Compiler ***     cas 26jan06

This Target Compiler can be used to create a new Forth System
using the Sourcecode 6502F82.FB.













\ *** Block No. 1, Hexblock 1

\ Target compiler loadscr                              09sep86we
\ Idea and first Implementation by ks/bp
\ Implemented on 6502  by ks/bp
\ ultraFORTH83-Version by bp/we
\ Atari 520 ST - Version by we
Onlyforth      Assembler nonrelocate
07 Constant imagepage     \ Virtual memory bank
Vocabulary Ttools
Vocabulary Defining
: .stat .blk .s ;     ' .stat Is .status
\ : 65( [compile] ( ; immediate
: 65( ; immediate : ) ; immediate \ cpu-addressing|lbyte|hbyte|
  1 $14 +thru   \ Target compiler
$15 $17 +thru   \ Target Tools
$18 $1A +thru   \ Redefinitions
save  $1B $24 +thru  \ Predefinitions

\ *** Block No. 2, Hexblock 2

\ Target header pointers                             bp05mar86we

Variable tdp                  : there  tdp @ ;
Variable displace
Variable ?thead               0 ?thead !
Variable tlast                0 tlast !
Variable glast'               0 glast' !
Variable tdoes>
Variable >in:
Variable tvoc                 0 tvoc !
Variable tvoc-link            0 tvoc-link !
Variable tnext-link           0 tnext-link !

: c+!   ( 8b addr -- )        dup c@  rot +  swap  c! ;



\ *** Block No. 3, Hexblock 3

\ Image and byteorder                                  15sep86we

: >image    ( addr1 - addr2 )      displace @  -  ;

: >heap  ( from quan - )
    heap  over -  1 and +       \ 68000-align
    dup hallot heap swap cmove ;
\\
: >ascii 2drop ;                ' noop Alias C64>ascii

Code Lc@  ( laddr -- 8b )
.l SP )+ A0 move   .w D0 clr   .b A0 ) D0 move
.w D0 SP -) move   Next  end-code
Code Lc!  ( 8b addr -- )
.l SP )+ A0 move  .w SP )+ D0 move  .b D0 A0 ) move
Next end-code

\ *** Block No. 4, Hexblock 4

\ Ghost-creating                                       05mar86we

0 | Constant <forw>          0 | Constant <res>

| : Make.ghost  ( - cfa.ghost )
     here  dup 1 and allot   here
     state @  IF  context @  ELSE current  THEN  @
     dup @ ,  name
     dup c@  1 $1F uwithin  not abort" inval.Gname"
     dup c@ 1+  over c!
     c@ dup 1+ allot   1 and  0= IF  bl c,  THEN
     here 2 pick -  -rot
     <forw> , 0 , 0 ,
     swap   here over -  >heap
     heap swap !             swap dp !
     heap  +  ;

\ *** Block No. 5, Hexblock 5

\ ghost words                                          05mar86we

: gfind  ( string - cfa tf / string ff )
   dup  count + 1+  bl swap c!
   dup >r  1 over c+!  find  -1 r> c+! ;

: ghost   ( - cfa )
   >in @  name gfind   IF nip exit THEN
   drop  >in !  Make.ghost ;

: Word,              ghost execute ;

: gdoes>  ( cfa.ghost - cfa.does )
   4+ dup @   IF  @ exit  THEN
   here  dup <forw> ,  0 ,  4 >heap
   dp !  heap dup rot ! ;

\ *** Block No. 6, Hexblock 6

\ ghost utilities                                      04dec85we

: g'         name gfind 0= abort" ?" ;

: '.
   g' dup @ <forw> case?
      IF ." forw"  ELSE  <res> - abort" ??" ."  res"  THEN
   2+ dup @ 5 u.r
   2+ @ ?dup
      IF dup @ <forw> case?
         IF ."  fdef"  ELSE <res> - abort"  ??" ."  rdef"  THEN
   2+ @ 5 u.r THEN ;

' ' Alias h'



\ *** Block No. 7, Hexblock 7

\ .unresolved                                          05mar86we

| : forward? ( cfa - cfa / exit&true )
 dup @  <forw> =   over 2+ @ and  IF drop true rdrop exit THEN ;

| : unresolved? ( addr - f )
     2+ dup c@ $1F and over + c@ BL =
       IF    name> forward? 4+ @  dup IF  forward?  THEN
       THEN  drop  false ;

| : unresolved-words
     BEGIN @ ?dup WHILE dup unresolved?
          IF dup  2+ .name ?cr  THEN    REPEAT ;

: .unresolved       voc-link @
    BEGIN dup 4- unresolved-words  @ ?dup 0= UNTIL ;

\ *** Block No. 8, Hexblock 8

\ Extending Vocabularys for Target-Compilation         05mar86we

: Vocabulary       Vocabulary 0 ,  here  tvoc @ ,  tvoc ! ;

Vocabulary Transient    0 tvoc !

Only definitions Forth also

: T Transient ;   immediate
: H Forth     ;   immediate

definitions





\ *** Block No. 9, Hexblock 9

\ Transient primitives                                 05mar86we

Code byte>   ( 8bh 8bl -- 16b )
     SP )+ D1 move   SP ) D0 move   8 # D0 lsl   .b D1 D0 move
  .w D0 SP ) move    Next end-code
Code >byte   ( 16b -- 8bl 8bh )
   SP )+ D0 move   D0 D1 move   $FF D0 andi   8 # D1 lsr
   D0 SP -) move   D1 SP -) move    Next end-code

Transient definitions
: c@    H >image  imagepage lc@ ;
: c!    H >image  imagepage lc! ;
: @     dup T c@  swap 1+ T c@ 65( swap ) byte> ;
: !     >r  >byte 65( swap )  r@ T c!   r> 1+ T c! ;
: cmove ( from.mem to.target quan -)
   bounds ?DO  dup H c@ I  T c! H  1+ LOOP drop ;

\ *** Block No. 10, Hexblock a

\ Transient primitives                               bp05mar86we

: here   there ;
: allot  Tdp +! ;
: c,     T here c! 1 allot H ;
: ,      T here  ! 2 allot H ;

: ,"        Ascii " parse  dup T c,
   under there swap cmove
   .( dup 1 and 0= IF 1+ THEN ) allot  H ;

: fill ( addr quan 8b -)
   -rot bounds ?DO  dup I T c! H  LOOP  drop ;
: erase        0 T fill ;
: blank       bl T fill ;
: here!        H Tdp ! ;

\ *** Block No. 11, Hexblock b

\ Resolving                                            08dec85we
Forth definitions
: resolve ( cfa.ghost cfa.target -)
   over dup @ <res> =
   IF  space dup >name .name ." exists " ?cr 2+ ! drop exit THEN
   >r >r 2+ @ ?dup
   IF   BEGIN dup T @ H 2dup = abort" resolve loop" r@ rot T !
              H ?dup 0= UNTIL
   THEN  r> r> <res> over ! 2+ ! ;

: resdoes> ( cfa.ghost cfa.target -)
   swap gdoes> dup @ <res> = IF 2+ ! exit THEN swap resolve ;
] Does> [ here 4- 0 ] dup @ there rot ! T , H ;
' <forw> >body !
] Does> [ here 4- 0 ] @ T , H ;
' <res> >body !

\ *** Block No. 12, Hexblock c

\ move-threads  68000-align                          cas 26jan06

: move-threads           Tvoc @  Tvoc-link @
   BEGIN  over ?dup
   WHILE  2- @ over 2-  T ! @ H  swap @  swap  REPEAT
   error" some undef. Target-Vocs left" drop ;

| : tlatest ( - addr)           current @ 6 + ;

\\
not used for the 6502 architecture

| : 68000-talign ( cfa -- )     1 and IF  1 T allot H  THEN ;




\ *** Block No. 13, Hexblock d

\ save-target                                          09sep86we

Dos definitions

Code (filewrite  ( buff len handle -- n)
   SP )+ D0 move   .l D2 clr  .w  SP )+ D2 move
   .l  0 imagepage # D1 move    .w  SP )+ D1 move
   .l  D1 A7 -) move           \ buffer adress
   .l  D2 A7 -) move           \ buffer length
   .w  D0 A7 -) move           \ handle
    $40 # A7 -) move           \ call  WRITE
    1 trap     $0C # A7 adda
   .w  D0 SP -) move     Next  end-code   Forth definitions




\ *** Block No. 14, Hexblock e

\ save Target-System                                   09sep86we

: save-target     [ Dos ]
   bl word count   dup     0= abort" missing filename"
   over + off    (createfile  dup >r   0< abort" no device "
   T here $1C - 4 ! 0 , 0 , H  [ Dos ]  \ Programm header
   0  there r@ (filewrite  there - abort" write error"
   r> (closefile  0< abort" close error" ;









\ *** Block No. 15, Hexblock f

\\ 6502-ALIGN  ?HEAD  \        08SEP84BP)

| : 6502-align/1   ( adr -- adr' ) dup  0FF and  0FF =  - ;


| : 6502-align/2   ( lfa -- lfa )
   there  0FF and 0FF =
   IF  dup dup 1+  there over - 1+ cmove>  \ lfa now invalid
       1 tlast +! 1 tallot  THEN  ;








\ *** Block No. 16, Hexblock 10

\\ WARNING   CREATE            30DEC84BP)

VARIABLE WARNING  0 WARNING !

| : EXISTS?
    WARNING @ ?EXIT
    LAST @  CURRENT @  (FIND  NIP
    IF SPACE  LAST @ .NAME ." EXISTS " ?CR THEN  ;

: CREATE  HERE BLK @ ,  CURRENT @ @ ,
    NAME  C@ DUP 1 020  UWITHIN NOT  ABORT" INVALID NAME"
    HERE  LAST ! 1+ ALLOT  EXISTS? ?HEAD @
    IF 1 ?HEAD +!  DUP  6502-ALIGN/1 , \ POINTER TO CODE
       HEAPMOVE 020 FLAG! 6502-ALIGN/1 DP !
    ELSE  6502-ALIGN/2  DROP THEN  REVEAL  0 ,
    ;CODE  DOCREATE JMP END-CODE

\ *** Block No. 17, Hexblock 11

\ compiling names into targ.                           05mar86we

: (theader
   ?thead @ IF  1 ?thead +!
   there $FF and $FF = IF 1 T allot H THEN exit  THEN
   >in @  name  swap >in !
   dup c@  1 $20 uwithin  not abort" inval. Tname"
   dup c@ 3 + there + $FF and $FF =
   there 2+ $FF and $FF = or IF 1 T allot H THEN
   blk @ T ,   H there tlatest dup @ T , H !   there dup tlast !
   over c@ 1+ .( even ) dup T allot   cmove H ;

: Theader          tlast off
   (theader Ghost dup glast' !
   there resolve ;


\ *** Block No. 18, Hexblock 12

\ prebuild defining words                            bp27jun85we

| : executable? ( adr - adr f )       dup ;
| : tpfa,                             there , ;
| : (prebuild ( cfa.adr -- )
     >in @ Create >in ! here 2- ! ;

: prebuild ( adr 0.from.: - 0 )
   0 ?pairs executable? dup >r
    IF  [compile] Literal compile (prebuild  ELSE  drop  THEN
   compile Theader  Ghost gdoes> ,
   r> IF  compile tpfa,  THEN  0 ;    immediate restrict





\ *** Block No. 19, Hexblock 13

\ code portion of def.words                          bp11sep86we

: dummy      0 ;

: DO> ( - adr.of.jmp.dodoes> 0 )
   [compile] Does>  here 4-  compile @  0 ] ;











\ *** Block No. 20, Hexblock 14

\ the 68000 Assembler                                  11sep86we

Forth definitions
| Create relocate  ]  T c, , c@ here allot ! c! H  [

Transient definitions

: Assembler    H [ Tassembler ] relocate >codes ! Tassembler ;
: >label ( 16b -)   H >in @ name gfind rot >in !
                      IF over resolve dup THEN drop Constant ;
: Label     T .( here 1 and allot ) here >label Assembler H ;
: Code           H Theader there 2+ T , Assembler H ;





\ *** Block No. 21, Hexblock 15

\ immed. restr. ' \ compile                          bp05mar86we

: ?pairs   ( n1 n2 -- )   H - abort" unstructured" ;
: >mark    ( - addr )     H there  T 0 , H ;
: >resolve ( addr - )     H there over - swap  T ! H ;
: <mark    ( - addr )     H there ;
: <resolve ( addr - )     H there -  T , H ;
: immediate               H Tlast @ ?dup
                          IF  dup T c@ $40 or swap c! H  THEN ;
: restrict                H Tlast @ ?dup
                          IF  dup T c@ $80 or swap c! H  THEN ;
: ' ( <name> - cfa )      H g' dup @ <res> - abort" ?" 2+ @ ;
: |                       H ?thead @ ?exit  ?thead on ;
: compile                 H Ghost , ; immediate restrict



\ *** Block No. 22, Hexblock 16

\ Target tools                                       ks05mar86we

Onlyforth Ttools also definitions

| : ttype ( adr n -)   bounds ?DO  I  T c@ H  dup
   bl > IF  emit  ELSE  drop  Ascii . emit  THEN  LOOP ;
: .name ( nfa -)       ?dup IF  dup 1+ swap T c@ H $1F and ttype
                            ELSE ." ??? " THEN space ?cr ;
| : nfa? ( cfa lfa - nfa / cfa ff)
  BEGIN  dup  WHILE  2dup 2+ dup T c@ H $1F and + 1+ .( even ) =
                                  IF  2+ nip exit  THEN
                     T @ H REPEAT ;
: >name ( cfa - nfa / ff)
 Tvoc BEGIN  @ dup  WHILE  under 2- @ nfa? ?dup
                                  IF nip exit THEN
                           swap REPEAT  nip ;

\ *** Block No. 23, Hexblock 17

\ Ttools for decompiling                             ks05mar86we

| : ?:        dup  4 u.r ." :" ;
| : @?        dup  T  @ H  6 u.r ;
| : c?        dup  T c@ H  3 .r ;

: s ( addr - addr+ )         ?: space c? 3 spaces
 dup 1+ over  T c@ H  ttype dup  T c@ H  + 1+ ;

: n ( addr - addr+2 )        ?: @? 2 spaces
 dup  T @ H  [ Ttools ] >name .name  H 2+ ;

: d ( addr n - addr+n )      2dup swap ?: swap 0 DO  c? 1+  LOOP
                             2 spaces -rot ttype ;



\ *** Block No. 24, Hexblock 18

\ Tools for decompiling                              bp05mar86we

: l ( addr -- addr+2 )    ?: 5 spaces @? 2+ ;

: c ( addr -- addr+1 )    1 d ;

: b ( addr -- addr+1 )    ?: @? dup T @ H over + 5 u.r  2+ ;

: dump ( adr n -)         bounds ?DO  cr I $10 d drop
                          stop? IF  LEAVE  THEN  $10 +LOOP ;

: view                    T ' H [ Ttools ] >name ?dup
                            IF  4-  T @ H  l  THEN ;




\ *** Block No. 25, Hexblock 19

\ reinterpretation def.-words                          05mar86we

Onlyforth

: redefinition
   tdoes> @ IF  >in push  [ ' >interpret >body ] Literal push
               state push  context push  >in: @ >in !
               name [ ' Transient 2+ ] Literal (find nip 0=
                 IF cr ." Redefinition: " here .name
                    >in: @ >in !  :  Defining interpret THEN
            THEN  0 tdoes> ! ;






\ *** Block No. 26, Hexblock 1a

\ Create..does> structure                            bp05mar86we

| : (;tcode
     Tlast @ dup  T c@ .( dup 1 and - ) 1+ + !  H rdrop ;
| : changecfa    compile lit tdoes> @ , compile (;tcode ;

Defining definitions

: ;code          0 ?pairs changecfa reveal rdrop ;
                                     immediate restrict

Defining ' ;code Alias does>         immediate restrict

: ;              [compile] ; rdrop ; immediate restrict



\ *** Block No. 27, Hexblock 1b

\ redefinition conditionals                          bp27jun85we

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





\ *** Block No. 28, Hexblock 1c

\ clear Liter. Ascii ['] ."                          bp05mar86we

Onlyforth Transient definitions

: clear            true abort" There are ghosts" ;
: Literal ( n -)   T compile lit  , H ;               immediate
: Ascii            H bl word 1+ c@  state @
                      IF T [compile] Literal H THEN ; immediate
: [']              T ' [compile] Literal H ; immediate restrict
: "                T compile (" ,"  H ;      immediate restrict
: ."               T compile (." ," H ;      immediate restrict






\ *** Block No. 29, Hexblock 1d

\ Target compilation  ]  [                           bp05mar86we

Forth definitions

: tcompile
   ?stack  >in @  name find ?dup
    IF  0> IF  nip execute >interpret  THEN
        drop dup >in ! name
    THEN gfind IF  nip execute >interpret  THEN
   nullstring? IF  drop exit  THEN
   number?  ?dup IF 0> IF  swap T [compile] Literal  THEN
                    [compile] Literal H drop >interpret  THEN
   drop >in !  Word,  >interpret ;

Transient definitions
: ]           H state on ['] tcompile is >interpret ;

\ *** Block No. 30, Hexblock 1e

\ Target conditionals                                bp05mar86we

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
: REPEAT       T compile  branch (repeat H ; immediate restrict




\ *** Block No. 31, Hexblock 1f

\ Target conditionals                                bp27jun85we

: DO             T compile (do >mark H 3 ;  immediate restrict
: ?DO            T compile (?do >mark H 3 ; immediate restrict
: LOOP           T 3 ?pairs compile (loop compile endloop
                 >resolve H ;               immediate restrict
: +LOOP          T 3 ?pairs compile (+loop compile endloop
                 >resolve H ;               immediate restrict









\ *** Block No. 32, Hexblock 20

\ predefinitions                                     bp05mar86we

: abort"                T compile (abort" ," H ; immediate
: error"                T compile (err"   ," H ; immediate

Forth definitions

Variable torigin
Variable tudp           0 Tudp !

: >user                 T c@ H  torigin @ + ;






\ *** Block No. 33, Hexblock 21

\ Datatypes                                          bp05mar86we

Transient definitions
: origin!            H torigin ! ;
: user'  ( -- n )    T ' >body c@ H ;
: uallot ( n -- )    H tudp @ swap tudp +! ;

                     DO> >user ;
: User               prebuild User 2 T uallot c, ;

                     DO> ;
: Create             prebuild Create ;

                     DO> T @ H ;
: Constant           prebuild Constant T , ;
: Variable           Create 2 T allot ;

\ *** Block No. 34, Hexblock 22

\ Datatypes                                          bp05mar86we

dummy
: Vocabulary
   H >in @  Vocabulary  >in !  T prebuild Vocabulary 0 , 0 ,
   here  H tvoc-link @ T ,  H tvoc-link ! ;











\ *** Block No. 35, Hexblock 23

\ target defining words                              bp08sep86we

              Do> ;
: Defer       prebuild Defer 2 T allot ;
: Is          T ' H >body state @ IF   T compile (is , H
                                  ELSE T ! H THEN ; immediate
| : dodoes>   T compile (;code H Glast' @
              there resdoes> there tdoes> ! ;

: ;code       0 T ?pairs dodoes> Assembler H [compile] [
              redefinition ; immediate restrict

: does>       T dodoes> $04C C,
              compile (dodoes> H ;      immediate restrict



\ *** Block No. 36, Hexblock 24

\ :  Alias  ;                                        bp25mar86we

: Create:       T Create    H current @ context !   T ] H 0 ;

dummy
: :             H tdoes> off   >in @ >in: !   T prebuild :
                H current @ context !   T ] H 0 ;

: Alias ( n -- )   H Tlast off  (theader  Ghost over resolve
                   tlast @  T c@ H  $20 or  tlast @ T c! , H ;

: ;     T 0 ?pairs compile exit .( unnest gegen exit getauscht)
        [compile] [ H redefinition ;    immediate restrict




\ *** Block No. 37, Hexblock 25

\ predefinitions                                     bp11sep86we

: compile             T compile compile H ; immediate restrict
: Host                H Onlyforth Ttools also ;
: Compiler            T Host H Transient also definitions ;
: [compile]           H Word, ; immediate restrict
: Onlypatch           H there 3 - 0 tdoes> ! 0 ;

Onlyforth
: Target              Onlyforth Transient also definitions ;

Transient definitions
Ghost c, drop




\ *** Block No. 38, Hexblock 26


















\ *** Block No. 39, Hexblock 27

















