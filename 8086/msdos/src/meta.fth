
\ *** Block No. 0, Hexblock 0


















\ *** Block No. 1, Hexblock 1

\ Target compiler loadscr                         ks cas 09jun20
  Onlyforth   \needs Assembler   2 loadfrom asm.fb

  : c+!     ( 8b addr -- )      dup c@ rot + swap c! ;

  ' find $22 + @ Alias found

  : search   ( string 'vocab -- acf n / string ff )
     dup @ [ ' Forth @ ] Literal - Abort" no vocabulary"
     >body (find IF  found exit  THEN  false ;

  3 &27 thru  Onlyforth savesystem meta.com

cr .( Metacompiler saved as META.COM  )



\ *** Block No. 2, Hexblock 2

\ Predefinitions loadscreen                       ks 30 apr 88

  &28 load

cr .( Predefinitions geladen ...) cr












\ *** Block No. 3, Hexblock 3

\ Target header pointers                          ks 29 jun 87

  Variable tfile      tfile off         \ handle of target file
  Variable tdp        tdp off           \ target dp
  Variable displace   displace off      \ diplacement of code
  Variable ?thead     ?thead off        \ for headerless code
  Variable tlast      tlast off         \ last name in target
  Variable glast'     glast' off        \ acf of latest ghost
  Variable tdoes>     tdoes> off        \ code addr of last does
  Variable tdodo      tdodo off         \ location of dodo
  Variable >in:       >in: off          \ last :-def
  Variable tvoc       tvoc off          \
  Variable tvoc-link  tvoc-link off     \ voc-link in target
  Variable tnext-link tnext-link off    \ link for tracer



\ *** Block No. 4, Hexblock 4

\ Target header pointers                          ks 10 okt 87

  : there  ( -- taddr )   tdp @ ;

  : new   pushfile  makefile   isfile@ tfile !
     tvoc-link off   tnext-link off
     $100 tdp !   $100 displace ! ;










\ *** Block No. 5, Hexblock 5

\ Ghost-creating                                  ks 07 dez 87

0 | Constant <forw>          0 | Constant <res>

| Create gname  $21 allot

| : >heap  ( from quan -- )   \ heap over - 1 and + \ align
     dup hallot heap swap cmove ;

  : symbolic  ( string -- cfa.ghost )
     count  dup 1 $1F uwithin not Abort" invalid Gname"
     gname place   BL gname append   align here >r  makeview ,
     state @ IF  context  ELSE  current  THEN @ @  dup @ ,
     gname count under here place   1+ allot align
     here r@ -   <forw> , 0 , 0 ,   r@ here over - >heap
     heap 2+ rot !   r> dp !   heap + ;

\ *** Block No. 6, Hexblock 6

\ ghost words                                     ks 07 dez 87

  : gfind  ( string -- cfa tf / string ff )
     >r   1 r@ c+!   r@ find   -1 r> c+! ;

  : ghost   ( -- cfa )   name gfind ?exit  symbolic ;

  : gdoes>  ( cfa.ghost -- cfa.does )
     4 + dup @ IF  @ exit THEN
     here  <forw> , 0 ,  dup 4 >heap
     dp !   heap swap !  heap ;






\ *** Block No. 7, Hexblock 7

\ ghost utilities                                 ks 29 jun 87

  : g'  ( -- acf )   name gfind 0= Abort" ?T?" ;

  : '.   g' dup @ <forw> case?
     IF ." forw"  ELSE  <res> - Abort" ??" ."  res"  THEN
     2+ dup @ 5 u.r      2+ @ ?dup
     IF dup @ <forw> case?
        IF ."  fdef"  ELSE <res> - Abort"  ??" ."  rdef"  THEN
        2+ @ 5 u.r THEN ;

  ' ' Alias h'





\ *** Block No. 8, Hexblock 8

\ .unresolved                                     ks 29 jun 87

| : forward? ( cfa -- cfa / exit&true )
     dup @ <forw> = 0=exit  dup 2+ @ 0=exit  drop true rdrop ;

| : unresolved? ( addr -- f )   2+
     dup count $1F and + 1- c@  bl =
     IF  name> forward? 4+ @  dup IF  forward?  THEN
     THEN drop  false ;

| : unresolved-words   ( thread -- )
     BEGIN  @ ?dup WHILE dup unresolved?
                         IF dup 2+ .name ?cr THEN  REPEAT ;

  : .unresolved       voc-link @
     BEGIN  dup 4 - unresolved-words  @ ?dup 0= UNTIL ;

\ *** Block No. 9, Hexblock 9

\ Extending Vocabularys for Target-Compilation    ks 29 jun 87

  Vocabulary Ttools
  Vocabulary Defining

  : Vocabulary       Vocabulary 0 , here tvoc @ , tvoc ! ;

  Vocabulary Transient    tvoc off

  Root definitions

  : T Transient ; immediate
  : H Forth     ; immediate
  : D Defining  ; immediate

  Forth definitions

\ *** Block No. 10, Hexblock a

\ Image and byteorder                             ks 02 jul 87

| Code >byte  ( 16b -- 8b- 8b+ )   A A xor
     D- A- xchg   D+ D- xchg   A push   Next   end-code

| Code byte>  ( 8b- 8b+ -- 16b )
     A pop   D- D+ mov   A- D- xchg   Next   end-code

| : >target  ( addr1 -- daddr fcb )  displace @ - 0 tfile @  ;

  Transient definitions

  : c@    ( addr -- 8b )   [ Dos ]
     >target file@ dup 0< Abort" nie abgespeichert" ;

  : c!    ( 8b addr -- )   [ Dos ] >target file! ;

\ *** Block No. 11, Hexblock b

\ Transient primitives                            ks 09 jul 87
  : @     ( addr -- n )    H dup T c@ swap 1+ c@ byte> ;
  : !     ( n addr -- )    H >r  >byte r@ 1+ T c!  r> c! H ;

  : cmove ( from.mem to.target quan -- )  [ Dos ]
     >r >target fseek   ds@ swap r> tfile @ lfputs ;
\    bounds ?DO  dup c@   I T c! H 1+  LOOP  drop ;

  : here  ( -- taddr )     H tdp @ ;
  : here! ( taddr -- )     H tdp ! ;
  : allot ( n -- )         H tdp +! ;
  : c,    ( 8b -- )        T here c! 1 allot H ;
  : ,     ( 16b -- )       T here !  2 allot H ;
  : align ( -- )           H ; immediate
  : even  ( addr1 -- addr2 ) H ; immediate
  : halign                 H ; immediate

\ *** Block No. 12, Hexblock c

\ Transient primitives                            ks 29 jun 87

  : count ( addr1 -- addr2 len )  H dup 1+ swap T c@ H ;

  : ,"    H here ," here   over dp !
     over - T here swap dup allot cmove H ;

  : fill  ( addr quan 8b -- )  H
     -rot bounds ?DO  dup I T c! H  LOOP  drop ;
  : erase ( addr quan -- )    H  0 T fill H ;
  : blank ( addr quan -- )    H bl T fill H ;

  : move-threads    H tvoc @   tvoc-link @
     BEGIN  over ?dup
     WHILE  2- @ over 2- T !   @   H swap @ swap  REPEAT
     Error" some undef. Target-Vocs left" drop ;

\ *** Block No. 13, Hexblock d

\ Resolving                                       ks 29 jun 87
  Forth definitions

  : resolve ( cfa.ghost cfa.target -- )   over dup @ <res> =
     IF  space dup >name .name ." exists "  ?cr
         2+ !  drop exit  THEN   >r >r  2+ @ ?dup
     IF  BEGIN  dup T @ H 2dup = Abort" resolve loop"
                r@ rot T ! H ?dup 0= UNTIL
     THEN  r> r>  <res> over !  2+ ! ;

  : resdoes> ( acf.ghost acf.target -- )  swap gdoes>
     dup @ <res> = IF  2+ ! exit  THEN  swap resolve ;

here 2+ 0 ] Does> dup @ there rot ! T , H ;  ' <forw> >body !
here 2+ 0 ] Does> @ T , H ;                  ' <res>  >body !


\ *** Block No. 14, Hexblock e

\ compiling names into targ.                      ks 10 okt 87

| : tlatest ( -- addr )       current @ 6 + ;

  : (theader   ?thead @ IF  1 ?thead +! exit  THEN
     >in @  bl word  swap >in !  dup count upper
     dup c@ 1 $20 uwithin  not Abort" inval. Tname"
     blk @ $8400 or T align , H
     there tlatest @ T , H tlatest !   there tlast !
     there over c@ 1+  dup T allot cmove align H ;

  : theader    tlast off
     (theader ghost   dup glast' !   there resolve ;




\ *** Block No. 15, Hexblock f

\ prebuild defining words                         ks 29 jun 87

| : (prebuild   >in @  Create  >in !
                r> dup 2+ >r  @ here 2- ! ;

| : tpfa,       there , ;

  : prebuild   ( addr check# -- check# )  0 ?pairs
     dup IF  compile (prebuild  dup ,  THEN
     compile theader   ghost gdoes> ,
     IF  compile tpfa, THEN  0 ; immediate

  : dummy   0 ;

  : DO>   [compile] Does> here 3 - compile @  0 ] ;


\ *** Block No. 16, Hexblock 10

\ Constructing defining words in Host            kks 07 dez 87

| : defcomp  ( string -- )  dup ['] Defining search ?dup
     IF  0> IF  nip execute exit  THEN  drop dup  THEN
     find ?dup IF  0< IF  nip , exit  THEN THEN
     drop  ['] Forth search ?dup
     IF  0< IF  , exit  THEN  execute exit  THEN
     number? ?dup 0= Abort" ?"
     0> IF  swap [compile] Literal  THEN  [compile] Literal ;

| : definter ( string -- )  dup ['] Defining search ?dup
     IF  0< IF  nip execute exit  THEN THEN  drop
     find ?dup IF  1 and 0= Abort" compile only"  execute exit
               THEN  number? 0= Error" ?"  ;



\ *** Block No. 17, Hexblock 11

\ Constructing defining words in Host             ks 22 dez 87

| : (;tcode      r> @   tlast @ T count +  ! H ;

Defining definitions

  : ]  H ] ['] defcomp Is parser ;

  : [  H [compile] [ ['] definter Is parser ; immediate

  : ;  H [compile] ;  [compile] \\ ; immediate

  : Does>    H compile (;tcode  tdoes> @ ,
     [compile] ; -2 allot [compile] \\ ; immediate
D ' Does> Alias ;Code  immediate   H


\ *** Block No. 18, Hexblock 12

\ reinterpreting defining words                   ks 22 dez 87
  Forth definitions

  : ?reinterpret ( f -- )   0=exit
     state @ >r  >in @ >r   adr parser @ >r
     >in: @ >in !   :  D ] H interpret
     r> Is parser   r> >in !   r> state ! ;

  : undefined?  ( -- f )   glast' @ 4+ @ 0= ;

| : flag!  ( 8b -- )  tlast @ ?dup 0= IF  drop exit  THEN
     dup T c@ rot or swap c! H ;

| : nfa? ( acf alf -- anf / acf ff )
     BEGIN  dup WHILE  2dup 2+ T count $1F and + even H =
                       IF  2+ nip exit  THEN  T @ H REPEAT ;

\ *** Block No. 19, Hexblock 13

\ the 8086 Assembler                              ks 29 jun 87

| Create relocate ] T c, , here ! c! H [

Transient definitions

  : Assembler  H [ Assembler ] relocate >codes ! Assembler ;

  : >label ( 16b -- )  H >in @ name gfind rot >in !
     IF over resolve dup THEN drop Constant ;

  : Label      T here >label Assembler H ;

  : Code       H theader T here 2+ , Assembler H ;



\ *** Block No. 20, Hexblock 14

( Transient primitives                           ks 17 dec 83 )

' exit Alias exit           ' load Alias load
' /    Alias /              ' thru Alias thru
' swap Alias swap           ' *    Alias *
' dup  Alias dup            ' drop Alias drop
' /mod Alias /mod           ' rot  Alias rot
' -rot Alias -rot           ' over Alias over
' 2*   Alias 2*             ' +    Alias +
' -    Alias -              ' 1+   Alias 1+
' 2+   Alias 2+             ' 1-   Alias 1-
' 2-   Alias 2-           ' negate Alias negate
' 2swap Alias 2swap         ' 2dup Alias 2dup




\ *** Block No. 21, Hexblock 15

\ Transient primitives                           kks 29 jun 87

       ' also Alias also         ' words Alias words
' definitions Alias definitions    ' hex Alias hex
' decimal     Alias decimal         ' (  Alias (  immediate
       ' \    Alias \  immediate    ' \\ Alias \\ immediate
       ' .(   Alias .( immediate    ' [  Alias [  immediate
       ' cr   Alias cr
' end-code    Alias end-code  ' Transient Alias Transient
   ' +thru    Alias +thru        ' +load Alias +load
         ' .s Alias .s

Tools ' trace  Alias trace  immediate




\ *** Block No. 22, Hexblock 16

\ immediate words and branch primitives           ks 29 jun 87

  : >mark     ( -- addr )  T here 0 , H ;
  : >resolve  ( addr -- )  T here over - swap ! H ;
  : <mark     ( -- addr )  H there ;
  : <resolve  ( addr -- )  T here - , H ;

  : immediate     H $40 flag! ;
  : restrict      H $80 flag! ;


  : |             H ?thead @ ?exit ?thead on ;
  : internal      H 1 ?thead ! ;
  : external      H ?thead off ;



\ *** Block No. 23, Hexblock 17

\ ' | compile  Alias  >name                       ks 29 jun 87

  : '        ( -- acf )     H g' dup @ <res> -
                            IF  Error" undefined"  THEN  2+ @ ;

  : compile       H ghost , ; immediate restrict

  : >name ( acf -- anf / ff )  H tvoc
     BEGIN  @ dup WHILE  under 2- @ nfa? ?dup IF nip exit THEN
            swap  REPEAT  nip ;







\ *** Block No. 24, Hexblock 18

\ >name  Alias                                    ks 29 jun 87

  : >body ( acf -- apf )   H 2+ ;

  : Alias ( n -- )   H tlast off
     (theader  ghost over resolve  T , H  $20 flag! ;

  : on    ( addr -- )   H  true swap T ! H ;
  : off   ( addr -- )   H false swap T ! H ;








\ *** Block No. 25, Hexblock 19

\ Target tools                                    ks  9 sep 86
  Onlyforth

| : .tfield ( taddr len quan -)  >r under Pad swap
     bounds ?DO  dup T c@ I H c!  1+  LOOP  drop
     Pad over type r> swap - 0 max spaces ;

  ' view Alias hview

  Ttools also definitions

| : ?:  ( addr -- addr )   dup 4 u.r ." :" ;
| : @?  ( addr -- addr )   dup T @ H 6 u.r ;
| : c?  ( addr -- addr )   dup T c@ H 3 .r ;



\ *** Block No. 26, Hexblock 1a

\ Ttools for decompiling                          ks  9 sep 86

  : s  ( addr -- addr+ )   ?: space c? 4 spaces
     T count 2dup + even -rot 18 .tfield ;

  : n  ( addr -- addr+2 )  ?: @? 2 spaces   dup T @ >name H
     ?dup IF  T count H  ELSE  0 0  THEN
     $1F and  $18 .tfield 2+ ;

  : d  ( addr n -- addr+n )  2dup swap ?: 3 spaces
     swap 0 DO  c? 1+  LOOP  4 spaces -rot dup .tfield ;

  : l  ( addr -- addr+2 )  ?: 6 spaces @? 2+ 14 spaces ;

  : c  ( addr -- addr+1 )  1 d 15 spaces ;


\ *** Block No. 27, Hexblock 1b

\ Tools for decompiling                           ks 29 jun 87

  : b  ( addr -- addr+2 )  ?: @? dup T @ H
     over + 6 u.r 2+ 14 spaces ;

  : dump ( addr n -- )
     bounds ?DO cr I 10 d drop stop? IF LEAVE THEN  10 +LOOP ;

  : view  T ' >name H ?dup 0=exit  4 - T @ H ?dup 0=exit edit ;








\ *** Block No. 28, Hexblock 1c

\ Predefinitions loadscreen                       ks 29 jun 87
  Onlyforth

  : clear     H true Abort" There are ghosts" ;


  1 $B +thru










\ *** Block No. 29, Hexblock 1d

\ Literal ['] ?" ." "                             ks 29 jun 87
  Transient definitions Forth

  : Literal  ( n -- )   H dup $FF00 and
     IF  T compile lit , H exit THEN  T compile clit c, H ;
  immediate

  : Ascii   H bl word 1+ c@  state @ 0=exit
            T [compile] Literal H ; immediate

  : [']     T compile lit           H ; immediate
  : ."      T compile (."  ," align H ; immediate
  : "       T compile ("   ," align H ; immediate




\ *** Block No. 30, Hexblock 1e

\ Target compilation  ]                           ks 07 dez 87
  Forth definitions

| : tcompile  ( string -- )   dup find ?dup
     IF  0> IF  nip execute exit  THEN THEN
     drop gfind IF  execute exit  THEN   number? ?dup
     IF  0> IF  swap T [compile] Literal  THEN
                [compile] Literal H exit  THEN
     symbolic execute ;

  Transient definitions

  : ]    H ] ['] tcompile Is parser ;




\ *** Block No. 31, Hexblock 1f

\ Target conditionals                             ks 10 sep 86

  : IF        T compile ?branch >mark H 1 ; immediate restrict
  : THEN      abs 1 ?pairs T >resolve H ;   immediate restrict
  : ELSE      1 ?pairs T compile branch >mark
              swap >resolve H -1 ;          immediate restrict

  : BEGIN     T <mark H 2 ;                 immediate restrict
  : WHILE     2 ?pairs 2 T compile ?branch >mark H -2 2swap ;
                                            immediate restrict

| : (repeat   2 ?pairs T <resolve H
              BEGIN  dup -2 = WHILE  drop T >resolve H REPEAT ;

  : UNTIL     T compile ?branch (repeat H ; immediate restrict
  : REPEAT    T compile  branch (repeat H ; immediate restrict

\ *** Block No. 32, Hexblock 20

\ Target conditionals   Abort" etc.               ks 09 feb 88

  : DO        T compile (do  >mark H 3 ;    immediate restrict
  : ?DO       T compile (?do >mark H 3 ;    immediate restrict
  : LOOP      3 ?pairs T compile (loop
              compile endloop >resolve H ;  immediate restrict
  : +LOOP     3 ?pairs T compile (+loop
              compile endloop >resolve H ;  immediate restrict

  : Abort"    T compile (abort" ," align H ; immediate restrict
  : Error"    T compile (error" ," align H ; immediate restrict






\ *** Block No. 33, Hexblock 21

\ Target does>  ;code                             ks 29 jun 87

| : dodoes>   T compile (;code
     H glast' @ there resdoes>   there tdoes> ! ;

  : Does>    H undefined? T dodoes>
     $E9 c,  H tdodo @ there - 2- T ,
     H ?reinterpret ; immediate restrict

  : ;Code  H 0 ?pairs undefined? T dodoes> H ?reinterpret
     T [compile] [ Assembler H ; immediate restrict






\ *** Block No. 34, Hexblock 22

\ User                                            ks 09 jul 87
  Forth definitions

  Variable torigin    torigin off       \ cold boot vector
  Variable tudp       tudp off          \ user variable counter
  : >user    ( addr1 -- addr2 )  T c@ H torigin @ + ;

  Transient definitions Forth

  : origin!  ( taddr -- )     H torigin !  tudp off ;
  : uallot   ( n -- offset )  H tudp @  swap tudp +! ;

  DO> >user ;
  : User      T prebuild User 2 uallot c, H ;



\ *** Block No. 35, Hexblock 23

\ Variable  Constant  Create                      ks 01 okt 87

  DO> ;
  : Variable T prebuild Create  2 allot H ;

  DO> T @ H ;
  : Constant T prebuild Constant  , H ;

  DO> ;
  : Create    T prebuild Create  H ;

  : Create:   T Create ] H end-code 0 ;





\ *** Block No. 36, Hexblock 24

\ Defer  Is  Vocabulary                           ks 29 jun 87

  DO> ;
  : Defer     T prebuild Defer  2 allot ;
  : Is        T ' >body H state @
     IF  T compile (is , H exit  THEN  T ! H ; immediate

  dummy
  : Vocabulary   H >in @ Vocabulary >in !
     T prebuild Vocabulary  0 , 0 ,
     H there   tvoc-link @ T , H   tvoc-link ! ;






\ *** Block No. 37, Hexblock 25

\ File                                            ks 19 mär 88
  Forth definitions

  Variable tfile-link  tfile-link off
  Variable tfileno     tfileno off
  &45 Constant tb/fcb

  Transient definitions Forth

  dummy
  : File   T prebuild File  here tb/fcb 0 fill
     here H tfile-link @ T , H tfile-link !
     1 tfileno +!  tfileno @ T c, 0 , 0 , 0 , 0 , 0 ,
     here dup >r  1+   tb/fcb &13 - allot   H tlast @
     T count dup r> c!
     H bounds ?DO  I T c@ over c! H 1+  LOOP drop ;

\ *** Block No. 38, Hexblock 26

\ : ; compile Host [compile]                      ks 29 jun 87

  dummy
  : :        H >in @ >in: !   T prebuild : ] H end-code 0 ;

  : ;        0 ?pairs T compile unnest
             [compile] [ H ; immediate restrict

  : compile  T compile compile H ; immediate restrict

  : Host     H Onlyforth ;

  : Compiler H Onlyforth Transient also definitions ;

  : [compile]  H ghost execute ; immediate restrict


\ *** Block No. 39, Hexblock 27

\ Target                                          ks 29 jun 87

  Onlyforth

  : Target   H vp off  Transient also definitions ;

  Transient definitions

  ghost c, drop








\ *** Block No. 40, Hexblock 28


















\ *** Block No. 41, Hexblock 29


















\ *** Block No. 42, Hexblock 2a


















\ *** Block No. 43, Hexblock 2b


















\ *** Block No. 44, Hexblock 2c


















\ *** Block No. 45, Hexblock 2d


















\ *** Block No. 46, Hexblock 2e


















\ *** Block No. 47, Hexblock 2f


















\ *** Block No. 48, Hexblock 30


















\ *** Block No. 49, Hexblock 31


















\ *** Block No. 50, Hexblock 32


















\ *** Block No. 51, Hexblock 33


















\ *** Block No. 52, Hexblock 34

















