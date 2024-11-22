
\ *** Block No. 0, Hexblock 0

\ Extended-Decompiler for VolksForth LOAD-SCREEN      UH 07Nov86

Dieses File enthaelt einen Decompiler, der bereits kompilierte
Worte wieder in Sourcetextform bringt.
Strukturierte Worte wie IF THEN ELSE, BEGIN WHILE REPEAT UNTIL
und DO LOOP +LOOP werden in einem an AI-grenzenden Vorgang
erkannt und umgeformt.
Ein Decompiler kann aber keine (Stack-) Kommentare wieder
herzaubern, die Benutzung der Screens und dann view, wird
daher staerkstens empfohlen.

Denn:        Es ist immernoch ein Fehler drin!
Und um den zu korrigieren, ist der Sourcetext dem Objektkode
doch vorzuziehen.

              Benutzung:       see <name>

\ *** Block No. 1, Hexblock 1

\ Extended-Decompiler for VolksForth LOAD-SCREEN         07Nov86

Onlyforth Tools also definitions

1 13 +thru

\\
Produces compilable Forth source from normal compiled Forth.

         These source blocks are based on the works of

            Henry Laxen, Mike Perry and Wil Baden

               volksFORTH version: U. Hoffmann



\ *** Block No. 2, Hexblock 2

\ detacting does>                                        01Jul86

internal

' does> 4+ @   Alias (;code
' Forth @ 1+ @  Constant (dodoes>

: does?    ( IP -  f )
   dup c@  $CD ( call ) =  swap
   1+ @  (dodoes>  = and ;







\ *** Block No. 3, Hexblock 3

\ indentation.                                           04Jul86
Variable #spaces   #spaces off

: +in  ( -- ) 3 #spaces +! ;

: -in  ( -- ) -3 #spaces +! ;

: ind-cr ( -- ) ( col #spaces @ = ?exit ) cr  #spaces @ spaces ;

: ?ind-cr ( -- ) col c/l u> IF ind-cr THEN  ;







\ *** Block No. 4, Hexblock 4

\ case defining words                                    01Jul86

: Case:  ( -- )
   Create:  Does>   swap 2* +  perform ;

: Associative: ( n -- )
   Constant   Does>      ( n - index )
   dup @ -rot  dup @ 0
   DO  2+  2dup @ =
       IF  2drop drop  I  0 0  LEAVE  THEN   LOOP 2drop ;







\ *** Block No. 5, Hexblock 5

\ branching                                              04Jul86

Variable #branches    Variable #branch

: branch-type ( n -- a ) 6 * pad + ;
: branch-from ( n -- a ) branch-type 2+ ;
: branch-to   ( n -- a ) branch-type 4+ ;

: branched ( adr type -- ) \ Make entry in branch-table.
     #branches @ branch-type !   dup #branches @ branch-from !
     2+ dup @ +  #branches @ branch-to !   1 #branches +! ;

\\ branch-table: { type0|from0|to0 | type1|from1|to1 ... }




\ *** Block No. 6, Hexblock 6

\ branching                                              01Jul86

: branch-back ( adr type --  )
   \  : make entry in branch-table & reclassify branch-type.)
   over swap branched
   2+ dup dup @ + swap 2+ ( loop-start,-end.)
   0  #branches @ 1-
   ?DO
      over I branch-from @  u> IF LEAVE THEN
      dup  I branch-to @ = IF ['] while  I branch-type !  THEN
   -1 +LOOP 2drop ;






\ *** Block No. 7, Hexblock 7

\ branching                                              01Jul86
: forward? ( ip -- f )   2+ @ 0> ;

: ?branch+ ( ip -- ip' ) dup 4+ swap dup forward?
   IF ['] if branched exit THEN   ['] until branch-back ;

: branch+ ( ip -- ip' ) dup 4+ swap dup forward?
   IF ['] else branched exit THEN ['] repeat branch-back ;

: (loop)+ ( ip -- ip' )
   dup  dup @ ( loop,+loop )  branch-back -1 #branches +! 4+ ;

: string+ ( ip -- ip' ) 2+ count + even ;

: (;code+ ( ip -- ip' ) 2+ dup does? not IF 0= exit THEN 3+  ;


\ *** Block No. 8, Hexblock 8

\ classify each word                                     25Aug86
Forth

&15 Associative: execution-class
 ] clit         lit             ?branch         branch
   (do          (."             (abort"         (;code
   ("           (?do            (loop
   (+loop       unnest          (is             compile   [

Case:  execution-class+
   3+           4+              ?branch+        branch+
   2+           string+         string+         (;code+
   string+      2+              4+
   4+           0=              4+              4+         2+  ;

Tools

\ *** Block No. 9, Hexblock 9

\ first pass                                             01Jul86

: pass1 ( cfa -- ) #branches off >body
   BEGIN dup @ execution-class execution-class+
      dup 0= stop? or
   UNTIL drop ;











\ *** Block No. 10, Hexblock a

\ identify branch destinations.                          04Jul86
: thru.branchtable ( -- limit start ) #branches @ 0 ;
: ?.then ( ip -- ) thru.branchtable
   ?DO I branch-to @ over =
      IF I branch-from @ over u<
        IF I branch-type @ dup ['] else = swap ['] if = or
          IF -in ." THEN " ind-cr LEAVE THEN THEN THEN
   LOOP ;
: ?.begin ( ip -- ) thru.branchtable
   ?DO I branch-to @ over =
      IF I branch-from @ over u< not
        IF I branch-type @ dup
           ['] repeat = swap ['] until = or
           IF ind-cr ." BEGIN " +in LEAVE THEN THEN THEN
   LOOP ;
( put "BEGIN" and "THEN" where used.)

\ *** Block No. 11, Hexblock b

\ decompile each type of word                            01Jul86

: .word   ( ip -- ip' ) dup @ >name .name 2+  ;

: .(word  ( ip -- ip' )     dup @  >name
      ?dup 0= IF ." ??? " ELSE
      count $1f and swap 1+ swap 1-  type space THEN 2+ ;
: .inline ( val16b -- )
     dup >name ?dup IF ." ['] " .name drop exit THEN . ;

: .lit    ( ip -- ip' )     2+ dup @ .inline  2+  ?.then ;
: .clit   ( ip -- ip' )     2+ dup c@ . 1+ ?.then ;
: .string ( ip -- ip' )
     .(word count 2dup type Ascii " emit space + even ?.then ;

: .unnest  ( ip -- 0 )      ." ; " 0=  ;

\ *** Block No. 12, Hexblock c

\ decompile each type of word                            01Jul86

: .default ( ip -- ip' ) dup @ >name ?dup IF
    c@ $40 and IF ." [COMPILE] " THEN THEN   .word ?.then ;

: .['] ( ip -- ip' )  .(word dup @ 2- >name .name  2+  ?.then ;

: .compile ( ip -- ip' ) .word  .word  ?.then ;









\ *** Block No. 13, Hexblock d

\ decompiling conditionals                               04Jul86

: .if     ( ip nfa -- ip' )  ind-cr .name +in 4+ ?.then ;
: .repeat ( ip nfa -- ip' )  -in .name ind-cr 4+ ?.then ;
: .else   ( ip nfa -- ip' )  -in ind-cr .name +in 4+ ;
: .do     ( ip nfa -- ip' )  ind-cr .(word +in 2+ ?.then ;
: .loop   ( ip nfa -- ip' )  -in .(word ind-cr 2+ ?.then ;

5 Associative: branch-class
 ' if ,   ' while ,  ' else ,  ' repeat ,  ' until ,
Case: .branch-class
   .if    .else      .else     .repeat     .repeat      ;

: .branch ( ip -- ip' )
    #branch @   branch-type @   1 #branch +!
    dup >name   swap branch-class .branch-class ;

\ *** Block No. 14, Hexblock e

\ decompile Does> ;code                                  04Jul86

: .(;code  ( IP - IP' f)
   2+ dup does?
   IF  ind-cr ." DOES> " 3+  ELSE  ." ;CODE "  0=  THEN ;












\ *** Block No. 15, Hexblock f

\ classify word's output                                 01Jul86

Case: .execution-class
   .clit        .lit            .branch         .branch
   .do          .string         .string         .(;code
   .string      .do             .loop
   .loop        .unnest         .[']            .compile
   .default ;









\ *** Block No. 16, Hexblock 10

\ decompile colon-definitions                            04Jul86

: pass2 ( cfa -- ) #branch off >body
   BEGIN ?.begin ?ind-cr  dup @ execution-class .execution-class
         dup 0= stop? or
   UNTIL drop ;

: .pfa ( cfa -- ) #spaces off  +in  dup pass1   pass2 ;

: .immediate  ( cfa - )    >name c@ dup
    ?ind-cr  40 and IF  ." IMMEDIATE "  THEN
    ?ind-cr  80 and IF  ." RESTRICT"    THEN ;

: .:  ( cfa - )  ." : " dup >name .name 3 spaces  .pfa ;



\ *** Block No. 17, Hexblock 11

\ display category of word                               01Jul86
external   Defer (see   internal

: .does>  ( cfa - )     ." DOES> "  @  1+ .pfa ;

: .user-variable  ( cfa - ) ." USER " dup >name dup .name
    3 spaces swap execute @ u. .name ." ! " ;

: .defer   ( cfa - )
 ." deferred "  dup >name .name  ." Is "  >body @  (see  ;

: .other   ( cfa - )  dup >name .name
    dup @ over >body =   IF drop ."  is Code " exit THEN
    dup @ does? IF .does> exit THEN
    drop ." is unknown " ;


\ *** Block No. 18, Hexblock 12

\ decompiling variables and constants                    01Jul86

: .constant   ( cfa - )
    dup  >body @ u.  ." CONSTANT "  >name .name ;

: .variable   ( cfa - ) ." VARIABLE "
    dup >name dup .name 3 spaces swap >body @ u. .name ." ! " ;










\ *** Block No. 19, Hexblock 13

\ classify a word                                     UH 25Jan88

5 Associative: definition-class
  ' quit @ ,    ' 0 @ ,         ' scr @ ,       ' base @ ,
  ' 'cold @ ,

Case: .definition-class
  .:            .constant       .variable       .user-variable
  .defer        .other  ;








\ *** Block No. 20, Hexblock 14

\ Top level of Decompiler                                04Jul86

external

: ((see    ( cfa -)
    #spaces off cr
    dup dup @
    definition-class .definition-class   .immediate  ;

' ((see Is (see

Forth definitions
: see   ' (see ;




\ *** Block No. 21, Hexblock 15


















\ *** Block No. 22, Hexblock 16


















\ *** Block No. 23, Hexblock 17

















