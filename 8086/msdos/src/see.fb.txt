
\ *** Block No. 0, Hexblock 0

\ Extended-Decompiler for VolksForth                 cas 10nov05

This file contains the volksFORTH decompiler. The decompiler
will convert FORTH code back to Sourcecode.
Conditional words like IF THEN ELSE, BEGIN WHILE REPEAT UNTIL
and DO LOOP +LOOP are identified and converted.

The Decompiler cannot re-create comments, so please use
comments in screens and view.


Because: There is always one more bug!
And to correct bug, nothing beats good commented sourcecode.


Usage:   SEE <name>

\ *** Block No. 1, Hexblock 1

\ Extended-Decompiler for VolksForth LOAD-SCREEN  ks 22 dez 87
Onlyforth Tools also definitions

| : internal    1 ?head ! ;
| : external    ?head off ;

1 &18 +thru

\\
Produces compilable Forth source from normal compiled Forth.

         These source blocks are based on the works of

            Henry Laxen, Mike Perry and Wil Baden

               volksFORTH version: U. Hoffmann

\ *** Block No. 2, Hexblock 2

\ detecting does>                                 ks 22 dez 87

internal

' Forth @ 1+ dup @ + 2+ Constant (dodoes>

: does?    ( IP -  f )
   dup c@  $E9 ( jmp ) =
   swap 1+ dup @ + 2+   (dodoes>  = and ;








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

\ first pass                                      ks 22 dez 87

: pass1 ( cfa -- ) #branches off >body
   BEGIN dup @ execution-class execution-class+
      dup 0= stop? or
   UNTIL drop ;

: thru.branchtable ( -- limit start ) #branches @ 0 ;









\ *** Block No. 10, Hexblock a

\ identify branch destinations.                   ks 22 dez 87
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

\ decompiling variables and constants             ks 22 dez 87

: .constant   ( cfa - )
    dup  >body @ u.  ." CONSTANT "  >name .name ;

: .variable   ( cfa - ) ." VARIABLE "
    dup >name dup .name 3 spaces swap >body @ u. .name ." ! " ;

5 Associative: definition-class
  ' quit @ ,    ' 0 @ ,         ' scr @ ,       ' base @ ,
  ' 'cold @ ,

Case: .definition-class
  .:            .constant       .variable       .user-variable
  .defer        .other  ;


\ *** Block No. 19, Hexblock 13

\ Top level of Decompiler                             ks 20dez87

external

: ((see    ( cfa -)
    #spaces off cr
    dup dup @
    definition-class .definition-class   .immediate  ;

' ((see Is (see

Forth definitions
  : see   ' (see ;




\ *** Block No. 20, Hexblock 14


















\ *** Block No. 21, Hexblock 15


















\ *** Block No. 22, Hexblock 16


















\ *** Block No. 23, Hexblock 17


















\ *** Block No. 24, Hexblock 18


















\ *** Block No. 25, Hexblock 19


















\ *** Block No. 26, Hexblock 1a


















\ *** Block No. 27, Hexblock 1b


















\ *** Block No. 28, Hexblock 1c


















\ *** Block No. 29, Hexblock 1d


















\ *** Block No. 30, Hexblock 1e


















\ *** Block No. 31, Hexblock 1f


















\ *** Block No. 32, Hexblock 20


















\ *** Block No. 33, Hexblock 21


















\ *** Block No. 34, Hexblock 22


















\ *** Block No. 35, Hexblock 23


















\ *** Block No. 36, Hexblock 24


















\ *** Block No. 37, Hexblock 25


















\ *** Block No. 38, Hexblock 26


















\ *** Block No. 39, Hexblock 27


















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


















\ *** Block No. 87, Hexblock 57


















\ *** Block No. 88, Hexblock 58


















\ *** Block No. 89, Hexblock 59


















\ *** Block No. 90, Hexblock 5a


















\ *** Block No. 91, Hexblock 5b


















\ *** Block No. 92, Hexblock 5c


















\ *** Block No. 93, Hexblock 5d


















\ *** Block No. 94, Hexblock 5e


















\ *** Block No. 95, Hexblock 5f


















\ *** Block No. 96, Hexblock 60


















\ *** Block No. 97, Hexblock 61


















\ *** Block No. 98, Hexblock 62


















\ *** Block No. 99, Hexblock 63


















\ *** Block No. 100, Hexblock 64


















\ *** Block No. 101, Hexblock 65


















\ *** Block No. 102, Hexblock 66


















\ *** Block No. 103, Hexblock 67


















\ *** Block No. 104, Hexblock 68


















\ *** Block No. 105, Hexblock 69


















\ *** Block No. 106, Hexblock 6a


















\ *** Block No. 107, Hexblock 6b


















\ *** Block No. 108, Hexblock 6c


















\ *** Block No. 109, Hexblock 6d


















\ *** Block No. 110, Hexblock 6e


















\ *** Block No. 111, Hexblock 6f


















\ *** Block No. 112, Hexblock 70


















\ *** Block No. 113, Hexblock 71


















\ *** Block No. 114, Hexblock 72


















\ *** Block No. 115, Hexblock 73


















\ *** Block No. 116, Hexblock 74


















\ *** Block No. 117, Hexblock 75


















\ *** Block No. 118, Hexblock 76


















\ *** Block No. 119, Hexblock 77


















\ *** Block No. 120, Hexblock 78


















\ *** Block No. 121, Hexblock 79

















