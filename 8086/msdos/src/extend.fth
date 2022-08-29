
\ *** Block No. 0, Hexblock 0

\                                                 ks 11 mai 88
Dieses File enthält Definitionen, die zum Laden der weiteren
System- und Applikationsfiles benötigt werden.

Unter anderem finden sich hier auch MS-DOS spezifische
Befehle wie zum Beispiel das Allokieren von Speicher-
platz ausserhalb des auf 64k begrenzten Forthsystems
und einige Routinen, die das Arbeiten mit dem Video-
Display erleichtern sowie einige Operatoren zur String-
manipulation.







\ *** Block No. 1, Hexblock 1

\ loadscreen for often used words                 ks cas 25sep16

  Onlyforth  \needs Assembler   2 loadfrom asm.fb

  ' save-buffers Alias sav

  ' name &12 + Constant 'name

  ' page Alias cls

   1 8 +thru  .( Systemerweiterung geladen) cr






\ *** Block No. 2, Hexblock 2

\ Postkernel words                                ks 22 dez 87

  : blank   ( addr quan -- )   bl fill ;

  Code stash ( u1 u2 -- u1 u1 u2 )
     S W mov   W ) push   Next   end-code
\ : stash    ( u1 u2 -- u1 u1 u2 )   over swap ;

  : >expect  ( addr len -- )  stash expect  span @ over place ;

  : .field   ( addr len quan -- )
     over - >r type r> 0 max spaces ;

  : tab  ( n -- )   col - 0 max spaces ;



\ *** Block No. 3, Hexblock 3

\ postkernel                                      ks 08 mär 89
\ hier sollte END-CODE eigentlich aehem, also z.B. -TRANSIENT

\needs end-code  : end-code   toss also ;

  : u?     ( addr -- )   @ u. ;

  : adr   ' >body  state @ 0=exit  [compile] Literal ; immediate

  : Abort(  ( f -- )  IF  [compile] .(  true abort"  !"  THEN
                      [compile] ( ;

  : arguments  ( n -- )
     depth 1- > Error" zu wenige Parameter" ;



\ *** Block No. 4, Hexblock 4

\ MS-DOS memory management

  Code lallocate  ( pages -- seg ff / rest err# )
     R push   D R mov   $48 # A+ mov   $21 int  CS
     ?[  A D xchg   A pop   R push   A R xchg
     ][  R pop   A push   0 # D mov  ]?   Next   end-code

  Code lfree      ( seg -- err# )
     E: push   D E: mov   $49 # A+ mov   $21 int  CS
     ?[  A D xchg  ][  0 # D mov  ]?   E: pop   Next   end-code







\ *** Block No. 5, Hexblock 5

\ postkernel                                      ks 03 aug 87

  c/row c/col * 2* Constant c/dis   \ characters per display

  Code video@    ( -- seg )   D push   R D mov   $F # A+ mov
     $10 int   R D xchg   0 # D- mov   7 # A- cmp
     0= ?[  $B0 # D+ mov  ][  $B8 # D+ add  ]?  Next
  end-code

  : savevideo  ( -- seg / ff )
      [ c/dis b/seg /mod swap 0<> - ] Literal lallocate
      IF  drop false exit  THEN  video@ 0 2 pick 0 c/dis lmove ;

  : restorevideo  ( seg -- )   ?dup 0=exit
     dup 0 video@ 0 c/dis lmove  lfree drop ;


\ *** Block No. 6, Hexblock 6

\ string operators   append attach                ks 21 jun 87

| : .stringoverflow   true Abort" String zu lang" ;

  Code append  ( char addr -- )
     D W mov   D pop   W ) A- mov   1 # A- add  CS
     ?[  ;c: .stringoverflow ; Assembler ]?
     A- W ) mov   0 # A+ mov   A W add
     D- W ) mov   D pop   Next   end-code

  Code attach  ( addr len addr1 -- )  D W mov   C pop
     I D mov   I pop   W ) A- mov   A- A+ mov   C- A+ add  CS
     ?[  ;c: .stringoverflow ; Assembler ]?
     A+ W ) mov   A+ A+ xor   A+ C+ mov   A W add   W inc
     rep byte movs   D I mov   D pop   Next   end-code


\ *** Block No. 7, Hexblock 7

\\ string operators   append attach detract        ks 21 jun 87

  : append   ( char addr -- )
     under   count + c!   dup c@ 1+ swap c! ;

  : attach ( addr len addr.to -- )
     >r under   r@ count + swap move   r@ c@ + r> c! ;

  : detract ( addr -- char )
     dup c@ 1- dup 0> and  over c!
     count >r  dup count -rot  swap r> cmove ;






\ *** Block No. 8, Hexblock 8

\ ?"  string operator                             ks 09 feb 88

\ : (?"  ( 8b -- index )  "lit under count rot
\    scan IF  swap - exit  THEN  2drop false ;

| Create months  ," janfebmäraprmaijunjulaugsepoktnovdez"

  : >months   ( n -- addr len )   3 * 2- months + 3 ;

| Code (?"  ( 8b -- index )
     A D xchg   I ) C- mov   0 # C+ mov   C I add
     I W mov   I inc   std   0<>rep byte scas   cld
     0= ?[  C inc  ]?  C D mov   Next
  end-code

  : ?"   compile (?" ," align ; immediate restrict

\ *** Block No. 9, Hexblock 9

\ Conditional compilation                         ks 12 dez 88
| Defer cond

  : .THEN ; immediate

  : .ELSE  ( -- )   0
     BEGIN  name nullstring? IF  drop exit  THEN
            find IF  cond -1 case? ?exit  ELSE  drop  THEN
     REPEAT ; immediate

  : .IF ( f -- )  ?exit [compile] .ELSE ; immediate

| : (cond  ( n cfa -- n' )
     ['] .THEN case? IF  1- exit  THEN
     ['] .ELSE case? IF  dup 0= + exit  THEN
     ['] .IF   = 0=exit  1+ ; ' (cond is cond

\ *** Block No. 10, Hexblock a

















