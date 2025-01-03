( ----- 000 )
\  ####  volksFORTH   ####                           cas 11apr21
   volksFORTH designed and developed by
   the volksFORTH team

    see https://volksforth.sf.net
        https://github.com/forth-ev/VolksForth

    for documentation, updated versions and development
    information
( ----- 001 )
\ MS-DOS   volksForth Load Screen                 ks cas 18jul20
  warning off          \ disable warnings during compilation
  Onlyforth  \needs Transient   include meta.fb
  2 loadfrom META.fb

  new FORTH.COM   Onlyforth Target definitions

  4 &111 thru          \ Standard 8088-System
  warning on
  flush                 \ close FORTH.COM

cr .( new kernel as "FORTH.COM" written) cr bell
( ----- 002 )
\\ The use of the 8088/86 register                ks 27 oct 86

The assembler uses forth style names for the register
Mapping of Forth Registernames to INTEL Register Names:

A <=> AX      A- <=> AL     A+ <=> AH
C <=> CX      C- <=> CL     C+ <=> CH
  Register A and C are free to use

D <=> DX      D- <=> DL     D+ <=> DH
  the Top of (Data-) Stack (TOS)

R <=> BX      R- <=> RL     R+ <=> RH
  the Return_stack_pointer
( ----- 003 )
\\ The use of the 8088/86 register                ks 27 oct 86

U <=> BP     User_area_pointer
S <=> SP     Daten_stack_pointer
I <=> SI     Instruction_pointer
W <=> DI     Word_pointer, free for general use

D: <=> DS    E: <=> ES    S: <=> SS    C: <=> CS
   All segment registers are set to the value of code-segment
   C: and must be restored to the same if changed in the code
( ----- 004 )
\ FORTH Preamble and ID                           ks 11 m�r 89
Assembler

nop   5555 # jmp              here 2- >label >cold
nop   5555 # jmp              here 2- >label >restart

Create origin   here origin!    here $100 0 fill
\ Coldstart values for user variables

  $E9 int  end-code  -4 ,  $FC allot
\ this is the multitasker initialization in the user area

| Create logo ," volksFORTH-83 Version 3.9.3"
( ----- 005 )
\ Next                                            ks 27 oct 86

  Variable next-link    0 next-link !

  Host Forth Assembler also definitions

  : Next    lods   A W xchg   W ) jmp
            there tnext-link @ T , H tnext-link ! ;

\ Next is in-line code. All "nexts" are linked into a
\ list with the anchor NEXT-LINK for the debugger

  : u'       ( -- offset )    T ' 2+ c@ H ;

  Target
( ----- 006 )
\ recover ;c: noop                                ks 27 oct 86

  Create recover   Assembler
     R dec   R dec   I R ) mov   I pop   Next
  end-code

Host Forth Assembler also definitions

  :  ;c:   0 T recover # call ] end-code H ;

Target

| Code di    cli               Next   end-code
| Code ei    sti     here      Next   end-code

  Code noop        here 2- !   end-code
( ----- 007 )
\ User variables                                  ks 16 sep 88
  8 uallot drop  \ Space for the multitasker
       \ Fields: entry  link  spare  SPsave
       \ Length compatible to 68000, 6502 and 8080 volksFORTH
  User s0
  User r0
  User dp
  User offset            0 offset !
  User base              &10 base !
  User output
  User input
  User errorhandler   \ pointer for Abort" -code
  User aborted        \ code address of latest error
  User voc-link
  User file-link      ( TODO: Why is  UDP a user variable? )
  User udp            \ points to next free addr in User_area
( ----- 008 )
\ manipulate system pointers                      ks 03 aug 87

  Code sp@ ( -- addr )   D push   S D mov   Next   end-code

  Code sp! ( addr -- )   D S mov   D pop   Next   end-code


  Code up@ ( -- addr )   D push   U D mov   Next   end-code

  Code up! ( addr -- )   D U mov   D pop    Next   end-code

  Code ds@ ( -- addr )   D push   D: D mov   Next   end-code

  $10 Constant b/seg    \ bytes per segment
( ----- 009 )
\ manipulate returnstack                          ks 27 oct 86

  Code rp@ ( -- addr )   D push   R D mov   Next   end-code

  Code rp! ( addr -- )   D R mov   D pop    Next   end-code


  Code >r  ( 16b -- )  R dec   R dec   D R ) mov   D pop   Next
  end-code restrict

  Code r>  ( -- 16b )  D push   R ) D mov   R inc   R inc   Next
  end-code restrict
( ----- 010 )
\ r@ rdrop  exit unnest ?exit                     ks 27 oct 86
  Code r@ ( -- 16b )   D push   R ) D mov   Next   end-code

  Code rdrop           R inc   R inc   Next   end-code restrict

  Code exit
  Label >exit   R ) I mov   R inc   R inc   Next   end-code

  Code unnest   >exit  here 2- !   end-code

  Code ?exit  ( flag -- )
     D D or   D pop   >exit 0= ?]      [[  Next   end-code

  Code 0=exit ( flag -- )
     D D or   D pop   >exit 0= not ?]  ]]   end-code
\ : ?exit ( flag -- )   IF rdrop THEN ;
( ----- 011 )
\ execute  perform                                ks 27 oct 86

  Code execute ( acf -- )   D W mov   D pop   W ) jmp   end-code

  Code perform ( addr -- )  D W mov  D pop   W ) W mov   W ) jmp
  end-code

\ : perform   ( addr -- )      @ execute ;
( ----- 012 )
\ c@ c! ctoggle                                   ks 27 oct 86

  Code c@   ( addr -- 8b )
     D W mov   W ) D- mov   0 # D+ mov   Next   end-code

  Code c!   ( 16b addr -- )
     D W mov   A pop   A- W ) mov   D pop   Next   end-code

  Code ctoggle   ( 8b addr -- )
     D W mov   A pop   A- W ) xor   D pop   Next   end-code

\ : ctoggle   ( 8b addr -- )   under c@ xor swap c! ;

  Code flip ( 16b1 -- 16b2 )   D- D+ xchg   Next   end-code
( ----- 013 )
\ @ ! 2@ 2!                                       ks 27 oct 86

  Code @  ( addr -- 16b )  D W mov   W ) D mov   Next   end-code

  Code !  ( 16b addr -- )  D W mov   W ) pop   D pop   Next
  end-code

  : 2@   ( addr -- 32b )    dup 2+ @   swap @ ;

  : 2!   ( 32b addr -- )    under !   2+ ! ;
( ----- 014 )
\ +! drop swap                                    ks 27 oct 86

  Code +!     ( 16b addr -- )
     D W mov   A pop   A W ) add   D pop   Next   end-code

\  : +!       ( n addr -- )   under @ + swap ! ;


  Code drop   ( 16b -- )   D pop   Next   end-code

  Code swap   ( 16b1 16b2 -- 16b2 16b1 )
     A pop   D push   A D xchg   Next   end-code
( ----- 015 )
\ dup  ?dup                                       ks 27 oct 86

  Code dup    ( 16b -- 16b 16b )  D push   Next   end-code

\ : dup       ( 16b -- 16b 16b )    sp@ @ ;

  Code ?dup   ( 16b -- 16b 16b / false )
     D D or   0= not ?[  D push  ]?  Next   end-code

\ : ?dup      ( 16b -- 16b 16b / false)   dup 0=exit dup ;
( ----- 016 )
\ over rot nip under                              ks 27 oct 86

  Code over   ( 16b1 16b2 -- 16b1 16b2 16b1 )
     A D xchg   D pop   D push   A push   Next   end-code
\ : over  >r dup r> swap ;

  Code rot    ( 16b1 16b2 16b3 -- 16b2 16b3 16b1 )
     A D xchg  C pop   D pop   C push   A push   Next  end-code
\ : rot   >r swap r> swap ;

  Code nip ( 16b1 16b2 -- 16b2 )  S inc  S inc   Next  end-code
\ : nip   swap drop ;

  Code under ( 16b1 16b2 -- 16b2 16b1 16b2 )
     A pop   D push   A push   Next   end-code
\ : under swap over ;
( ----- 017 )
\ -rot pick                                       ks 27 oct 86

  Code -rot    ( 16b1 16b2 16b3 -- 16b3 16b1 16b2 )
     A D xchg   D pop   C pop   A push   C push   Next  end-code

\ : -rot    ( 16b1 16b2 16b3 -- 16b3 16b1 16b2 )   rot rot ;

  Code pick    ( n -- 16b.n )
     D sal   D W mov   S W add   W ) D mov   Next   end-code

\ : pick    ( n -- 16b.n )     1+ 2* sp@ + @ ;
( ----- 018 )
\ roll -roll                                      ks 27 oct 86

  Code roll  ( n -- )
     A I xchg   D sal   D C mov   D I mov   S I add
     I ) D mov   I W mov   I dec   W inc   std
     rep byte movs   cld   A I xchg   S inc   S inc   Next
  end-code
\ : roll   ( n -- )
\    dup >r  pick sp@ dup 2+  r> 1+ 2* cmove> drop ;

  Code -roll ( n -- )   A I xchg   D sal   D C mov
     S W mov   D pop   S I mov   S dec   S dec
     rep byte movs   D W ) mov   D pop   A I xchg   Next
  end-code
\ : -roll   ( n -- ) >r dup sp@ dup 2+
\    dup 2+ swap r@ 2* cmove r> 1+ 2* + ! ;
( ----- 019 )
\ 2swap  2drop  2dup 2over                        ks 27 oct 86
  Code 2swap ( 32b1 32b2 -- 32b2 32b1 )   C pop   A pop   W pop
     C push   D push   W push   A D xchg   Next   end-code
\ : 2swap ( 32b1 32b2 -- 32b2 32b1 ) rot >r rot r> ;

  Code 2drop ( 32b -- )  S inc   S inc   D pop   Next  end-code
\ : 2drop ( 32b -- ) drop drop ;

  Code 2dup ( 32b -- 32b 32b )
     S W mov   D push   W ) push   Next   end-code
\ : 2dup ( 32b -- 32b 32b ) over over ;

  Code 2over  ( 1 2 x x -- 1 2 x x 1 2 )
     D push   S W mov   6 W D) push   4 W D) D mov   Next
  end-code
\ : 2over     ( 1 2 x x -- 1 2 x x 1 2 )  3 pick  3 pick ;
( ----- 020 )
\ and or xor not                                  ks 27 oct 86

  Code not   ( 16b1 -- 16b2 )  D com   Next   end-code

  Code and   ( 16b1 16b2 -- 16b3 )
     A pop   A D and   Next   end-code

  Code or    ( 16b1 16b2 -- 16b3 )
     A pop   A D or   Next   end-code
\ : or       ( 16b1 16b2 -- 16b3 )   not swap not and not ;

  Code xor   ( 16b1 16b2 -- 16b3 )
     A pop   A D xor   Next   end-code
( ----- 021 )
\ + -  negate                                     ks 27 oct 86

  Code +   ( n1 n2 -- n3 )   A pop   A D add   Next   end-code

  Code negate  ( n1 -- n2 )    D neg   Next  end-code
\ : negate     ( n1 -- n2 )    not 1+ ;

  Code -    ( n1 n2 -- n3 )
     A pop   D A sub   A D xchg   Next    end-code
\ : -    ( n1 n2 -- n3 )   negate + ;
( ----- 022 )
\ dnegate d+                                      ks 27 oct 86

  Code dnegate ( d1 -- -d1 )     D com   A pop   A neg
     CS not ?[  D inc  ]?   A push   Next   end-code

  Code d+      ( d1 d2 -- d3 )   A pop   C pop   W pop
     W A add   A push   C D adc   Next   end-code
( ----- 023 )
\ 1+ 2+ 3+ 4+ 6+    1- 2- 4-                      ks 27 oct 86

  Code 1+ ( n1 -- n2 )    [[   D inc   Next
  Code 2+ ( n1 -- n2 )    [[   D inc   swap ]]
  Code 3+ ( n1 -- n2 )    [[   D inc   swap ]]
  Code 4+ ( n1 -- n2 )    [[   D inc   swap ]]
| Code 6+ ( n1 -- n2 )    D inc   D inc   ]]   end-code

  Code 1- ( n1 -- n2 )    [[   D dec   Next
  Code 2- ( n1 -- n2 )    [[   D dec   swap ]]
  Code 4- ( n1 -- n2 )    D dec   D dec   ]]   end-code
( ----- 024 )
\ number Constants                                ks 30 jan 88
-1 Constant true      0 Constant false

      0 ( --  0 )   Constant   0
      1 ( --  1 )   Constant   1
      2 ( --  2 )   Constant   2
      3 ( --  3 )   Constant   3
      4 ( --  4 )   Constant   4
     -1 ( -- -1 )   Constant  -1

  Code on  ( addr -- )   -1 # A mov
[[   D W mov   A W ) mov   D pop   Next
  Code off ( addr -- )    0 # A mov   ]]   end-code

\ : on   ( addr -- )   true  swap ! ;
\ : off  ( addr -- )   false swap ! ;
( ----- 025 )
\ words for number literals                       ks 27 oct 86

  Code lit    ( -- 16b )   D push   I ) D mov   I inc
[[   I inc   Next   end-code restrict

  Code clit   ( -- 8b )
     D push   I ) D- mov   0 # D+ mov   ]]   end-code restrict

  : Literal  ( 16b -- )
     dup $FF00 and   IF  compile lit , exit  THEN
     compile clit c, ; immediate restrict
( ----- 026 )
\ comparision code words                          ks 27 oct 86

  Code 0=    ( 16b -- flag )
     D D or   0 # D mov   0= ?[  D dec  ]?  Next   end-code

  Code 0<>  ( n -- flag )
     D D or   0 # D mov   0= not ?[  D dec  ]?  Next   end-code
\ : 0<> ( n -- flag )        0= not ;

  Code u<    ( u1 u2 -- flag )   A pop
[[   D A sub   0 # D mov  CS ?[  D dec  ]?  Next   end-code

  Code u>    ( u1 u2 -- flag )   A D xchg   D pop  ]]  end-code
\ : u>  ( u1 u2 -- flag )    swap u< ;
( ----- 027 )
\  comparision words                              ks 13 sep 88
  Code <     ( n1 n2 -- flag )   A pop
[[ [[   D A sub   0 # D mov   < ?[  D dec  ]?  Next   end-code

  Code >    ( n1 n2 -- flag )   A D xchg   D pop  ]]  end-code

  Code 0>   ( n -- flag )       A A xor           ]]  end-code

\ : <   ( n1 n2 -- flag )
\    2dup xor 0< IF  drop 0< exit  THEN  - 0< ;
\ : >   ( n1 n2 -- flag )    swap < ;
\ : 0>  ( n -- flag )        negate 0< ;

  Code 0<   ( n1 n2 -- flag )
     D D or   0 # D mov   0< ?[  D dec  ]?   Next   end-code
\ : 0<  ( n1 -- flag )       8000 and 0<> ;
( ----- 028 )
\ comparision words                               ks 27 oct 86

  Code =    ( n1 n2 -- flag )   A pop   A D cmp
     0 # D mov  0= ?[  D dec  ]?   Next   end-code
\ : =   ( n1 n2 -- flag )    - 0= ;

  Code uwithin  ( u1 [low high[  -- flag )   A pop   C pop
     A C cmp  CS ?[ [[ swap   0 # D mov   Next  ]?
           D C cmp  CS ?]  -1 # D mov   Next   end-code
\ : uwithin  ( u1 [low up[  -- f )   over - -rot   - u> ;

  Code case?  ( 16b1 16b2 -- 16b1 ff / tf )  A pop   A D sub
     0= ?[  D dec  ][  A push   D D xor  ]?  Next   end-code
\ : case? ( 16b1 16b2 -- 16b1 false / true )
\    over = dup 0=exit  nip ;
( ----- 029 )
\ double number comparisons                       ks 27 oct 86

  Code d0=  ( d - f)      A pop   A D or
     0= not ?[  1 # D mov  ]?  D dec   Next   end-code
\ : d0= ( d -- flag )        or 0= ;

  : d=  ( d1 d2 -- flag )    dnegate d+ d0= ;

Code d<    ( d1 d2 -- flag )    C pop   A pop
   D A sub   A pop   -1 # D mov  < ?[  [[ swap   Next  ]?
   0= ?[  C A sub  CS ?[  D dec  ]? ]?  D inc   ]]   end-code
\ : d<  ( d1 d2 -- flag )
\    rot 2dup -  IF  > nip nip exit  THEN  2drop u< ;
( ----- 030 )
\ min max umax umin abs dabs extend               ks 27 oct 86
  Code min  ( n1 n2 -- n3 )  A pop   A D sub  < ?[  D A add  ]?
                       [[ [[ [[   A D xchg   Next   end-code
  Code max  ( n1 n2 -- n3 )
     A pop   A D sub  dup < not ?]  D A add    ]]   end-code
  Code umin ( u1 u2 -- u3 )
     A pop   A D sub  dup CS ?]  D A add       ]]   end-code
  Code umax ( u1 u2 -- u3 )
     A pop   A D sub  dup CS not ?]  D A add   ]]   end-code

  Code extend ( n -- d )
     A D xchg   cwd   A push   Next   end-code

  Code abs ( n -- u )   D D or  0< ?[  D neg  ]?  Next  end-code

  : dabs  ( d -- ud )      extend 0=exit  dnegate ;
( ----- 031 )
\\ min max umax umin extend                               10Mar8

| : minimax  ( n1 n2 flag -- n3 )   rdrop IF swap THEN drop ;

: min  ( n1 n2 -- n3 )              2dup  > minimax ;
: max  ( n1 n2 -- n3 )              2dup  < minimax ;
: umax  ( u1 u2 -- u3 )             2dup u< minimax ;
: umin  ( u1 u2 -- u3 )             2dup u> minimax ;
: extend   ( n -- d )               dup 0< ;
: dabs  ( d -- ud )                 extend IF dnegate THEN ;
: abs   ( n -- u)                   extend IF  negate THEN ;
( ----- 032 )
\ (do (?do endloop  bounds                        ks 30 jan 88

  Code (do  ( limit start -- )   A pop
[[   $80 # A+ xor   R dec   R dec   I inc   I inc
     I R ) mov   R dec   R dec   A R ) mov   R dec   R dec
     A D sub   D R ) mov   D pop   Next  end-code  restrict

  Code (?do ( limit start -- )   A pop   A D cmp  0= ?]
     I ) I add   D pop   Next   end-code  restrict

  Code endloop    6 # R add   Next   end-code restrict

  Code bounds  ( start count -- limit start )
     A pop   A D xchg   D A add   A push   Next   end-code
\ : bounds ( start count -- limit start )     over + swap ;
( ----- 033 )
\ (loop  (+loop                                   ks 27 oct 86

  Code (loop   R ) word inc
[[   OS not ?[  4 R D) I mov  ]?  Next   end-code restrict

  Code (+loop   D R ) add   D pop  ]]  end-code restrict

\\

| : dodo              rdrop r> 2+ dup >r rot >r swap >r >r ;
\ dodo puts "index | limit | adr.of.DO" on return-stack

  : (do  ( limit start -- )  over - dodo ;  restrict
  : (?do ( limit start -- )  over - ?dup IF dodo THEN
                           r> dup  @ +  >r drop ; restrict
( ----- 034 )
\ loop indices                                    ks 27 oct 86

  Code I  ( -- n )  D push   R ) D mov   2 R D) D add   Next
  end-code
\ : I     ( -- n )  r>  r> dup r@ + -rot  >r >r ;

  Code J  ( -- n )  D push   6 R D) D mov   8 R D) D add   Next
  end-code
( ----- 035 )
\ branch ?branch                                  ks 27 oct 86

  Code branch
[[   I ) I add   Next   end-code restrict
\ : branch r> dup @ + >r ;

  Code ?branch  D D or  D pop   0= not ?]
     I inc   I inc   Next   end-code restrict
( ----- 036 )
\ resolve loops and branches                      ks 02 okt 87

  : >mark     ( -- addr )          here 0 , ;

  : >resolve  ( addr -- )          here over - swap ! ;

  : <mark     ( -- addr )          here ;

  : <resolve  ( addr -- )          here - , ;

  : ?pairs    ( n1 n2 -- )         - Abort" unstructured" ;
( ----- 037 )
\ Branching                                       ks 17 jul 87

  : IF     compile ?branch >mark  1 ; immediate restrict
  : THEN   abs 1 ?pairs  >resolve ;   immediate restrict
  : ELSE   1 ?pairs  compile branch >mark
           swap >resolve  -1 ;        immediate restrict

  : BEGIN   <mark 2 ;                 immediate restrict
  : WHILE   2 ?pairs  2 compile ?branch
            >mark -2 2swap  ;         immediate restrict

| : (repeat   2 ?pairs  <resolve
     BEGIN  dup -2 = WHILE  drop >resolve  REPEAT ;

  : REPEAT compile branch   (repeat ; immediate restrict
  : UNTIL  compile ?branch  (repeat ; immediate restrict
( ----- 038 )
\ Loops                                           ks 27 oct 86

  : DO       compile (do  >mark  3 ; immediate restrict
  : ?DO      compile (?do >mark  3 ; immediate restrict
  : LOOP     3 ?pairs  compile (loop
             compile endloop  >resolve ;  immediate restrict
  : +LOOP    3 ?pairs  compile (+loop
             compile endloop  >resolve ;  immediate restrict

  Code LEAVE    6 # R add   -2 R D) I mov
     I dec   I dec   I ) I add   Next   end-code restrict

\ : LEAVE     endloop r> 2- dup @ + >r ;         restrict
\ Returnstack: | calladr | index | limit | adr of DO |
( ----- 039 )
\ um*  m*  *                                      ks 29 jul 87

  Code um* ( u1 u2 -- ud3 )
     A D xchg   C pop   C mul   A push   Next   end-code

  Code m*  ( n1 n2 -- d3 )
     A D xchg   C pop   C imul   A push   Next   end-code
\ : m*  ( n1 n2 -- d )  dup 0< dup >r IF  negate  THEN  swap
\    dup 0< IF negate r> not >r THEN  um* r> 0=exit  dnegate ;

  : *      ( n1 n2 - prod )   um* drop ;

  Code 2*  ( u -- 2*u )   D shl   Next   end-code
\ : 2*     ( u -- 2*u )   dup + ;
( ----- 040 )
\ um/mod  m/mod                                   ks 27 oct 86

  Code um/mod  ( ud1 u2 -- urem uquot )
     D C mov   D pop   A pop   C div   A D xchg   A push   Next
  end-code

  Code m/mod  ( d1 n2 -- rem quot )   D C mov   D pop
Label divide    D+ A+ mov   C+ A+ xor   A pop  0< not
     ?[  C idiv  [[ swap   A D xchg   A push   Next  ]?
     C idiv   D D or   dup 0= not ?]  A dec   C D add  ]]
  end-code

\ : m/mod ( d n -- mod quot )   dup >r
\    abs over 0< IF  under + swap  THEN   um/mod   r@ 0<
\    IF  negate over IF  swap r@ + swap 1-  THEN THEN  rdrop ;
( ----- 041 )
\ /mod division trap  2/                          ks 13 sep 88

  Code /mod  ( n1 n2 -- rem quot )
     D C mov   A pop   cwd   A push   divide ]]  end-code
\ : /mod   ( n1 n2 -- rem quot )      over 0< swap m/mod ;

  0 >label >divINT

  Label divovl Assembler
     4 # S add   popf   1 # D- mov  ;c: Abort" / overflow" ;

  Code 2/  ( n1 -- n/2 )   D sar   Next   end-code
\ : 2/  ( n -- n/2 )   2 / ;
( ----- 042 )
\ / mod */mod */ u/mod  ud/mod                    ks 27 oct 86

  : /      ( n1 n2 --     quot )      /mod nip ;

  : mod    ( n1 n2 -- rem )           /mod drop ;

  : */mod  ( n1 n2 n3 -- rem quot )   >r m* r> m/mod ;

  : */     ( n1 n2 n3 -- quot )       */mod nip ;

  : u/mod  ( u1 u2 -- urem uquot )    0 swap um/mod ;

  : ud/mod ( ud1 u2 -- urem udquot )
     >r   0 r@ um/mod   r> swap >r   um/mod r> ;
( ----- 043 )
\ cmove cmove> move                               ks 27 oct 86

  Code cmove  ( from to quan -- )   A I xchg   D C mov
     W pop   I pop   D pop   rep byte movs   A I xchg   Next
  end-code

  Code cmove>  ( from to quan -- )
     A I xchg  D C mov  W pop  I pop   D pop
Label moveup   C dec   C W add   C I add   C inc
     std   rep byte movs   A I xchg   cld   Next   end-code

  Code move  ( from to quan -- )
     A I xchg   D C mov   W pop   I pop   D pop
Label domove   I W cmp   moveup CS ?]
     rep byte movs   A I xchg   Next   end-code
( ----- 044 )
\ place count                                     ks 27 oct 86

| Code (place ( addr len to - len to)   A I xchg   D W mov
     C pop   I pop   C push   W inc   domove ]]  end-code

  : place  ( addr len to -)   (place c! ;

  Code count ( addr -- addr+1 len )   D W mov
     W ) D- mov   0 # D+ mov   W inc   W push   Next   end-code

\ : move   ( from to quan -- )
\    >r  2dup u< IF  r> cmove> exit  THEN  r> cmove ;
\ : place  ( addr len to -- ) over >r  rot over 1+  r> move c! ;
\ : count ( adr -- adr+1 len ) dup 1+ swap c@ ;
( ----- 045 )
\       fill erase                                ks 27 oct 86

  Code fill ( addr quan 8b -- )
     D A xchg   C pop   W pop   D pop   rep byte stos   Next
  end-code

\ : fill ( addr quan 8b -- )   swap ?dup
\    IF >r over c! dup 1+ r> 1- cmove exit THEN 2drop ;

  : erase   ( addr quan --)            0 fill ;
( ----- 046 )
\ here allot , c, pad compile                     ks 27 oct 86

  Code here ( -- addr )   D push   u' dp U D) D mov   Next
  end-code
\ : here    ( -- addr ) dp @ ;

  Code allot   ( n -- )   D  u' dp U D) add   D pop   Next
  end-code
\ : allot  ( n -- )    dp +! ;

  : ,      ( 16b -- )  here  ! 2 allot ;
  : c,     ( 8b -- )   here c! 1 allot ;
  : pad    ( -- addr ) here $42 + ;
  : compile            r> dup 2+ >r @ , ; restrict
( ----- 047 )
\ input strings                                   ks 23 dez 87

  Variable #tib     #tib off
  Variable >tib     here >tib ! $50 allot
  Variable >in      >in off
  Variable blk      blk off
  Variable span     span off

  : tib ( -- addr )  >tib @ ;

  : query  tib $50 expect span @ #tib !  >in off ;
( ----- 048 )
\ skip scan /string                               ks 22 dez 87

  Code skip  ( addr len char -- addr1 len1 )
     A D xchg   C pop   C0= not
     ?[  W pop   0=rep byte scas   0= not ?[  W dec   C inc  ]?
         W push  ]?  C D mov   Next   end-code

  Code scan  ( addr0 len0 char -- addr1 len1 )
     A D xchg   C pop  C0= not
     ?[  W pop  0<>rep byte scas   0= ?[  W dec   C inc  ]?
         W push  ]?   C D mov   Next   end-code

  Code /string  ( addr0 len0 +n -- addr1 len1 )
     A pop   C pop   D A sub  CS ?[  A D add   A A xor  ]?
     C D add   D push   A D xchg   Next   end-code
( ----- 049 )
\\ scan skip /string                              ks 29 jul 87

  : skip ( addr0 len0 char -- addr1 len1 )   >r
     BEGIN  dup
     WHILE  over c@ r@ = WHILE  1- swap 1+ swap
     REPEAT  rdrop ;

  : scan ( addr0 len0 char -- addr1 len1 )   >r
     BEGIN  dup
     WHILE  over c@ r@ - WHILE  1- swap 1+ swap
     REPEAT  rdrop ;

  : /string ( addr0 len0 +n -- addr1 len1 )
     over umin rot over + -rot - ;
( ----- 050 )
\ capital                                         ks 19 dez 87

  Create (capital  Assembler   $61 # A- cmp  CS not
     ?[  $7B # A- cmp  CS not
         ?[  $84 # A- cmp  0= ?[  $8E # A- mov  ret  ]?  \
             $94 # A- cmp  0= ?[  $99 # A- mov  ret  ]?  \
             $81 # A- cmp  0= ?[  $9A # A- mov  ]?  ret  \
         ]?  $20 # A- xor
     ]?  ret   end-code

  Code capital ( char -- char' )
     A D xchg   (capital # call   A D xchg   Next
  end-code
( ----- 051 )
\ upper                                           ks 03 aug 87

  Code upper   ( addr len -- )
     D C mov   W pop   D pop   C0= not
     ?[  [[  W ) A- mov   (capital # call
             A- W ) mov  W inc  C0= ?]  ]?   Next
  end-code

\\ high level definition, without umlauts

  : capital ( char -- char')
     dup  [char] a   [ char z 1+ ] Literal
     uwithin not ?exit   [ char a  char A - ] Literal - ;

  : upper  ( addr len -- )
     bounds ?DO  I c@ capital I c!  LOOP ;
( ----- 052 )
\ (word                                           ks 28 mai 87

| Code (word  ( char addr0 len0 -- addr1 )   D C mov   W pop
     A pop   >in #) D mov   D C sub  >= not
     ?[  C push   D W add   0=rep byte scas   W D mov  0= not
         ?[  W dec   D dec   C inc
             0<>rep byte scas   0= ?[  W dec  ]?
         ]?  A pop   C A sub   A >in #) add
         W C mov   D C sub  0= not
         ?[  D I xchg   u' dp U D) W mov   C- W ) mov
             W inc   rep byte movs   $20 # W ) byte mov
             D I mov   u' dp U D) D mov   Next
swap ]?  C >in #) add
         ]?  u' dp U D) W mov   $2000 # W ) mov   W D mov   Next
  end-code
( ----- 053 )
\  postpone                               cs 19 apr 21

  : postpone
    ' dup >name c@ $40 and
    IF , ELSE [compile] compile compile , THEN ; immediate

\\  (word
| : (word  ( char adr0 len0 -- addr )
     rot  >r  over swap   >in @ /string   r@ skip
     over swap   r> scan >r   rot over swap - r> 0<> - >in !
     over - here  dup >r  place  bl r@ count  + c!  r> ;

( ----- 054 )
\ source word parse name                          ks 03 aug 87

  defer source
  : (source ( -- addr len )  tib #tib @ ;
  ' (source Is source

  : word ( char -- addr )   source (word ;

  : parse ( char -- addr len )   >r  source  >in @ /string
     over swap   r> scan >r  over - dup  r> 0<>  -  >in +! ;

  : name ( -- string )   bl word dup count upper exit ;
( ----- 055 )
\ state char [char] ," "lit (" "            cs 19 apr 21
  Variable state   state off
  : char ( "char" -- c ) bl word 1+ c@ ;
  : [char]  ( "char" --  )
    char [compile] Literal ; immediate restrict
   : ,"  [char] " parse  here over 1+ allot place ;
  Code "lit    ( -- addr )   D push   R ) D mov   D W mov
     W ) A- mov   0 # A+ mov   A inc   A R ) add   Next
  end-code restrict
\ : "lit  r> r> under  count + even >r >r ;   restrict
  : ("    "lit ; restrict
  : "     compile (" ," align ; immediate restrict
( ----- 056 )
\ ." ( .( \ \\ hex decimal                        ks 12 dez 88

  : (."      "lit count type ; restrict
  : ."       compile (." ," align ; immediate restrict

  : (        [char] ) parse 2drop ; immediate
  : .(       [char] ) parse type ; immediate

  : \        >in @ negate   c/l mod   >in +! ; immediate
  : \\       b/blk >in ! ; immediate
  : have   ( <name> -- f )  name find nip   0<> ; immediate
  : \needs   have 0=exit  [compile] \  ;

  : hex      $10 base ! ;
  : decimal  &10 base ! ;
( ----- 057 )
\ number conversion: digit? accumulate convert    ks 08 okt 87

  : digit? ( char -- digit true/ false )  dup  [char] 9 >
 ( IF  [ char A char 9 - 1- ] Literal -  dup [char] 9 >  and)
     IF 7 - dup [char] 9 > and
     THEN  [char] 0 -   dup base @ u<  dup ?exit  nip ;

  : accumulate ( +d0 adr digit -- +d1 adr )   swap >r
     swap  base @ um* drop   rot  base @ um*  d+   r> ;

  : convert ( +d1 addr0 -- +d2 addr2 )
     1+  BEGIN  count digit? WHILE  accumulate  REPEAT 1- ;
( ----- 058 )
\ number conversion                               ks 29 jun 87
| : end?       ( -- flag )               >in @ 0= ;

| : nchr       ( addr0 -- addr1 char )   count -1 >in +! ;

| : previous   ( addr0 -- addr0 char )   1- count ;

| : punctuation?   ( char -- flag )
     [char] , over =   swap [char] . =  or ;
\ : punctuation?  ( char -- f )   ?" .," ;

| : fixbase?   ( char -- char false / newbase true )  capital
     [char] $ case? IF $10 true exit  THEN
     [char] H case? IF $10 true exit  THEN
     [char] & case? IF &10 true exit  THEN
     [char] % case? IF   2 true exit  THEN     false ;
( ----- 059 )
\ number conversion: dpl ?num ?nonum ?dpl         ks 27 oct 86

  Variable dpl      -1 dpl !

| : ?num      ( flag -- exit if true )  0=exit
     rdrop drop r> IF  dnegate  THEN   rot drop
     dpl @ 1+ ?dup ?exit  drop true ;

| : ?nonum     ( flag -- exit if true ) 0=exit
     rdrop 2drop drop rdrop false ;

| : ?dpl     dpl @  -1 =  ?exit  1 dpl +! ;
( ----- 060 )
\ number conversion: number?  number              ks 27 oct 86

  : number?   ( string -- string false / n 0< / d 0> )
     base push  >in push  dup count >in !  dpl on
     0 >r ( +sign)   0.0   rot end? ?nonum nchr
     [char] - case?  IF  rdrop true >r end? ?nonum nchr  THEN
     fixbase?       IF  base !        end? ?nonum nchr  THEN
     BEGIN digit? 0= ?nonum
           BEGIN  accumulate ?dpl end? ?num nchr digit?
        0= UNTIL  previous  punctuation?  0= ?nonum
           dpl off  end? ?num  nchr
     REPEAT ;

  : number ( string -- d )
     number? ?dup 0= Abort" ?"  0> ?exit  extend ;
( ----- 061 )
\ hide reveal immediate restrict                  ks 18 m�r 88
  Variable last     last off

  : last'   ( -- cfa )                last @ name> ;

| : last?   ( -- false / nfa true)    last @ ?dup ;
  : hide          last? 0=exit  2- @ current @ ! ;
  : reveal        last? 0=exit  2-   current @ ! ;

  : Recursive     reveal ; immediate restrict

| : flag!    ( 8b --)
     last?  IF  under c@ or over c!  THEN   drop  ;

  : immediate     $40 flag! ;
  : restrict      $80 flag! ;
( ----- 062 )
\ clearstack hallot heap heap?                    ks 27 oct 86

  Code clearstack   u' s0 U D) S mov   D pop   Next   end-code

  : hallot  ( quan -- )
     s0 @  over -  swap    sp@ 2+  dup rot -   dup s0 !
     2 pick  over -    di  move  clearstack  ei   s0 ! ;

  : heap    ( -- addr )        s0 @ 6 + ;
  : heap?   ( addr -- flag )   heap up@ uwithin ;

| : heapmove   ( from -- from )
     dup  here over -  dup hallot
     heap swap cmove   heap over - last +!  reveal ;
( ----- 063 )
\ Does>  ;                                        ks 18 m�r 88

| Create dodo   Assembler
     R dec   R dec   I R ) mov      \ push IP
     D push   2 W D) D lea          \ load parameter address
     W ) I mov   3 # I add   Next   end-code

  dodo Host tdodo ! Target       \ target compiler needs to know

  : (;code          r> last' ! ;

  : Does>     compile (;code   $E9 c,  ( jmp instruction)
     dodo here 2+ - , ; immediate restrict
( ----- 064 )
\ ?head  |  alignments                            ks 19 m�r 88
  Variable ?head     ?head off

  : |                ?head @  ?exit  ?head on ;

 \ no alignment required on x86
  : even   ( addr -- addr1 ) ; immediate
  : align  ( -- )            ; immediate
  : halign ( -- )            ; immediate

  Variable warning    warning on

| : ?exists   warning @  0=exit
     last @ current @ (find nip 0=exit
     space last @ .name ." exists " ?cr ;
( ----- 065 )
\ Create Variable                                 ks 19 m�r 88

  Defer makeview         ' 0 Is makeview

  : Create    align  here  makeview ,  current @ @ ,
     name c@ dup 1 $20 uwithin  not Abort" invalid name"
     here last !  1+ allot  align   ?exists
     ?head @ IF    1 ?head +!   dup ,   \ Pointer to Code
                   halign  heapmove   $20 flag!   dup dp !
             THEN  drop reveal 0 ,
  ;Code  ( -- addr )    D push   2 W D) D lea   Next  end-code

  : Variable            Create 0 , ;
( ----- 066 )
\ nfa?                                            ks 28 mai 87

  Code nfa?   ( thread cfa -- nfa / false )
     W pop   R A mov   $1F # C mov
     [[  W ) W mov   W W or  0= not
     ?[[  2 W D) R- mov   C R and   3 R W DI) R lea
          $20 # 2 W D) test  0= not ?[  R ) R mov  ]?
          D R cmp  0= ?]  2 W D) W lea
     ]?  W D mov   A R mov   Next   end-code

\\

  : nfa?    ( thread cfa -- nfa / false )   >r
     BEGIN  @ dup 0= IF  rdrop exit  THEN
            dup 2+ name> r@ = UNTIL  2+ rdrop ;
( ----- 067 )
\ >name name> >body .name                         ks 13 aug 87

  : >name   ( acf -- anf / ff )     voc-link
     BEGIN  @ dup WHILE  2dup 4 - swap nfa?
            ?dup IF  -rot 2drop exit  THEN  REPEAT nip ;

  : (name>   ( nfa -- cfa )   count  $1F and + even ;

  : name> ( nfa -- cfa )
     dup (name> swap  c@ $20 and 0=exit  @ ;

  : >body   ( cfa -- pfa )       2+ ;
  : body>   ( pfa -- cfa )       2- ;

  : .name   ( nfa -- ) ?dup  IF  dup heap?  IF ." | " THEN
     count $1F and type  ELSE ." ???"  THEN space ;
( ----- 068 )
\ : ; Constant Variable                           ks 29 oct 86

  : Create:  Create  hide  current @ context !  0 ] ;

  : :        Create:
  ;Code   R dec   R dec   I R ) mov   2 W D) I lea   Next
  end-code

  : ;        0 ?pairs   compile unnest   [compile] [   reveal ;
  immediate restrict

  : Constant ( n -- )   Create ,
  ;Code      ( -- n )   D push   2 W D) D mov   Next   end-code
( ----- 069 )
\ uallot User Alias Defer                         ks 02 okt 87
  : uallot   ( quan -- offset )   even    dup udp @ +
     $FF u> Abort" Userarea full"   udp @   swap udp +! ;

  : User    Create 2 uallot c,
  ;Code   ( -- addr )   D push   2 W D) D- mov
                        0 # D+ mov   U D add   Next   end-code

  : Alias ( cfa -- )
     Create  last @ dup c@ $20 and
     IF  -2 allot  ELSE  $20 flag!  THEN  (name> ! ;

| : crash           true Abort" crash" ;

  : Defer     Create ['] crash ,
  ;Code   2 W D) W mov   W ) jmp   end-code
( ----- 070 )
\ vp current context also toss                    ks 02 okt 87

  Create vp  $10 allot
  Variable current

  : context   ( -- adr )          vp dup @ + 2+ ;

| : thru.vocstack ( -- from to )    vp 2+ context ;

\ "Only Forth also Assembler" gives
\ vp:  countword = 6 | Root | Forth | Assembler |

  : also          vp @ &10 > Error" Vocabulary stack full"
                  context @  2 vp +!  context ! ;

  : toss          vp @ 0=exit   -2 vp +! ;
( ----- 071 )
\ Vocabulary Forth Only Onlyforth definitions     ks 19 jun 88
  : Vocabulary  Create  0 , 0 ,  here  voc-link @ ,  voc-link !
  Does>   context ! ;
\  | Name | Code | Thread | Coldthread | Voc-link |

  Vocabulary Forth
Host  h' Transient 8 + @  T h' Forth 8 + H !
Target  Forth also definitions

  Vocabulary Root

  : Only     vp off  Root also ;

  : Onlyforth   Only Forth also definitions ;

  : definitions            context @ current ! ;
( ----- 072 )
\ order vocs words                                ks 19 jun 88
| : init-vocabularys        voc-link @
     BEGIN  dup 2- @ over 4- ! @ ?dup 0= UNTIL ;
| : .voc   ( adr -- )      @ 2- >name .name ;

  : order    vp 4+  context over umax
     DO  I .voc  -2 +LOOP   2 spaces current .voc ;

  : vocs   voc-link
     BEGIN  @ ?dup WHILE  dup 6 - >name .name  REPEAT ;

  : words  ( -- )   [compile] char capital >r   context @
     BEGIN  @ dup  stop? 0=  and
     WHILE  ?cr dup 2+  r@ bl = over 1+ c@ r@ = or
       IF  .name space  ELSE  drop  THEN
     REPEAT drop rdrop ;
( ----- 073 )
\ (find  found                                    ks 09 jul 87
| : found ( nfa -- cfa n )   dup c@ >r
     (name> r@ $20 and  IF  @       THEN
         -1 r@ $80 and  IF  1-      THEN
            r> $40 and  IF  negate  THEN ;

  Code (find   ( string thread -- string ff / anf tf )
     D I xchg   W pop   D push   W ) A- mov   W inc
     W D mov   0 # C+ mov   $1F # A+ mov   A+ A- and
     [[  I ) I mov   I I or  0= not
    ?[[  2 I D) C- mov   A+ C- and   A- C- cmp   dup 0= ?]
         I push   D W mov   3 # I add
                            0=rep byte cmps   I pop  0= ?]
         3 # I add   I W mov   -1 # D mov
     ][  D W mov   0 # D mov  ]?   W dec   I pop   W push   Next
  end-code
( ----- 074 )
\\  -text (find                                   ks 02 okt 87

  : -text ( adr1 len adr2 -- 0< 1<2 / 0= 1=2 / 0> 1>2 )
     over bounds
     DO  drop count I c@ - dup IF LEAVE THEN  LOOP nip ;

  : (find    ( string thread -- str false / NFA +n )
     over c@ $1F and >r  @
     BEGIN  dup WHILE  dup @   swap 2+   dup c@ $1F and  r@  =
                       IF  dup 1+  r@  4 pick 1+ -text
                           0= IF  rdrop -rot drop exit
                       THEN   THEN  drop
     REPEAT  rdrop ;
( ----- 075 )
\ find  '  [compile]  [']  nullstring?            ks 29 oct 86

  : find    ( string -- acf n / string false )
     context   dup @  over 2- @  = IF  2-  THEN
     BEGIN  under @ (find  IF  nip found exit  THEN
            swap 2-   dup vp = UNTIL  drop false ;

  : '    ( -- cfa )      name find ?exit Error" ?" ;

  : [compile]       ' , ;                 immediate restrict

  : [']             ' [compile] Literal ; immediate restrict

  : nullstring?   ( string -- string false / true )
     dup c@  0= dup 0=exit  nip ;
( ----- 076 )
\ interpreter                                     ks 07 dez 87

  Defer notfound

| : interpreter   ( string -- )   find ?dup
     IF  1 and IF  execute exit  THEN
         Error" compile only"
     THEN  number? ?exit  notfound ;

| : compiler    ( string -- )   find ?dup
     IF  0> IF  execute exit  THEN   , exit  THEN
     number? ?dup IF  0> IF  swap [compile] Literal  THEN
                         [compile] Literal  exit
                  THEN  notfound ;
( ----- 077 )
\ compiler [ ]                                    ks 16 sep 88

  : no.extensions  ( string -- )
     state @ IF  Abort" ?"  THEN  Error" ?" ;

  ' no.extensions Is notfound

  Defer parser   ( string -- )    ' interpreter Is parser

  : interpret
     BEGIN  ?stack name nullstring? IF  aborted off exit  THEN
            parser  REPEAT ;

  : [      ['] interpreter Is parser  state off ; immediate

  : ]      ['] compiler    Is parser  state on ;
( ----- 078 )
\  Is                                             ks 07 dez 87

  : (is      r> dup 2+ >r @ ! ;

| : def?  ( cfa -- )
     @  [ ' notfound @   ] Literal   - Abort" not deferred" ;

  : Is   ( addr -- )     '  dup def?   >body
     state @ IF  compile (is , exit  THEN  ! ; immediate
( ----- 079 )
\ ?stack                                          ks 01 okt 87

| : stackfull ( -- )     depth $20 > Abort" tight stack"
    reveal last? IF dup heap? IF name> ELSE 4- THEN (forget THEN
    true Abort" dictionary full" ;

  Code ?stack    u' dp U D) A mov   S A sub  CS
     ?[ $100 # A add  CS ?[ ;c: stackfull ; Assembler  ]? ]?
     u' s0 U D) A mov   A inc   A inc   S A sub
     CS not ?[  Next  ]?  ;c: true Abort" stack empty" ;

\ : ?stack     sp@ here - $100 u< IF  stackfull  THEN
\              sp@ s0 @ u> Abort" stack empty" ;
( ----- 080 )
\ .status push                                    ks 29 oct 86

| Create: pull  r> r> ! ;
  : push   ( addr -- )
     r> swap dup >r @ >r pull >r >r ; restrict

  Defer .status   ' noop Is .status


( ----- 081 )
\ depth rdepth postpone value to
  : rdepth   ( -- +n )           r0 @ rp@ 2+   - 2/ ;
  : depth    ( -- +n )           sp@ s0 @ swap - 2/ ;

  : value create , DOES> @ ;
  : TO ( x "<spaces>name" -- )
    ' >body state @
    IF [compile] Literal ! ELSE ! THEN ; immediate

( ----- 082 )
\  prompt  quit                                   ks 16 sep 88
  : (prompt   .status  state @ IF  cr ." ] " exit  THEN
     aborted @ 0= IF  ."  ok"  THEN  cr ;

  Defer prompt    ' (prompt Is prompt

  : (quit  BEGIN  prompt query interpret  REPEAT ;

  Defer 'quit     ' (quit Is 'quit

  : quit  r0 @ rp!   [compile] [   blk off
    key? IF key drop THEN
    'quit ;

( ----- 083 )
\ end-trace abort                                 ks 26 jul 87

  : standardi/o     [ output ] Literal output 4 cmove ;

  Code end-trace    next-link # W mov   $AD # A- mov
     $FF97 # C mov   [[  W ) W mov   W W or  0= not
                     ?[[  A- -4 W D) mov   C -3 W D) mov
                     ]]?  lods   A W xchg   W ) jmp   end-code

  Defer 'abort     ' noop Is 'abort

  : abort    end-trace clearstack 'abort standardi/o quit ;
( ----- 084 )
\ (error Abort" Error"                            ks 16 sep 88
  Variable scr      1 scr !
  Variable r#       r# off

  : (error ( string -- )   rdrop r> aborted !  standardi/o
     space here .name   count type space ?cr
     blk @ ?dup IF  scr ! >in @ r# !  THEN  quit ;
  ' (error errorhandler !

  : (abort"    "lit swap IF  >r clearstack r>
     errorhandler perform exit THEN drop ; restrict

| : (error"    "lit swap IF  errorhandler perform exit  THEN
               drop ; restrict
( ----- 085 )
\ -trailing space spaces                          ks 16 sep 88

  : Abort"     compile (abort" ," align ; immediate restrict
  : Error"     compile (error" ," align ; immediate restrict

  $20 Constant bl

  : -trailing ( addr n1 -- addr n2)
     dup 0 ?DO  2dup + 1- c@ bl - IF LEAVE THEN  1-  LOOP ;

  : space                bl emit ;
  : spaces   ( u -- )    0 ?DO  space  LOOP ;
( ----- 086 )
\ hold <# #> sign # #s                            ks 29 dez 87

| : hld   ( -- addr)              pad 2- ;

  : hold    ( char -- )           -1 hld +!   hld @ c! ;

  : <#                            hld hld ! ;

  : #>      ( 32b -- addr +n )    2drop   hld @   hld over - ;

  : sign    ( n -- )              0< not ?exit  [char] - hold ;

  : #       ( +d1 -- +d2)
     base @ ud/mod   rot dup 9 >  7 and +  [char] 0 +  hold ;

  : #s      ( +d -- 0 0 )         BEGIN # 2dup d0= UNTIL ;
( ----- 087 )
\ print numbers .s                                ks 07 feb 89

  : d.r   ( d +n -- )   -rot under dabs <# #s rot sign #>
                        rot over max over - spaces type ;
  : d.    ( d -- )      0 d.r space ;

  : .r    ( n +n -- )   swap extend rot d.r ;
  : .     ( n -- )      extend d. ;

  : u.r   ( u +n -- )   0 swap d.r ;
  : u.    ( u -- )      0 d. ;

  : .s    sp@ s0 @ over - $20 umin bounds ?DO I @ u. 2 +LOOP ;
( ----- 088 )
\ c/l l/s

  &64 Constant c/l        \ Screen line length
  &16 Constant l/s        \ lines per screen

( ----- 089 )
\ multitasker primitives                          ks 29 oct 86

  Code pause    D push   I push   R push
     S 6 U D) mov   2 U D) U add   4 # U add   U jmp
  end-code

  : lock ( addr -- )
     dup @  up@  = IF  drop exit  THEN
     BEGIN  dup @ WHILE  pause  REPEAT  up@ swap ! ;

  : unlock   ( addr -- )        dup lock off ;

  Label wake   Assembler   U pop   2 # U sub   A pop
     popf   6 U D) S mov   R pop   I pop   D pop   Next
  end-code
  $E9 4 * >label >taskINT
( ----- 090 )

( ----- 091 )
  $10000 Constant limit     Variable first
  $408 Constant b/buf               \ real size of block buffer
  $400 Constant b/blk               \ bytes/block

  Defer r/w                         \ low level disk access word

( ----- 092 )

( ----- 093 )

( ----- 094 )

( ----- 095 )

( ----- 096 )

( ----- 097 )

( ----- 098 )

( ----- 099 )
\ endpoints of forget                             uh 27 apr 88

| : |? ( nfa -- flag )   c@ $20 and ;

| : forget? ( adr nfa -- flag )   \ code in heap or above adr ?
     name>  under  1+ u<  swap  heap?  or ;

| : endpoint ( addr sym thread -- addr sym' )
     BEGIN  BEGIN  @  2 pick  over  u> IF  drop exit  THEN
                   dup heap? UNTIL  dup >r 2+ dup |?
        IF  >r over r@ forget? IF  r@ (name> >body  umax  THEN
            rdrop  THEN  r>
     REPEAT ;

| : endpoints ( addr -- addr symb )   heap  voc-link @
     BEGIN  @ ?dup WHILE  dup >r 4- endpoint r> REPEAT ;
( ----- 100 )
\ remove, -words, -tasks                          ks 30 apr 88
  : remove ( dic sym thread -- dic sym )
     BEGIN dup @ ?dup      \ unlink forg. words
     WHILE dup heap?
       IF  2 pick over u>  ELSE  3 pick over 1+ u<  THEN
       IF  @ over ! ( unlink word)  ELSE nip THEN  REPEAT drop ;

| : remove-words ( dic sym -- dic sym )   voc-link
     BEGIN  @ ?dup WHILE  dup >r  4- remove  r> REPEAT ;

| : >up   2+ dup @ 2+ + ;

| : remove-tasks  ( dic -- )  up@
     BEGIN  dup >up up@ - WHILE  2dup >up swap here uwithin
        IF dup >up >up over - 2- 2- over 2+ !  ELSE  >up  THEN
     REPEAT  2drop ;
( ----- 101 )
\ remove-vocs trim                                ks 31 oct 86

| : remove-vocs ( dic symb -- dic symb )
     voc-link remove     thru.vocstack
     DO  2dup I @ -rot uwithin
         IF  [ ' Forth 2+ ] Literal I !  THEN  -2 +LOOP
     2dup  current @  -rot  uwithin 0=exit
     [ ' Forth 2+ ] Literal current ! ;

  Defer custom-remove     ' noop Is custom-remove

  : trim   ( dic symb -- )  next-link remove
     over  remove-tasks remove-vocs remove-words
     custom-remove  heap swap - hallot dp !  last off ;
( ----- 102 )
\ deleting words from dict.                       ks 02 okt 87

  : clear        here  dup up@  trim  dp ! ;

  : (forget ( adr -- )
     dup heap? Abort" is symbol"  endpoints  trim ;

  : forget   ' dup [ dp ] Literal @  u< Abort" protected"
     >name  dup  heap? IF  name>  ELSE  4-  THEN  (forget ;

  : empty   [ dp ] Literal @ up@ trim
            [ udp ] Literal @ udp ! ;
( ----- 103 )
\ save stop? ?cr                              ks 1UH 26sep88

  : save    here  up@ trim   up@ origin $100 cmove
     voc-link @ BEGIN  dup 4- @ over 2- ! @ ?dup  0= UNTIL ;

  $1B Constant #esc

| : end?   key #esc case? 0=
     IF  #cr case? 0= IF 3 ( Ctrl-C ) - ?exit THEN  THEN
     true rdrop ;

  : stop? ( -- flag )   key? IF  end? end?  THEN  false ;

  : ?cr       col c/l u> 0=exit  cr ;
( ----- 104 )
\ in/output structure                             ks 31 oct 86

| : Out:   Create dup c, 2+ Does> c@ output @ + perform ;

  : Output:  Create: Does> output ! ;
0   Out: emit   Out: cr   Out: type   Out: del
    Out: page   Out: at   Out: at?    drop

  : row ( -- row )     at? drop ;
  : col ( -- col )     at? nip ;

| : In:    Create dup c, 2+ Does> c@ input @ + perform ;

  : Input:   Create:  Does> input ! ;
0   In: key   In: key?   In: decode   In: expect  drop
( ----- 105 )
\ Alias  only definitionen                        ks 31 oct 86

  Root definitions

  : seal  [ ' Root >body ] Literal off ; \ "erases" Root Vocab.

  ' Only        Alias Only
  ' Forth       Alias Forth
  ' words       Alias words
  ' also        Alias also
  ' definitions Alias definitions

  Forth definitions
( ----- 106 )
\ 'restart  'cold                                 ks 01 sep 88

  Defer 'restart  ' noop Is 'restart

| : (restart   ['] (quit Is 'quit  'restart
     [ errorhandler ] Literal @ errorhandler !
     ['] noop Is 'abort  end-trace clearstack
     standardi/o interpret quit ;

  Defer 'cold    ' noop Is 'cold

| : (cold      origin up@ $100 cmove   $80 count
     $50 umin >r tib r@ move  r> #tib !  >in off  blk off
     init-vocabularys 'cold  Onlyforth $80 c@ 0= IF
     page logo count type cr THEN (restart ;
( ----- 107 )
\ (boot                                           ks 11 m�r 89

  Label #segs  ( -- R: seg )   Assembler
     C: seg ' limit >body #) R mov   R R or  0= not
     ?[  4 # C- mov   R C* shr   R inc   ret  ]?
     $1000 # R mov   ret
  end-code

  Label (boot   Assembler   cli   cld   A A xor   A D: mov
     #segs # call   C: D mov   D R add   R E: mov
     $200 # C mov   0 # I mov   I W mov   rep movs
       wake # >taskINT #) mov   C: >taskINT 2+ #) mov
     divovl #  >divINT #) mov   C:  >divINT 2+ #) mov   ret
  end-code
( ----- 108 )
\ restart                                         ks 09 m�r 89

  Label warmboot   here >restart 2+ -  >restart ! Assembler
         (boot # call
  here   ' (restart >body # I mov
  Label bootsystem
     C: A mov   A E: mov   A D: mov   A S: mov
     s0 #) U mov   6 # U add   u' s0 U D) S mov
     D pop   u' r0 U D) R mov   sti   Next
  end-code

  Code restart   here 2- !   end-code
( ----- 109 )
\  bye                                            ks 11 m�r 89

  : bye       empty poweroff ;

( ----- 110 )
\ cold                                            ks 09 m�r 89

  here  >cold 2+  -   >cold !  Assembler
     (boot # call   C: A mov   A D: mov  A E: mov
  \  #segs # call   $41 # R add  \ another k for the ints
  \  $4A # A+ mov   $21 int        \ alloc memory
  \ CS ?[  $10 # return_code #) byte mov   ' (bye @ # jmp  ]?
  here   s0 #) W mov   6 # W add   origin # I mov   $20 # C mov
     rep movs   ' (cold >body # I mov   bootsystem # jmp
  end-code

  Code cold   here 2- !   end-code
( ----- 111 )
\ System patchup                                  ks 16 sep 88

  1 &11 +thru      \ PC-BIOS interface
  : forth-83 ;     \ last word in Dictionary

  0 ' limit >body !   $DFF6 s0 !    $E77C r0 !
  s0 @ s0 2- !   here dp !

  Host  tudp @       Target   udp !
  Host  tvoc-link @  Target   voc-link !
  Host  tnext-link @ Target   next-link !
  Host  tfile-link @ Target Forth  file-link !
  Host  T move-threads H
  save-buffers cr .( unresolved: )  .unresolved
( ----- 112 )
\ lc@ lc!  l@ l!  special 8088 operators          ks 27 oct 86

  Code lc@  ( seg:addr -- 8b )   D: pop   D W mov
     W ) D- mov   0 # D+ mov   C: A mov   A D: mov   Next
  end-code

  Code lc!  ( 8b seg:addr -- )   D: pop   A pop   D W mov
     A- W ) mov   C: A mov   A D: mov   D pop   Next   end-code

  Code l@  ( seg:addr -- 16b )   D: pop   D W mov
     W ) D mov   C: A mov   A D: mov   Next   end-code

  Code l!  ( 16b seg:addr -- )   D: pop   A pop   D W mov
     A W ) mov   C: A mov   A D: mov   D pop   Next   end-code
( ----- 113 )
\ ltype  lmove    special 8088 operators          ks 11 dez 87

  : ltype   ( seg:addr len -- )
     0 ?DO  2dup I + lc@ emit  LOOP  2drop ;

  Code lmove  ( from.seg:addr to.seg:addr quan -- )
     A I xchg   D C mov   W pop   E: pop
     I pop   D: pop   I W cmp  CS
     ?[  rep byte movs
     ][  C dec   C W add   C I add   C inc
         std   rep byte movs   cld
     ]?  A I xchg   C: A mov   A E: mov
     A D: mov   D pop   Next   end-code
( ----- 114 )

( ----- 115 )
\ APM PC Shutdown - poweroff

 CODE poweroff ( -- )
   \ Connect to APM API
   $5301 # A mov   R R xor   $15 int
   \ Try to set APM version (to 1.2)
   $530E # A mov   R R xor   $0102 # C mov  $15 int
   \ Turn off the system
   $5307 # A mov  $01 # R mov  $03 # C mov  $15 int
 END-CODE
( ----- 116 )
\ BIOS  keyboard input                           ks 16 sep 88

  Code (key@  ( -- 8b )  D push   A+ A+ xor   $16 int
     A- D- xchg   0 # D+ mov   Next   end-code

  Code (key?  ( -- f )   D push   1 # A+ mov   D D xor
     $16 int   0= not ?[  D dec  ]?   Next   end-code

  : empty-keys ;

  : (key  ( -- 8b )   BEGIN  pause (key? UNTIL  (key@ ;

( ----- 117 )
\ (decode expect                                  ks 16 sep 88
   7 Constant #bel            8 Constant #bs
   9 Constant #tab           $A Constant #lf
  $D Constant #cr           $26 Constant #eof
  : (decode  ( addr pos1 key -- addr pos2 )
     #bs  case? IF  dup 0=exit del 1- exit  THEN
     #cr  case? IF  dup span ! space  exit  THEN
     #lf  case? IF  exit THEN
     #eof case? IF  bye  THEN
     >r  2dup +  r@ swap c!  r> emit  1+ ;

  : (expect ( addr len1 -- )  span !   0
     BEGIN   dup span @ u< WHILE  key decode  REPEAT  2drop ;

  Input: keyboard [ here input ! ]
          (key (key? (decode (expect [ drop
( ----- 118 )
\ BIOS character output                          ks 29 jun 87

  Code charout  ( char -- )   D- A- mov
     $E # A+ mov   $10 int   D pop   ' pause # W mov   W ) jmp
  end-code

  &80 Constant c/row            &25 Constant c/col

  : (emit   ( char -- )  dup bl u< IF  $80 or  THEN  charout ;
  : (cr                  #cr charout #lf charout ;
  : (del                 #bs charout bl charout #bs charout ;
  : (at                  2drop ;
  : (at?                 0 0 ;
  : (page                c/col 0 DO  cr  LOOP ;
( ----- 119 )
\ BIOS character output                          ks  7 may 85

  : bell   #bel charout ;

  : tipp   ( addr len -- )  bounds ?DO  I c@ emit  LOOP ;

  Output: display [ here output ! ]
           (emit (cr  tipp (del (page (at (at? [ drop

( ----- 120 )

  Code pc@    ( port -- 8b )
     D byte in   A- D- mov   D+ D+ xor   Next
  end-code

  Code pc!    ( 8b port -- )
     A pop   D byte out   D pop   Next
  end-code
( ----- 121 )
\ zero terminated strings                            cas 25jan06

  : counted   ( asciz -- addr len )
     dup -1 0 scan drop over - ;

  : >asciz   ( string addr -- asciz )   2dup >r  -
     IF  count r@ place  r@  THEN  0 r> count + c!  1+ ;

  : asciz    ( -- asciz )   name here >asciz ;
