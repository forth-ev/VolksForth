\ *** Block No. 2, Hexblock 2

\ FORTH Preamble and ID                            uho 19May2005

2 .pagestatus

Assembler

nop  0 jmp   here 2- >label >boot
nop  0 jmp   here 2- >label >cold
nop  0 jmp   here 2- >label >restart

here dup origin!
\ Hier beginnen die Kaltstartwerte der Benutzervariablen

6 rst   0 jmp   end-code  \ for multitasker

$100 allot

| Create logo ," volksFORTH-83 rev. 3.80a"

\ *** Block No. 3, Hexblock 3

\ Assembler Labels Next Forth-Register                   29Jun86

Label dpush   D push    Label hpush   H push
Label >next
   IP ldax   IP inx   A L mov   IP ldax   IP inx   A H mov
Label >next1
   M E mov   H inx   M D mov   xchg   pchl
end-code

Variable RP
Variable UP
\ IP in BC
\ W  in DE
\ SP in SP
Variable IPsave


\ *** Block No. 4, Hexblock 4

\ Assembler Macros                                       20Oct86
Compiler Assembler also definitions  Forth
: Next    T >next jmp [ Forth ]  ;
T hpush Forth Constant hpush   T dpush Forth Constant dpush
T >next Forth Constant >next

: rpush ( reg -- )  RP lhld  H dcx  DUP M mov ( high )
   H dcx   1+ M mov ( low )   RP shld  [ Forth ] ;

: rpop  ( reg -- )  RP lhld   M over 1+ mov ( low )  H inx
   M swap mov ( high )  H inx   RP shld [ Forth ] ;
\  rpush und rpop gehen nicht mit HL

: mvx ( src dest -- )
   2dup  mov ( high )  1+ swap 1+ swap mov  ( low ) [ Forth ] ;
Target

\ *** Block No. 5, Hexblock 5

\ recover ;c: noop                                       20Oct86

Create recover   Assembler
   W pop   IP rpush   W IP mvx
Next end-code

Compiler Assembler also definitions   Forth

: ;c:   0 T recover call  end-code   ]  [ Forth ] ;

Target

| Code di   di Next end-code
| Code ei   ei Next end-code

Code noop   >next here 2- !   end-code

\ *** Block No. 6, Hexblock 6

\ User variables                                         04Oct87

Constant origin   8 uallot drop  \ Multitasker
     \ Felder: entry  link  spare  SPsave
     \ Laenge kompatibel zum 68000 und 6502 volksFORTH
User s0
User r0
User dp
User offset            0 offset !
User base              $0A base !
User output
User input
User errorhandler       \ pointer for Abort" -code
User voc-link
User udp                \ points to next free addr in User


\ *** Block No. 7, Hexblock 7

\ manipulate system pointers                             11Jun86

Code sp@  ( -- addr)   0 H lxi   SP dad   hpush jmp   end-code

Code sp!  ( addr --)   H pop   sphl   Next   end-code


Code up@  ( -- addr)   UP lhld   hpush jmp   end-code

Code up!  ( addr --)   H pop   UP shld   Next   end-code







\ *** Block No. 8, Hexblock 8

\ manipulate returnstack                                 11Jun86

Code rp@ ( -- addr )   RP lhld   hpush jmp   end-code

Code rp! ( addr -- )   H pop   RP shld   Next  end-code


Code >r  ( 16b -- )    D pop   D rpush   Next end-code restrict

Code r>  ( -- 16b )    D rpop   D push   Next end-code restrict







\ *** Block No. 9, Hexblock 9

\ r@ rdrop  exit unnest ?exit                            07Oct87
Code r@ ( -- 16b )
   RP lhld   M E mov   H inx   M D mov   D push   Next  end-code

Code rdrop
   RP lhld   H inx   H inx   RP shld   Next end-code   restrict

Code exit    Label >exit   IP rpop   Next  end-code
Code unnest   >exit  here 2- !

Code ?exit ( flag -- )
   H pop   H A mov   L ora   >exit jnz   Next end-code

Code 0=exit ( flag -- )
   H pop   H A mov   L ora   >exit jz    Next end-code
\ : ?exit ( flag -- )   IF rdrop THEN ;

\ *** Block No. 10, Hexblock a

\ execute  perform                             11Jun86   18Nov87

Code execute   ( cfa -- )
   H pop >Next1 jmp end-code

Code perform   ( 'cfa -- )
   H pop    M A mov   H inx   M H mov   A L mov  >Next1 jmp
end-code


\ \\
\ : perform   ( addr -- )      @ execute ;





\ *** Block No. 11, Hexblock b

\ c@ c! ctoggle                                          07Oct87

Code c@   ( addr -- 8b )
   H pop   M L mov   0 H mvi   hpush jmp   end-code

Code c!   ( 16b addr -- )
   H pop   D pop    E M mov   Next   end-code

Code flip ( 16b1 -- 16b2 )
   H pop   H A mov   L H mov   A L mov   Hpush jmp   end-code

Code ctoggle ( 8b addr -- )
   H pop   D pop   M A mov   E xra   A M mov   Next  end-code

\ \\
\ : ctoggle   ( 8b addr --)      under c@ xor swap c! ;

\ *** Block No. 12, Hexblock c

\ @ ! 2@ 2!                                    11Jun86   18Nov87

Code @  ( addr -- 16b )   H pop   Label fetch
  M E mov   H inx   M D mov   D push   Next   end-code

Code !  ( 16b addr -- )
   H pop   D pop   E M mov   H inx   D M mov   Next   end-code

Code 2@   ( addr -- 32b )   H pop   H push
   H inx  H inx   M E mov   H inx   M D mov   H pop   D push
   M E mov   H inx   M D mov  D push    Next   end-code

Code 2!   ( 32b addr -- )   H pop
   D pop    E M mov   H inx   D M mov   H inx
   D pop    E M mov   H inx   D M mov   Next   end-code


\ *** Block No. 13, Hexblock d

\ +! drop swap                                 11Jun86   18Nov87

Code +! ( 16b addr -- )  H pop
   Label +store   D pop
     M A mov   E add   A M mov   H inx
     M A mov   D adc   A M mov    Next   end-code

\  : +!   ( n addr -- )   under @ + swap ! ;


Code drop   ( 16b -- )   H pop   Next   end-code

Code swap   ( 16b1 16b2 -- 16b2 16b1 )
   H pop   xthl   hpush jmp   end-code



\ *** Block No. 14, Hexblock e

\ dup  ?dup                                              16May86

Code dup    ( 16b -- 16b 16b )
   H pop   H push   hpush jmp   end-code

Code ?dup ( 16b -- 16b 16b / false)
   H pop   H A mov   L ora   0<> ?[ H push ]?
   hpush jmp   end-code

\ \\
\ : ?dup ( 16b -- 16b 16b / false) dup IF dup THEN ;
\
\ : dup ( 16b -- 16b 16b )    sp@ @ ;




\ *** Block No. 15, Hexblock f

\ over rot nip under                                     11Jun86

Code over   ( 16b1 16b2 - 16b1 16b2 16b1 )
   D pop   H pop   H push   dpush jmp   end-code
Code rot    ( 16b1 16b2 16b3 - 16b2 16b3 16b1 )
   D pop   H pop   xthl   dpush jmp   end-code
Code nip ( 16b1 16b2 -- 16b2)
   H pop   D pop   hpush jmp   end-code
Code under ( 16b1 16b2 -- 16b2 16b1 16b2)
   H pop   D pop   H push  dpush jmp   end-code

\ \\
\ : over   >r swap r> swap ;
\ : rot   >r dup r> swap ;
\ : nip   swap drop ;
\ : under swap over ;

\ *** Block No. 16, Hexblock 10

\ -rot pick roll -roll                                   11Jun86
Code -rot    ( 16b1 16b2 16b3 -- 16b3 16b1 16b2 )
   H pop    D pop   xthl   H push   D push   Next   end-code

Code pick    ( n -- 16b.n )
   H pop   H dad   SP dad
   M E mov   H inx   M D mov   D push   Next   end-code

: roll   ( n -- )
   dup >r  pick sp@ dup 2+  r> 1+ 2* cmove> drop ;

: -roll   ( n -- ) >r dup sp@ dup 2+
   dup 2+ swap r@ 2* cmove r> 1+ 2* + ! ;
\ \\
\ : -rot    ( 16b1 16b2 16b3 -- 16b3 16b1 16b2 )   rot rot ;
\ : pick    ( n -- 16b.n )     1+ 2* sp@ + @ ;

\ *** Block No. 17, Hexblock 11

\ double word stack manipulation                         09May86
Code 2swap ( 32b1 32b2 -- 32b2 32b1)
   H pop   D pop   xthl   H push
   5 H lxi   SP dad   M A mov    D M mov   A D mov
   H dcx   M A mov   E M mov   A E mov   H pop   dpush jmp
end-code

Code 2drop ( 32b -- )   H pop   H pop   Next   end-code

Code 2dup ( 32b -- 32b 32b)
   H pop   D pop   D push   H push   dpush jmp   end-code

\ \\
\ : 2swap ( 32b1 32b2 -- 32b2 32b1) rot >r rot r> ;
\ : 2drop ( 32b -- ) drop drop ;
\ : 2dup ( 32b -- 32b 32b) over over ;

\ *** Block No. 18, Hexblock 12

\ + and or xor not                                       09May86
Code +     ( n1 n2 -- n3 )
   H pop   D pop   D dad   hpush jmp end-code
Code or    ( 16b1 16b2 -- 16b3 )
   H pop   D pop   H A mov  D ora  A H mov
   L A mov   E ora   A L mov   hpush jmp end-code
Code and   ( 16b1 16b2 -- 16b3 )
   H pop   D pop   H A mov   D ana   A H mov
   L A mov   E ana   A L mov   hpush jmp   end-code
Code xor   ( 16b1 16b2 -- 16b3 )
   H pop   D pop   H A mov   D xra   A H mov
   L A mov   E xra   A L mov   hpush jmp   end-code
Code not   ( 16b1 -- 16b2 )  H pop  Label >not
   H A mov   cma    A H mov   L A mov   cma   A L mov
   hpush jmp   end-code


\ *** Block No. 19, Hexblock 13

\ -  negate                                              16May86

Code -    ( n1 n2 -- n3 )
   D pop   H pop
   L A mov   E sub   A L mov
   H A mov   D sbb   A H mov   hpush jmp end-code

Code negate ( n1 -- n2 )
   H pop   H dcx   >not jmp   end-code

\ \\
\ : -    ( n1 n2 -- n3 )   negate + ;





\ *** Block No. 20, Hexblock 14

\ dnegate d+                                   10Mar86   18Nov87

Code dnegate   ( d1 -- -d1 )  H pop
   Label >dnegate
      D pop   A sub   E sub   A E mov   0 A mvi   D sbb
      A D mov   0 A mvi   L sbb   A L mov   0 A mvi   H sbb
      A H mov   dpush jmp   end-code

Code d+        ( d1 d2 -- d3)
   6 H lxi   SP dad   M E mov   C M mov   H inx
   M D mov   B M mov  B pop   H pop   D dad   xchg
   H pop   L A mov   C adc   A L mov   H A mov   B adc
   A H mov   B pop   dpush jmp   end-code




\ *** Block No. 21, Hexblock 15

\ 1+ 2+ 3+ 4+ 6+    1- 2- 4-                             27Apr86
Code 1+ ( n1 -- n2 )    H pop   H inx   hpush jmp   end-code
Code 2+ ( n1 -- n2 )
   H pop    H inx   H inx   hpush jmp   end-code
Code 3+ ( n1 -- n2 )
   H pop   H inx   H inx   H inx   hpush jmp   end-code
Code 4+ ( n1 -- n2 )
   H pop   4 D lxi   D dad   hpush jmp   end-code
| Code  6+ ( n1 -- n2 )
   H pop   6 D lxi   D dad   hpush jmp   end-code
Code 1- ( n1 -- n2 )    H pop   H dcx   hpush jmp   end-code
Code 2- ( n1 -- n2 )
   H pop    H dcx   H dcx   hpush jmp   end-code
Code 4- ( n1 -- n2 )
   H pop   -4 D lxi   D dad   hpush jmp   end-code


\ *** Block No. 22, Hexblock 16

\ number Constants                                       07Oct87
-1 Constant true      0 Constant false

 0 ( --  0 )   Constant   0
 1 ( --  1 )   Constant   1
 2 ( --  2 )   Constant   2
 3 ( --  3 )   Constant   3
 4 ( --  4 )   Constant   4
-1 ( -- -1 )   Constant  -1

Code on ( addr -- )  H pop   $FF A mvi
 Label set   A M mov   H inx   A M mov   Next
Code off ( addr -- )   H pop   A xra   set jmp   end-code

\  : on   ( addr -- )   true  swap ! ;
\ : off  ( addr -- )   false swap ! ;

\ *** Block No. 23, Hexblock 17

\ words for number literals                              16May86

Code lit   ( -- 16b )
   IP ldax   A L mov   IP inx   IP ldax   A H mov   IP inx
hpush jmp end-code

Code clit   ( -- 8b )
   IP ldax   A L mov   0 H mvi   IP inx   hpush jmp
end-code

: Literal  ( 16b -- )
   dup $FF00 and   IF  compile lit , exit  THEN
   compile clit c, ; immediate restrict




\ *** Block No. 24, Hexblock 18

\ comparision words                                      18Nov87
Label  (u<  ( HL,DE  ->  HL u< DE  c,z )
   H A mov   D cmp   rnz   L A mov   E cmp   ret
Label  (<   ( HL,DE  ->  HL  < DE  c,z )
   H A mov   D xra   (u< jp    D A mov   H cmp   ret

Label yes  true  H lxi  hpush jmp
Code u< ( u1 u2 -- flag )   D pop   H pop
   Label uless   (u< call   yes jc
   Label no   false H lxi   hpush jmp

Code <     ( n1 n2 -- flag )   D pop   H pop
   Label less    (< call  yes jc  no jmp   end-code

Code u> ( u1 u2 -- flag )  H pop   D pop  uless jmp   end-code
Code >  ( n1 n2 -- flag )  H pop   D pop   less jmp   end-code

\ *** Block No. 25, Hexblock 19

\ comparision words                                      18Nov87
Code 0< ( n1 n2 -- flag )   H pop
   Label negative   H dad   yes jc   no jmp   end-code

Code 0> ( n -- flag )   H pop   H A mov   A ora   no jm
                        L ora   yes jnz   no jmp   end-code

Code 0= ( n -- flag )   H pop
   Label zero=   H A mov   L ora   yes jz    no jmp   end-code

Code 0<>  ( n -- flag )
   H pop   H A mov   L ora   yes jnz   no jmp   end-code

Code =    ( n1 n2 -- flag )  H pop   D pop
   L A mov   E cmp   no jnz
   H A mov   D cmp   no jnz   yes jmp   end-code

\ *** Block No. 26, Hexblock 1a

\ \\ comparision words high level                          18Nov87
\ : 0<  ( n1 -- flag )       8000 and 0<> ;
\ : >   ( n1 n2 -- flag )    swap < ;
\ : 0>  ( n -- flag )        negate 0< ;
\ : 0<> ( n -- flag )        0= not ;
\ : u>  ( u1 u2 -- flag )    swap u< ;
\ : =   ( n1 n2 -- flag )    - 0= ;
\ : uwithin  ( u1 [low up[ -- flag )    over - -rot  - u> ;
\ | : minimax  ( n1 n2 flag -- n3 )   rdrop IF swap THEN drop ;
\ : min  ( n1 n2 -- n3 )              2dup  > minimax ;
\ : max  ( n1 n2 -- n3 )              2dup  < minimax ;
\ : umax  ( u1 u2 -- u3 )             2dup u< minimax ;
\ : umin  ( u1 u2 -- u3 )             2dup u> minimax ;
\ : extend   ( n -- d )               dup 0< ;
\ : dabs  ( d -- ud )                 extend IF dnegate THEN ;
\ : abs   ( n -- u)                   extend IF  negate THEN ;

\ *** Block No. 27, Hexblock 1b

\ uwthin double number comparison words                  18Nov87

Code uwithin ( u1 [low up[ -- flag )  H pop   D pop   xthl
   (u< call   cs ?[    H pop   no jmp   ]?
   D pop   (u< call   yes jc   no jmp   end-code

Code d0= ( d -- flag )  H pop
   H A mov   L ora   H pop   no jnz   zero= jmp  end-code

: d=  ( d1 d2 -- flag )    rot =  -rot =  and ;
: d<  ( d1 d2 -- flag )
    rot 2dup =  IF 2drop u< exit THEN  > nip nip ;


\ \\
\ : d0= ( d -- flag )        or 0= ;

\ *** Block No. 28, Hexblock 1c

\ minimum maximum                                        18Nov87

Code umax ( u1 u2 -- u3 )
   H pop   D pop   (u< call
Label minimax  cs ?[ xchg ]?  hpush jmp   end-code

Code umin ( u1 u2 -- u3 )
   H pop   D pop   (u< call   cmc   minimax jmp   end-code

Code max ( n1 n2 -- n3 )
   H pop   D pop   (< call   minimax jmp   end-code

Code min ( n1 n2 -- n3 )
   H pop   D pop   (< call   cmc   minimax   jmp end-code



\ *** Block No. 29, Hexblock 1d

\ sign extension absolute values                         18Nov87

Code extend ( n -- d )  H pop   H push   negative jmp  end-code

Code abs    ( a -- u )  H pop   H A mov  A ora
   hpush jp   H dcx  >not jmp  end-code

Code dabs   ( d -- ud )  H pop   H A mov   A ora
   hpush jp   >dnegate jmp   end-code








\ *** Block No. 30, Hexblock 1e

\ branch ?branch                                         20Nov87

Code branch ( -- )  Label >branch
   IP H mvx   M E mov   H inx   M D mov   H dcx
   D dad   H IP mvx   Next   end-code

Code ?branch ( fl -- )
   H pop   H A mov   L ora  >branch jz
   IP inx   IP inx  Next   end-code


\ \\
\ : branch r> dup @ + >r ;




\ *** Block No. 31, Hexblock 1f

\ loop primitives                              11Jun86   20Nov87

Code bounds ( start count -- limit start )
   H pop   D pop   D dad    H push   D push   Next   end-code

Code endloop
   RP lhld   6 D lxi   D dad   RP shld   next  end-code restrict

\ \\ dodo puts "index | limit | adr.of.DO" on return-stack
\ : bounds ( start count -- limit start )     over + swap ;
\
\ | : dodo              rdrop r> 2+ dup >r rot >r swap >r >r ;
\
\ : (do  ( limit start -- )  over - dodo ;  restrict
\ : (?do ( limit start -- )  over - ?dup IF dodo THEN
\                            r> dup  @ +  >r drop ; restrict

\ *** Block No. 32, Hexblock 20

\ loop primitives                                        20Nov87

Code (do ( limit start -- )  H pop  D pop
  Label >do
    L A mov   E sub   A L mov
    H A mov   D sbb   A H mov
    H push   IP inx   IP inx
    RP lhld   H dcx  IP M mov   H dcx   IP' M mov
              H dcx   D M mov   H dcx    E  M mov
    D pop     H dcx   D M mov   H dcx    E  M mov   RP shld
    Next   end-code   restrict

Code (?do ( limit start -- )  H pop  D pop
    H A mov   D cmp   >do jnz
    L A mov   E cmp   >do jnz   >branch jmp
end-code  restrict

\ *** Block No. 33, Hexblock 21

\ (loop (+loop                                 14May86   20Nov87

Code (loop
   RP lhld   M inr   0= ?[ H inx   M inr   >next jz   ]?
Label doloop   RP lhld    4 D lxi  D dad
               M IP' mov   H inx   M IP mov   Next
end-code restrict

Code (+loop
   RP lhld   D pop
   M A mov   E add   A M mov   H inx
   M A mov   D adc   A M mov
   rar   D xra   doloop jp   Next
end-code restrict



\ *** Block No. 34, Hexblock 22

\ loop indices                                 06May86   20Nov87

Code I ( -- n )
   RP lhld
Label >I     M E mov   H inx   M D mov   D push
   H inx     M E mov   H inx   M D mov   H pop   D dad
   hpush jmp
end-code

Code J ( -- n )
   RP lhld   6 D lxi  D dad     >I jmp   end-code






\ *** Block No. 35, Hexblock 23

\ interpretive conditionals                           UH 25Jan88

| Create: remove>>  r> rp! ;
| : >>r ( addr len -- addr ) r>  over rp@  under swap - dup rp!
     swap >r remove>> >r swap >r  dup >r swap cmove r> ;

| Variable saved-dp     0 saved-dp !

| Variable level    0 level !

| : +level ( -- ) level @ IF 1 level +! exit THEN state @ ?exit
      1 level !  here saved-dp ! ] ;

| : -level ( -- ) state @ 0= Abort" unstructured"
     level @ 0=exit -1 level +!  level @ ?exit  compile unnest
     [compile] [  saved-dp @  here over dp !  over -  >>r >r ;

\ *** Block No. 36, Hexblock 24

\ resolve loops and branches                          UH 25Jan88

: >mark     ( -- addr )           here 0 , ;

: +>mark    ( acf -- addr )       +level , >mark ;

: >resolve  ( addr -- )           here over - swap !  -level ;

: <mark     ( -- addr )           +level  here ;

: <resolve  ( addr -- )           here - , -level ;

: ?pairs    ( n1 n2 -- )          - Abort" unstructured" ;




\ *** Block No. 37, Hexblock 25

\ case?                                                  14May86

Code case? ( 16b1 16b2 -- 16b1 false / true )
   H pop   D pop
   H A mov   D cmp   0= ?[   L A mov   E cmp   yes jz ]?
   D push   no jmp   end-code

\ \\
\ : case? ( 16b1 16b2 -- 16b1 false / true )
\     over = dup  IF nip THEN ;







\ *** Block No. 38, Hexblock 26

\ Branching                                           UH 25Jan88

: IF             ['] ?branch +>mark 1 ; immediate
: THEN           abs 1 ?pairs >resolve ;   immediate
: ELSE           1 ?pairs  ['] branch +>mark swap
                 >resolve -1 ;             immediate
: BEGIN          <mark 2 ;                 immediate
: WHILE          2 ?pairs 2   ['] ?branch +>mark
                 -2 2swap ;                immediate

| : (reptil      <resolve
                 BEGIN dup -2 = WHILE drop >resolve REPEAT ;

: REPEAT         2 ?pairs compile  branch (reptil ; immediate
: UNTIL          2 ?pairs compile ?branch (reptil ; immediate


\ *** Block No. 39, Hexblock 27

\ Loops                                               UH 25Jan88

: DO        ['] (do  +>mark 3 ;       immediate
: ?DO       ['] (?do +>mark 3 ;       immediate
: LOOP      3 ?pairs compile  (loop compile endloop >resolve ;
                                         immediate
: +LOOP     3 ?pairs compile (+loop compile endloop >resolve ;
                                         immediate

Code LEAVE
   RP lhld   4 D lxi   D dad   M E mov   H inx   M D mov
   H inx   RP shld   xchg    H dcx   M D mov   H dcx   M E mov
   D dad   H IP mvx   Next   end-code   restrict

\ \\ Returnstack: calladr | index limit | adr of DO
\ : LEAVE     endloop r> 2- dup @ + >r ;             restrict

\ *** Block No. 40, Hexblock 28

\ um*                                                    16May86
Label (um*   0 H lxi   ( 0=Teil-Produkt )
             4 C mvi   ( Schleifen-Zaehler )
         [[  H dad ( Schiebe HL 24 bits nach links )
             ral cs ?[   D dad   0 aci   ]?
             H dad   ral   cs ?[   D dad   0 aci   ]?
      C dcr  0= ?]   ret

Code um*      ( u1 u2 -- ud )
   D pop      H pop     B push   H B mov   L A mov   (um* call
   H push   A H mov   B A mov    H B mov             (um* call
   D pop    D C mov     B dad      0 aci   L D mov    H L mov
   A H mov      B pop     dpush jmp end-code




\ *** Block No. 41, Hexblock 29

\ m* * 2* 2/                                             16May86

: m*  ( n1 n2 -- d )    dup 0< dup >r IF negate THEN
                        swap dup 0< IF negate r> not >r THEN
                        um* r> IF dnegate THEN ;

: *  ( n1 n2 - prod )   um* drop ;

Code 2*  ( n -- 2*n )   H pop   H dad   hpush jmp   end-code

Code 2/  ( n -- n/2 )
   H pop   H A mov   rlc   rrc   rar   A H mov
           L A mov   rar   A L mov   hpush jmp   end-code
\ \\
\ : 2*  ( n -- 2*n )   2 * ;
\ : 2/  ( n -- n/2 )   2 / ;

\ *** Block No. 42, Hexblock 2a

\ um/mod                                                 14May86
Label usl0
   A E mov   H A mov   C sub   A H mov   E A mov   B sbb
   cs ?[ H A mov   C add   A H mov   E A mov   D dcr rz
Label usla
         H dad   ral   usl0 jnc
         A E mov   H A mov   C sub   A H mov   E A mov   B sbb
      ]? L inr   D dcr   usla jnz   ret
Label usbad     -1 H lxi   B pop   H push   hpush jmp
Code um/mod   ( d1 n1 -- rem quot )
   IP H mvx     B pop   D pop   xthl   xchg
   L A mov   C sub   H A mov   B sbb   usbad jnc
   H A mov   L H mov   D L mov   8 D mvi   D push
   usla call   D pop   H push   E L mov   usla call
   A D mov   H E mov   B pop   C H mov   B pop
   D push   hpush jmp   end-code

\ *** Block No. 43, Hexblock 2b

\ m/mod                                                  16May86

: m/mod ( d n -- mod quot)
   dup >r  abs over 0< IF  under + swap  THEN
   um/mod  r@ 0< IF  negate over IF  swap r@ + swap 1-
   THEN THEN rdrop ;











\ *** Block No. 44, Hexblock 2c

\ /mod / mod */mod */ u/mod  ud/mod                      16May86

: /mod   ( n1 n2 -- rem quot )      >r extend r> m/mod ;

: /      ( n1 n2 --     quot )      /mod nip ;

: mod    ( n1 n2 -- rem )           /mod drop ;

: */mod  ( n1 n2 n3 -- rem quot )   >r m* r> m/mod ;

: */     ( n1 n2 n3 -- quot )       */mod nip ;

: u/mod  ( u1 u2 -- urem uquot )    0 swap um/mod ;

: ud/mod ( ud1 u2 -- urem udquot )  >r 0 r@ um/mod r> swap >r
                                    um/mod r> ;

\ *** Block No. 45, Hexblock 2d

\ cmove cmove>                                 16May86   18Nov87

Code cmove ( from to count -- )   IP H mvx   IPsave shld
     B pop   D pop   H pop
  Label (cmove
     [[ B A mov   C ora   0= not ?[[
        M A mov   H INX   D stax   D inx  B dcx
     ]]? IPsave lhld  H IP mvx   Next end-code

Code cmove>   ( from to count -- )   IP H mvx   IPsave shld
     B pop   D pop   H pop
  Label (cmove>
     B dad   H dcx   xchg   B dad   H dcx   xchg
     [[ B A mov   C ora   0= not ?[[
        M A mov   H dcx   D stax   D dcx   B dcx
     ]]? IPsave lhld  H IP mvx   Next end-code

\ *** Block No. 46, Hexblock 2e

\ move place count                             17Oct86   18Nov87

Code move  ( from to quan -- )
      IP H mvx  Ipsave shld   B pop   D pop   H pop
   Label domove  (u< call   (cmove jnc   (cmove> jmp   end-code

| Code (place ( addr len to -- len to )  IP H mvx  Ipsave shld
    D pop  B pop  H pop
    B push  D push   D inx   domove jmp   end-code

: place ( addr len to -- )  (place c! ;

Code count ( adr -- adr+1 len )   H pop   M E mov   0 D mvi
      H inx    H push   D push   Next  end-code



\ *** Block No. 47, Hexblock 2f

\ fill erase                                             18Nov87

Code fill ( addr quan 8b -- )
   IP H mvx   IPsave shld   D pop   B pop   H pop
   [[ B A mov   C ora   0<> ?[[
      E M mov   H inx   B dcx
   ]]?  IPsave lhld   H IP mvx   Next   end-code

: erase   ( addr quan --)            0 fill ;

\ \\ : fill ( addr quan 8b -- )
\    swap ?dup IF >r over c! dup 1+ r> 1- cmove exit THEN 2drop ;
\ : count ( adr -- adr+1 len )  dup 1+ swap c@ ;
\ : move   ( from to quan -- )
\    >r  2dup u< IF  r> cmove> exit  THEN  r> cmove ;
\ : place  ( addr len to --)  over >r  rot over 1+  r> move c! ;

\ *** Block No. 48, Hexblock 30

\ here allot , c, pad compile                  11Jun86   18Nov87

Code here ( -- addr )  user' dp  D lxi
   UP lhld   D dad   fetch jmp   end-code

Code allot ( n -- )    user' dp  D lxi
   UP lhld   D dad   +store jmp  end-code

: ,      ( 16b -- )  here  ! 2 allot ;
: c,     ( 8b -- )   here c! 1 allot ;

: pad    ( -- addr ) here $42 + ;
: compile            r> dup 2+ >r @ , ; restrict

\ : here   ( -- addr ) dp @ ;
\ : allot  ( n -- )    dp +! ;

\ *** Block No. 49, Hexblock 31

\ input strings                                          11Jun86

$84 Constant /tib
Variable #tib     0 #tib !
Variable >tib     here >tib ! /tib allot
Variable >in      0 >in !
Variable blk      0 blk !
Variable span     0 span !

: tib ( -- addr )  >tib @ ;

: query ( -- )  tib $50 expect span @ #tib !  >in off blk off ;






\ *** Block No. 50, Hexblock 32

\ \\ scan skip /string                           16May86   18Nov87
\
\ : scan ( addr0 len0 char -- addr1 len1 ) >r
\    BEGIN dup WHILE  over c@ r@ -  WHILE  1- swap 1+ swap REPEAT
\    rdrop ;
\
\ : skip ( addr len del -- addr1 len1 ) >r
\    BEGIN dup WHILE  over c@ r@ =  WHILE  1- swap 1+ swap REPEAT
\    rdrop ;
\
\ : /string ( addr0 len0 +n - addr1 len1 )
\    over umin rot over + -rot - ;





\ *** Block No. 51, Hexblock 33

\ skip scan                                              18Nov87
Label done    H push   B push   IPsave lhld   H IP mvx   Next
Code skip   ( addr len del -- addr1 len1 )
   IP H mvx   IPsave shld   D pop   B pop   H pop
   [[ B A mov   C ora   done jz
      M A mov   E cmp   done jnz   H inx   B dcx  ]] end-code

Code scan   ( addr len chr -- addr1 len1 )
   IP H mvx   IPsave shld   D pop   B pop   H pop
   [[ B A mov   C ora   done jz
      M A mov   E cmp   done jz   H inx   B dcx  ]] end-code

Code /string ( addr0 len0 +n - addr1 len1 )   H pop   D pop
   D push  (u< call  cs ?[ xchg ]?   H pop   xthl   D dad   xthl
   L A mov   E sub   A L mov   H A mov   D sbb   A H mov
   Hpush jmp   end-code

\ *** Block No. 52, Hexblock 34

\ capitalize    ohne Umlaute !!                16May86UH 25Jan88
Variable caps  0 caps !
Label ?capital   caps lda   A ana   rz
Label (capital ( e --> A,E )   E A mov  Ascii a cpi   rc
    Ascii z 1+ cpi   rnc   Ascii a Ascii A - sui  A E mov  ret

Code capital ( char -- char')  D pop
   (capital call    D push   Next  end-code
Code upper ( addr len -- )   D pop   E D mov   H pop   D inr
   [[ D dcr >next jz  M E mov  (capital call  E M mov  H inx ]]
end-code

\ \\ : capital ( char -- char')
\    dup  Ascii a   [ Ascii z 1+ ] Literal  uwithin not ?exit
\    [ Ascii a Ascii A - ] Literal - ;
\ : upper ( addr len -- )  bounds   ?DO I c@ capital I c! LOOP ;

\ *** Block No. 53, Hexblock 35

\ (word                                                  16May86

Code (word ( char adr0 len0 -- addr )
   IP H mvx   IPsave shld   B pop   B dcx  D pop
   >in lhld   D dad   xchg   xthl   xchg   H push   >in lhld
   C A mov   L sub   A L mov   B A mov   H sbb   A H mov
   cs ?[ B inx   C A mov   >in sta   B A mov   >in 1+ sta
                 D pop   H pop   D push
   ][ H inx   H B mvx   H pop
      [[  B A mov   C ora   0<>
      ?[[  M A mov   E cmp   0=   ?[[  H inx  B dcx ]]? ]?
      H push
      [[  B A mov  C ora   0<>
      ?[[  M A mov   E cmp   0<>   ?[[ H inx  B dcx ]]? ]?
      xchg    H pop   xthl
      E A mov   L sub   A L mov   D A mov   H sbb   A H mov

\ *** Block No. 54, Hexblock 36

\ (word Part2                                            16May86

      B A mov  C ora   0<> ?[ H inx ]?   >in shld   ]?
   H pop   E A mov   L sub   A C mov   D A mov   H sbb   A B mov
   H push   user' dp D lxi   UP lhld   D dad
   M A mov   H inx   M H mov  A L mov   D pop   H push
   C M mov   H inx
   [[ B A mov  C ora 0<>
   ?[[ D ldax  A M mov   H inx  D inx  B dcx ]]?   bl M mvi
   IPsave lhld   H IP mvx   Next   end-code
\ \\
\ : (word  ( char adr0 len0 -- addr )
\    rot  >r  over swap   >in @ /string
\    r@ skip   over swap   r> scan >r   rot over swap - r> 0<> -
\    >in !   over - here  dup >r  place  bl r@ count  + c!   r> ;


\ *** Block No. 55, Hexblock 37

\ source word parse name                       20Oct86UH 25Jan88

defer source

: (source   ( -- addr len)   tib #tib @  ;

' (source IS source

: word ( char -- addr )   source (word ;

: parse ( char -- addr len )
   >r  source  >in @ /string  over  swap r>  scan >r
   over - dup r>  0<> -  >in +! ;

: name ( -- addr )   bl word  dup count upper  exit ;



\ *** Block No. 56, Hexblock 38

\ state Ascii ," "lit ("  "                              18Nov87

Variable state   0 state !

: Ascii  ( char -- n )
   bl word  1+ c@  state @ IF [compile] Literal THEN ; immediate

Code "lit   RP lhld   M E mov   H inx   M D mov   H dcx
   D push   D ldax   D inx   E add   A M mov   H inx
   D A mov   0 aci   A M mov   Next  end-code

: ,"    Ascii " parse  here over 1+ allot place ;
: ("    "lit ; restrict
: "     compile (" ," align ; immediate restrict

\  : "lit  r> r> under  count + even  >r >r ;   restrict

\ *** Block No. 57, Hexblock 39

\ ." ( .( \ \\ hex decimal                               07Oct87

: (."      "lit count type ; restrict
: ."       compile (." ," align ; immediate restrict

: (        ascii ) parse 2drop ; immediate
: .(       ascii ) parse type ; immediate

: \        blk @ IF >in @ negate  c/l mod  >in +!
                 ELSE #tib @ >in ! THEN ; immediate

: \\       b/blk >in ! ; immediate
: \needs   name find nip 0=exit [compile] \ ;

: hex      $10 base ! ;
: decimal  $0A base ! ;



\ *** Block No. 58, Hexblock 3a

\ number conversion: digit?                    16May86   18Nov87

Code digit?    ( char -- n true : false )
   user' base D lxi   UP lhld   D dad
   D pop   E A mov   Ascii 0 sui   no jc
   $0A cpi   cs not ?[ Ascii A Ascii 0 - cpi   no jc
                      Ascii A Ascii 9 - 1- sui  ]?
   M cmp   no jnc
   0 H mvi   A L mov   H push   yes jmp   end-code

\ \\
\ : digit? ( char -- digit true/ false )  dup Ascii 9 >
\    IF [ Ascii A Ascii 9 - 1- ] Literal - dup Ascii 9 > and THEN
\    Ascii 0 - dup base @ u< dup ?exit nip ;



\ *** Block No. 59, Hexblock 3b

\ number conversion:  accumulate  convert                11Jun86

| : end?   ( -- flag )                   >in @ 0= ;
| : char   ( addr0 -- addr1 char )       count -1 >in +! ;
| : previous   ( addr0 -- addr0 char )   1- count ;

: accumulate ( +d0 adr digit - +d1 adr )
   swap >r swap  base @  um* drop rot  base @  um* d+ r> ;

: convert ( +d1 addr0 -- +d2 addr2 )
   1+ BEGIN count digit? WHILE accumulate REPEAT 1- ;






\ *** Block No. 60, Hexblock 3c

\ number conversion: ?nonum punctuation?                 07Oct87

| : ?nonum    ( flag -- exit if true ) 0=exit
      rdrop 2drop drop rdrop false ;

| : punctuation?   ( char -- flag )
     Ascii , over =  swap  Ascii . =  or ;










\ *** Block No. 61, Hexblock 3d

\ number conversion: fixbase?                            07Oct87

| : fixbase?  ( char - char false / newbase true )  capital
   Ascii & case? IF $0A true exit  THEN
   Ascii $ case? IF $10 true exit  THEN
   Ascii H case? IF $10 true exit  THEN
   Ascii % case? IF   2 true exit  THEN     false ;










\ *** Block No. 62, Hexblock 3e

\ number conversion: ?num ?dpl                           07Oct87

Variable dpl      -1 dpl !

| : ?num      ( flag -- exit if true )  0=exit
      rdrop drop r> IF dnegate THEN
      rot drop dpl @ 1+ ?dup ?exit  drop true ;

| : ?dpl     dpl @  -1 =  ?exit  1 dpl +! ;








\ *** Block No. 63, Hexblock 3f

\ number conversion: number?  number                     11Jun86

: number?   ( string - string false / n 0< / d 0> )
   base push  >in push  dup count >in !  dpl on
   0 >r ( +sign) 0.0 rot end? ?nonum char
   Ascii - case?  IF  rdrop true >r end? ?nonum char  THEN
   fixbase?       IF  base !        end? ?nonum char  THEN
   BEGIN digit? 0= ?nonum
     BEGIN accumulate ?dpl end? ?num char digit? 0=  UNTIL
     previous  punctuation?  0= ?nonum  dpl off  end? ?num  char
   REPEAT ;

: number ( string -- d )
   number? ?dup 0= Abort" ?"  0< IF extend THEN ;



\ *** Block No. 64, Hexblock 40

\ hide reveal immediate restrict                         11Jun86

Variable last     0 last !
| : last?   ( -- false / acf true)    last @ ?dup ;
: hide          last?  IF  2- @ current @ !  THEN ;
: reveal        last?  IF  2-   current @ !  THEN ;
: Recursive     reveal ; immediate restrict

| : flag!    ( 8b --)
      last?  IF  under c@ or over c!  THEN   drop  ;

: immediate     $40 flag! ;
: restrict      $80 flag! ;




\ *** Block No. 65, Hexblock 41

\ clearstack hallot heap heap?                           04Sep86

Code clearstack
   user' s0 D lxi   UP lhld   D dad   M E mov   H inx   M D mov
   xchg   sphl   Next   end-code

: hallot ( quan -- )
   s0 @  over -  swap    sp@ 2+  dup rot -   dup s0 !
   2 pick  over -    di  move  clearstack  ei   s0 ! ;

: heap    ( -- addr )        s0 @ 6 + ;
: heap?   ( addr -- flag )   heap up@ uwithin ;

| : heapmove   ( from -- from )
     dup  here over -  dup hallot
     heap swap cmove   heap over - last +!  reveal ;

\ *** Block No. 66, Hexblock 42

\ Does>  ;                                     11Jun86   20Nov87

Label (dodoes>
   IP rpush   IP pop   W inx   W push   Next   end-code

: (;code          r> last @ name> ! ;

: Does>
   compile (;code      $CD ( 8080-Call ) c,
   compile (dodoes> ; immediate restrict







\ *** Block No. 67, Hexblock 43

\ ?head  |  alignments                         20Oct86   18Nov87

Variable ?head     0 ?head !

: |                ?head @  ?exit  -1 ?head ! ;

\ machen nichts beim 8080:
: even   ( addr -- addr1 ) ; immediate
: align  ( -- )            ; immediate
: halign ( -- )            ; immediate

Variable warning    0 warning !

| : exists?         warning @  ?exit  last @ current @
   (find nip 0=exit  space last @ .name ." exists " ?cr ;


\ *** Block No. 68, Hexblock 44

\ warning   Create                             20Oct86   18Nov87

Defer makeview          ' 0 Is makeview

: (create ( string -- ) align here
    swap count $1F and here 4+ place makeview ,  current @ @ ,
    here last !  here c@ 1+ allot  align  exists?
    ?head @ IF  1 ?head +!   dup ,    \ Pointer to Code
                halign  heapmove  $20 flag!  dup dp !
            THEN drop  reveal 0 ,
    ;Code   W inx   W push   Next   end-code

: Create   name count 1 $20 uwithin not
           Abort" invalid name" 1- (create ;



\ *** Block No. 69, Hexblock 45

\ nfa?                                                   30Jun86

Code nfa? ( thread cfa -- nfa / false )
   D pop   H pop
   [[ M A mov   H inx   M H mov   A L mov
     H ora   Hpush jz   H push   H inx   H inx   H push   D push
     M A mov   H inx   $1F ani   A E mov   0 D mvi  D dad
     D pop xthl  M A mov   H pop   $20 ani
     0<> ?[ M A mov   H inx   M H mov   A L mov ]?
     H A mov   D cmp   0= ?[ L A mov  E cmp ]?
     H pop   0= ?]   H inx  H inx  Hpush jmp
end-code
\ \\
\ : nfa?    ( thread cfa -- nfa / false)
\    >r  BEGIN @ dup 0= IF  rdrop exit  THEN  dup 2+ name> r@ =
\        UNTIL 2+ rdrop ;

\ *** Block No. 70, Hexblock 46

\ >name name> >body .name                      30Jun86   07Oct87

: >name   ( cfa -- nfa / false )        voc-link
   BEGIN @ dup WHILE  2dup 4 - swap nfa?
      ?dup IF  -rot 2drop exit  THEN   REPEAT nip ;

Code (name>  ( nfa -- cfa )  H pop   M A mov   H inx   $1F ani
   A E mov   0 D mvi   D dad   hpush jmp   end-code
\ : (name>   ( nfa -- cfa )   count  $1F and + ;

: name> ( nfa -- cfa )  dup (name> swap  c@ $20 and  IF @ THEN ;

: >body ( cfa -- pfa )  2+ ;    : body> ( pfa -- cfa )  2- ;

: .name   ( nfa -- ) ?dup  IF  dup heap?  IF ." |" THEN
    count $1F and type  ELSE ." ???"  THEN space ;

\ *** Block No. 71, Hexblock 47

\ : ; Constant Variable                                  07Nov87

: Create:  Create  hide  current @ context !  0 ] ;

: : Create: ;Code  IP rpush   W inx   W IP mvx   Next end-code

: ;        0 ?pairs   compile unnest   [compile] [   reveal ;
           immediate restrict

: Constant ( n -- )   Create , ;Code
   W inx   xchg   M E mov   H inx   M D mov   D push   Next
   end-code

: Variable         Create 0 , ;



\ *** Block No. 72, Hexblock 48

\ uallot User Alias Defer                      11Jun86   18Nov87
: uallot   ( quan -- offset )   even  dup  udp @ +
    $FF u> Abort" Userarea full" udp @ swap udp +! ;

: User   Create 2 uallot c,
   ;Code W inx   W ldax   A E mov   0 D mvi
         UP lhld   D dad   hpush jmp   end-code

: Alias ( cfa -- )   Create  last  @ dup c@ $20 and
   IF  -2 allot  ELSE  $20 flag!  THEN (name> ! ;

| : crash           true Abort" crash" ;

: Defer   Create ['] crash ,
    ;Code W inx   xchg   M E mov   H inx   M D mov
          xchg   >next1 jmp   end-code

\ *** Block No. 73, Hexblock 49

\ vp current context also toss                           11Jun86

Create vp  $10 allot             Variable current

: context   ( -- adr )              vp dup @ + 2+ ;

| : thru.vocstack ( -- from to )    vp 2+ context ;
\ "Only Forth also Assembler" gives
\ vp:  countword = 6 | Only | Forth | Assembler |

: also          vp @ $0A > Error" Vocabulary stack full"
                context @  2 vp +! context ! ;
: toss          vp @ IF  -2 vp +!  THEN  ;




\ *** Block No. 74, Hexblock 4a

\ Vocabulary Forth Only Onlyforth              24Nov85   18Nov87

: Vocabulary
Create   0 , 0 ,  here  voc-link @ ,  voc-link !
 Does>   context ! ;
\  | Name | Code | Thread | Coldthread | Voc-link |

Vocabulary Forth
Vocabulary Root

: Only   vp off   Root also ;

: Onlyforth Only Forth also definitions ;




\ *** Block No. 75, Hexblock 4b

\ definitions order words                      10Oct87   20Nov87

| : init-vocabularys        voc-link @
     BEGIN  dup 2- @ over 4- ! @ ?dup 0= UNTIL ;

: definitions            context @ current ! ;

| : .voc   ( adr -- )    @ 2- >name .name ;

: order    vp 4+ context DO I .voc -2 +LOOP
           2 spaces current .voc ;

: words          context @
   BEGIN   @ dup stop? 0= and
   WHILE   ?cr dup 2+ .name space
   REPEAT  drop ;

\ *** Block No. 76, Hexblock 4c

\ found -text                                            11Jun86
| : found ( nfa -- cfa n )
   dup c@ >r   (name> r@ $20 and  IF  @       THEN
                   -1 r@ $80 and  IF  1-      THEN
                      r> $40 and  IF  negate  THEN  ;

\ \\
\ : -text ( adr1 u adr2 -- false:gleich/+1:str1>str2/-1:str1<str2)
\     over bounds DO   drop 1+ dup 1- c@ I c@ - dup
\                    IF dup abs / LEAVE THEN LOOP nip ;
\ | Variable string        | Variable strlen
\ : (find    ( string thread -- str false/NFA true )
\    >r count $1F and  strlen ! string !
\    BEGIN r> ?dup WHILE dup @ >r 2+ dup c@ $1F and strlen @ =
\     IF dup 1+ strlen @ string @ -text 0= ?dup IF rdrop exit THEN
\       THEN drop   REPEAT  string @ 1- false ;

\ *** Block No. 77, Hexblock 4d

\ (find                                                  11Jun86

Code (find   ( str thr - str false/ NFA true )
   H pop   D pop   IP push   D ldax   $1F ani   A C mov   D inx
Label findloop
   M A mov   H inx   M H mov  A L mov
   H A mov   L ora   0= ?[  IP pop  D dcx   D push   no jmp ]?
   H push   H inx   H inx   M A mov   $1F ani   C cmp
   0<>   ?[   H pop   findloop jmp   ]?
   D push   H inx   C B mov   B inr
   [[ B dcr   0<>  ?[[
      D ldax   M cmp   0<>   ?[ D pop  H pop   findloop jmp ]?
      H inx   D inx   ]]?
   D pop   H pop   H inx   H inx   IP pop   H push   yes jmp
end-code
\ \\  HL: thread, nfa   DE: string   C: strlen   B: counter

\ *** Block No. 78, Hexblock 4e

\ find  '  [compile]  [']  nullstring?                   18Nov87

: find    ( string -- cfa n / string false )
   context dup @ over  2- @ = IF  2-  THEN
   BEGIN  under @ (find  IF  nip found  exit  THEN
     over vp 2+  u> WHILE  swap 2-  REPEAT  nip false ;

: '    ( -- cfa )      name find ?exit Error" ?" ;

: [compile]            ' , ;                 immediate restrict

: [']                  ' [compile] Literal ; immediate restrict

: nullstring?   ( string -- string false / true )
   dup c@  0= dup 0=exit  nip ;


\ *** Block No. 79, Hexblock 4f

\ notfound                                     17Oct86UH 25Jan88

: no.extensions  ( string -- )
    state @ IF Abort" ?" THEN  Error" ?" ;

Defer notfound                  ' no.extensions Is notfound











\ *** Block No. 80, Hexblock 50

\ interpret interpreter compiler parser               UH 25Jan88
Defer parser

: interpret ( -- )
    BEGIN  ?stack  name  nullstring? ?exit  parser REPEAT ;

| : interpreter ( str -- ) find ?dup
     IF  1 and  IF execute  exit THEN Error" compile only" THEN
     number? ?exit   notfound ;

' interpreter Is parser

| : compiler ( str -- ) find  ?dup
     IF  0> IF  execute exit THEN , exit THEN
     number? ?dup IF 0> IF swap [compile] Literal THEN
                     [compile] Literal exit THEN  notfound ;

\ *** Block No. 81, Hexblock 51

\ [ ]                                                 UH 25Jan88

: [      ['] interpreter Is Parser  state off ; immediate

: ]      ['] compiler    Is Parser  state on ;












\ *** Block No. 82, Hexblock 52

\  Is                                          09May86UH 25Jan88

: (is      r> dup 2+ >r @ ! ;

| : def?  ( cfa -- )
     @  [ ' notfound @ ] Literal  - Abort" not deferred" ;

: Is   ( adr -- )     ' dup def? >body
   state @ IF  compile (is , exit  THEN  ! ; immediate








\ *** Block No. 83, Hexblock 53

$53 .pagestatus

\ ?stack                                                 30Jun86
| : stackfull ( -- )     depth $20 > Abort" tight stack"
    reveal last? IF dup heap? IF name> ELSE 4- THEN (forget THEN
    true Abort" Dictionary full" ;

Code ?stack
   UP lhld   user' dp D lxi   D dad   M E mov   H inx   M D mov
   0 H lxi  SP dad   L A mov   E sub   H A mov   D sbb
   0= ?[ ;c: stackfull ; Assembler ]?    H push
   UP lhld   user' s0 D lxi   D dad   M E mov   H inx   M D mov
   H pop    D A mov   H cmp  c0= ?[ 0= ?[ E A mov  L cmp ]? ]?
   >next jnc  ;c: true abort" Stack empty" ;
\ \\
\ : ?stack     sp@ here - 100 u< IF stackfull THEN
\                 sp@ s0 @ u> Abort" Stack empty" ;

