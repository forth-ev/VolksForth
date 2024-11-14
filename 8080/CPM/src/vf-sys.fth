\ *** Block No. 104, Hexblock 68

$68 .pagestatus

Target

\ endpoints of forget                                    01Jul86

| : |? ( nfa -- flag )   c@ $20 and ;
| : forget? ( adr nfa -- flag )   \ code in heap or above adr ?
     name>  under  1+ u<  swap  heap?  or ;

| : endpoints ( addr -- addr symb )
     heap voc-link @ >r
     BEGIN   r> @ ?dup      \ through all Vocabs
     WHILE dup >r 4- >r    \ link on returnstack
       BEGIN r> @ >r over 1- dup r@ u<      \ until link or
                   swap r@ 2+ name> u< and  \ code under adr
       WHILE r@ heap? [ 2dup ] UNTIL   \ search for name in heap
       r@ 2+ |? IF over r@ 2+ forget?
               IF r@ 2+ (name> 2+ umax THEN   \ then update symb
       THEN REPEAT rdrop  REPEAT ;

\ *** Block No. 105, Hexblock 69

\ remove, -words, -tasks                                 20Oct86

: remove ( dic sym thread - dic sym )
     BEGIN dup @ ?dup      \ unlink forg. words
     WHILE dup heap?
       IF  2 pick over u>  ELSE  3 pick over 1+ u<  THEN
       IF  @ over ! ( unlink word)  ELSE nip THEN  REPEAT drop ;

| : remove-words ( dic sym -- dic sym )
     voc-link BEGIN  @ ?dup
              WHILE  dup >r  4- remove  r> REPEAT ;

| : remove-tasks ( dic -- )       up@
     BEGIN  2+  dup @  up@ -  WHILE  2dup @ swap here uwithin
            IF dup @ 2+ @  over ! 2-
            ELSE @ THEN REPEAT 2drop ;

\ *** Block No. 106, Hexblock 6a

\ remove-vocs trim                             20Oct86   07Oct87

| : remove-vocs ( dic symb -- dic symb )
     voc-link remove       thru.vocstack
     DO 2dup I @ -rot uwithin
       IF  [ ' Forth 2+ ]  Literal I !  THEN  -2 +LOOP
     2dup  current @  -rot  uwithin
     IF  [ ' Forth 2+ ] Literal current ! THEN ;

Defer custom-remove     ' noop Is custom-remove

| : trim ( dic symb -- )
     over  remove-tasks remove-vocs remove-words
     custom-remove  heap swap - hallot dp ! 0 last ! ;



\ *** Block No. 107, Hexblock 6b

\ deleting words from dict.                    01Jul86   18Nov87

: clear        here  dup up@  trim  dp ! ;

: (forget ( adr --)    dup heap? Abort" is symbol"
                       endpoints  trim ;

: forget   ' dup [ dp ] Literal @  u< Abort" protected"
            >name  dup  heap?
            IF name> ELSE 4- THEN (forget ;

: empty   [ dp ] Literal @ up@ trim
          [ udp ] Literal @ udp ! ;




\ *** Block No. 108, Hexblock 6c

\ save bye stop? ?cr                                     18Nov87

: save    here  up@ trim
   voc-link @ BEGIN  dup 4- @ over 2- ! @ ?dup  0= UNTIL
   up@ origin $100 cmove ;

: bye       save-buffers  (bye ;
\ : bye       flush empty (bye ;

| : end?    key #cr = IF true rdrop THEN ;

: stop? ( -- flag )     key? IF end? end? THEN false ;

: ?cr                   col c/l u> 0=exit cr ;




\ *** Block No. 109, Hexblock 6d

\ in/output structure                                    07Jun86

| : Out:   Create dup c, 2+ Does> c@ output @ + perform ;

: Output:  Create:  Does> output ! ;
0   Out: emit   Out: cr   Out: type   Out: del
    Out: page   Out: at   Out: at?    drop

: row ( -- row)     at? drop ;
: col ( -- col)     at? nip ;

| : In:    Create dup c, 2+ Does> c@ input @ + perform ;

: Input:   Create:  Does> input ! ;
0   In: key   In: key?   In: decode   In: expect  drop


\ *** Block No. 110, Hexblock 6e

\ Alias  only definitionen                               18Nov87

Root definitions Forth

: seal  [ ' Root >body ] Literal off ; \ "erase" Root Vocab.

' Only        Alias Only
' Forth       Alias Forth
' words       Alias words
' also        Alias also
' definitions Alias definitions

Host Target




\ *** Block No. 111, Hexblock 6f

\ 'restart 'cold                               22Oct86   10Oct87

Defer 'restart  ' noop Is 'restart

| : (restart   ['] (quit Is 'quit drvinit 'restart
     [ errorhandler ] Literal @ errorhandler !
     ['] noop Is 'abort clearstack
     standardi/o interpret quit ;

Defer 'cold    ' noop Is 'cold

| : (cold      origin up@ $100 cmove  $80 count
     $50 umin >r tib r@ move r> #tib ! >in off  blk off
     init-vocabularys init-buffers  'cold
     Onlyforth page &24 spaces  logo count type cr (restart  ;


\ *** Block No. 112, Hexblock 70

\ cold bootsystem                                        20Oct86

Code cold   here >cold !
   s0 lhld   6 D lxi   D dad   origin D lxi   $3F C mvi
   [[ D ldax   A M mov  H inx   D inx   C dcr   0= ?]
   ' (cold >body IP lxi
Label bootsystem
   s0 lhld   6 D lxi   D dad   UP shld
   user' s0  D lxi   D dad
   M E mov   H inx   M D mov   xchg   sphl
   user' r0 D lxi    UP lhld    D dad
   M E mov   H inx   M D mov    xchg   RP shld
   $C3 ( jmp ) A mvi  $30 sta   wake H lxi  $31 shld ( Tasker )
   Next
end-code


\ *** Block No. 113, Hexblock 71

\ restart boot                                           20Oct86

Code restart      here >restart !
   ' (restart >body IP lxi   bootsystem jmp   end-code

Label boot     here >boot !    \ find link to Main:
   s0 lhld   6 D lxi   D dad   H B mvx    origin D lxi
   [[ [[ xchg   H inx   H inx   M E mov   H inx   M D mov
         D A mov   B cmp 0= ?]   E A mov   C cmp 0= ?]   H B mvx
   6 lhld   0 L mvi   ' limit >body shld
  -$1100 D lxi   D dad   r0 shld   \ set initial RP
   -$400 D lxi   D dad   s0 shld   \ set initial SP
       6 D lxi   D dad   xchg   B H mvx
       D M mov   H dcx   E M mov   \ set link to Maintask
  >cold 2- jmp
end-code

\ *** Block No. 114, Hexblock 72

$72 .pagestatus

\ "search                                                05Mar88

Label notfound      H pop   H pop
   IPsave lhld   H IP mvx   False H lxi   hpush jmp

Code "search ( text tlen buf blen -- addr tf / ff )
   IP H mvx  IPsave shld   D pop    H pop    xthl
   H A mov   L ora  notfound jz
   E A mov  L sub  A C mov   D A mov  H sbb  A B mov notfound jc
   B inx   D pop   xthl   M A mov  xthl  H push   xchg
Label scanfirst
   A E mov  ?capital call  E D mov
   [[  M E mov   H inx   B A mov  C ora   notfound jz   B dcx
       ?capital call  E A mov  D cmp  0=  ?]
   B D mvx   B pop   xchg   xthl   xchg   H push  B push  D push


\ *** Block No. 115, Hexblock 73

\ "search  part 2                                        27Nov87

Label match
   B dcx   B A mov  C ora  0<> ?[
   D inx   D ldax   D push   A E mov  ?capital call   E D mov
   M E mov  H inx  ?capital call  E A mov   D cmp   D pop
   match jz   H pop   B pop   D pop
   M A mov   xthl   B push   H B mvx  xchg  scanfirst jmp ]?
   D pop   D pop   H pop   D pop  H dcx  H push
   IPsave lhld  H IP mvx  True H lxi  hpush jmp
end-code
