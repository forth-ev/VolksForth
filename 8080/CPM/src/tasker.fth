
\ *** Block No. 0, Hexblock 0

\\ Multitasker                                           11Nov86

Dieses File enthaelt den Multitasker des volksFORTHs.
Er ist ein Round-Robin-Multitasker, d.h. jede Task behaelt
die Kontrolle ueber den Prozessor solange, bis sie sie
ausdruecklich abgibt.
Hintergrundtasks im volksFORTH koennen durch Semaphore geordnet
auf den Massenspeicher und auf den Drucker zugreifen.

In Verbindung mit dem Printer-Interface ist es moeglich
Files im Hintergrund auszudrucken. (SPOOL)






\ *** Block No. 1, Hexblock 1

\ Multitasker Loadscreen                       27Jun86   20Nov87

Onlyforth

\needs multitask   1 +load

02 05 +thru    \ Tasker










\ *** Block No. 2, Hexblock 2

\ stop singletask multitask                   28Aug86    20Nov87

Code stop   UP lhld   0 ( nop )  M mvi
Label taskpause
   IP push   RP lhld   H push   UP lhld   6 D lxi   D dad   xchg
   H L mov   SP dad   xchg   E M mov   H inx   D M mov
   UP lhld   H inx   pchl
end-code

: singletask  [ ' pause @ ] Literal  ['] pause ! ;

: multitask   [ taskpause ] Literal  ['] pause ! ;





\ *** Block No. 3, Hexblock 3

\ pass activate                                          28Aug86

: pass   ( n0 ... nr-1 Taddr r -- )
 BEGIN  [ rot ( Trick !! ) ]
  swap $F7 over c!        \ awake Task  ( rst 6 )
  r> -rot                 \ Stack:  IP r addr
  8 + >r                  \ s0 of Task
  r@ 2+ @ swap            \ Stack:  IP r0 r
  2+ 2*                   \ bytes on Taskstack incl. r0 & IP
  r@ @ over -             \ new SP
  dup r> 2- !             \ into Ssave
  swap bounds  ?DO  I !  2 +LOOP  ;  restrict

: activate   ( Taddr -- )
 0 [ -rot  ( Trick !! ) ]  REPEAT ; restrict


\ *** Block No. 4, Hexblock 4

\ sleep wake taskerror                         28Aug86   20Nov87

: sleep   ( Taddr -- )    $00 ( nop   ) swap c! ;
: wake    ( Taddr -- )    $F7 ( rst 6 ) swap c! ;

| : taskerror   ( string -- )
      standardi/o  singletask  ." Task error : " count type
      multitask stop ;









\ *** Block No. 5, Hexblock 5

\ Task                                                   20Nov87

: Task   ( rlen slen -- )
   0 Constant   here 2- >r    \ addr of task constant
   here  -rot                 \ here for Task dp
   even allot even            \ allot dictionary area
   here  r@ !                 \ set task constant addr
   up@ here $100 cmove        \ init user area
   here dup    $C300 ,        \ nop-jmp opcode to sleep task
   up@ 2+ dup @ ,  !          \ link task
   r> ,                       \ spare used for pointer to header
   dup  6 -   dup , ,         \ ssave and s0
   2dup + ,                   \ here + rlen = r0
   rot ,                      \ dp
   under  +  dp ! 0 ,         \ allot rstack
   ['] taskerror  [ ' errorhandler >body c@ ] Literal rot + ! ;

\ *** Block No. 6, Hexblock 6

\ rendezvous 's tasks                          27Jun86   20Nov87

: rendezvous   ( semaphoraddr -- ) dup unlock  pause  lock ;

| : statesmart       state @ IF  [compile] Literal  THEN ;

: 's   ( Taddr -- adr.of.tasks.userarea )
         ' >body c@ +  statesmart ;  immediate

: tasks   ( -- )  ." Main " cr up@ dup 2+ @
    BEGIN  2dup -  WHILE  dup  4+ @  body> >name .name
       dup c@ 0= ( nop ) IF  ." sleeping"  THEN  cr
       2+  @  REPEAT  2drop ;



