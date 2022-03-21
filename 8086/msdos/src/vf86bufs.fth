
\ *** Block No. 90, Hexblock 5a

\ Struktur der Blockpuffer                       ks 04 jul 87

\   0 : link zum naechsten Puffer
\   2 : file     0 = direct access
\               -1 = leer,
\               sonst adresse eines file control blocks
\   4 : blocknummer
\   6 : statusflags   Vorzeichenbit kennzeichnet update
\   8 : Data ... 1 Kb ...


  Forth definitions






\ *** Block No. 91, Hexblock 5b

\ buffer mechanism                                ks 04 okt 87

  Variable prev        prev off     \ Listhead of the buffers' list
| Variable buffers     buffers off  \ Semaphor

  $408 Constant b/buf               \ physikalische Groesse
  $400 Constant b/blk               \ bytes/block

  Defer r/w                         \ physikalischer Diskzugriff


\ *** Block No. 92, Hexblock 5c

\ (core?                                          ks 28 mai 87

  Code (core? ( blk file -- dataaddr / blk file )
     A pop   A push   D D or  0= ?[  u' offset U D) A add  ]?
     prev #) W mov   2 W D) D cmp  0=
     ?[  4 W D) A cmp  0=
         ?[  8 W D) D lea   A pop   ' exit @ # jmp  ]? ]?
     [[ [[  W ) C mov   C C or  0= ?[  Next  ]?
            C W xchg   4 W D) A cmp  0= ?]  2 W D) D cmp  0= ?]
     W ) A mov   prev #) D mov   D W ) mov   W prev #) mov
     8 W D) D lea   C W mov   A W ) mov   A pop
     ' exit @ # jmp
  end-code




\ *** Block No. 93, Hexblock 5d

\ (core?                                          ks 31 oct 86

\ | : this? ( blk file bufadr -- flag )
\     dup 4+ @  swap 2+ @  d= ;

\  .( (core?:  offset is handled differently in code! )

\ | : (core? ( blk file -- dataaddr / blk file )
\     BEGIN  over offset @ + over prev @ this?
\        IF  rdrop 2drop prev @ 8 + exit  THEN
\        2dup >r offset @ + >r prev @
\        BEGIN dup @ ?dup 0= IF  rdrop rdrop drop exit  THEN
\              dup r> r> 2dup >r >r rot this?  0=
\        WHILE nip REPEAT
\        dup @ rot ! prev @ over ! prev ! rdrop rdrop
\     REPEAT ;

\ *** Block No. 94, Hexblock 5e

\ backup emptybuf readblk                         ks 23 jul 87

| : backup ( bufaddr -- )       dup 6+ @ 0<
     IF  2+ dup @ 1+         \ buffer empty if file = -1
         IF  BEGIN  dup 6+ over 2+ @ 2 pick @ 0 r/w
             WHILE  1 ?diskerror  REPEAT
         THEN  4+ dup @ $7FFF and over !  THEN
     drop ;

  : emptybuf ( bufaddr -- )      2+ dup on 4+ off ;

| : readblk ( blk file addr -- blk file addr )
     dup emptybuf  >r
     BEGIN  2dup   0= offset @ and  +
            over   r@ 8 + -rot 1 r/w
     WHILE  2 ?diskerror  REPEAT r>  ;

\ *** Block No. 95, Hexblock 5f

\ take mark updates? full? core?                  ks 04 jul 87

| : take ( -- bufaddr)    prev
     BEGIN  dup @ WHILE  @ dup 2+ @ -1 = UNTIL
     buffers lock   dup backup ;

| : mark ( blk file bufaddr -- blk file )   2+ >r
     2dup r@ !  over 0= offset @ and +   r@ 2+ !
     r> 4+ off   buffers unlock ;

| : updates? ( -- bufaddr / flag)
     prev  BEGIN  @ dup  WHILE  dup 6+ @ 0< UNTIL ;

  : core? ( blk file -- addr /false )   (core? 2drop false ;



\ *** Block No. 96, Hexblock 60

\ block & buffer manipulation                     ks 01 okt 87

  : (buffer ( blk file -- addr )
      BEGIN  (core? take mark  REPEAT ;

  : (block ( blk file -- addr )
      BEGIN  (core? take readblk mark  REPEAT ;

  : buffer  ( blk -- addr )   isfile@ (buffer ;

  : block   ( blk -- addr )   isfile@ (block ;

  : (blk-source ( -- addr len )   blk @ ?dup
     IF  loadfile @ (block b/blk  exit  THEN  tib #tib @ ;

  ' (blk-source IS source


\ *** Block No. 97, Hexblock 61

\ block & buffer manipulation                     ks 02 okt 87

  : update          $80 prev @ 6+ 1+ ( Byte-Order! )  c! ;

  : (save-buffers    buffers lock
     BEGIN  updates? ?dup WHILE  backup REPEAT  buffers unlock ;

' (save-buffers IS save-buffers

  : (empty-buffers   buffers lock prev
     BEGIN  @ ?dup WHILE  dup emptybuf  REPEAT  buffers unlock ;

' (empty-buffers IS empty-buffers


  Dos definitions

\ *** Block No. 137, Hexblock 89

\ /block  *block                                  ks 02 okt 87

  Code /block  ( d -- rest blk )   A D xchg   C pop
     C D mov   A shr   D rcr   A shr   D rcr   D+ D- mov
     A- D+ xchg   $3FF # C and   C push   Next
  end-code
\ : /block  ( d -- rest blk )   b/blk um/mod ;

  Code *block  ( blk -- d )  A A xor   D+ D- xchg   D+ A+ xchg
     A+ sal   D rcl   A+ sal   D rcl   A push   Next
  end-code
\ : *block  ( blk -- d )   b/blk um* ;





\ *** Block No. 138, Hexblock 8a

\ fblock@  fblock!                                ks 19 mär 88
  Dos definitions

| : ?beyond   ( blk -- blk )  dup 0< 0=exit  9 ?diskerror ;

| : fblock   ( addr blk fcb -- seg:addr quan fcb )
     fcb !  ?beyond dup *block  fcb @  fseek   ds@ -rot
     fcb @ f.size 2@ /block rot -  ?beyond
     IF  drop b/blk  THEN  fcb @ ;

  : fblock@  ( addr blk fcb -- )    fblock lfgets drop ;

  : fblock!  ( addr blk fcb -- )    fblock lfputs ;




\ *** Block No. 139, Hexblock 8b

\ (r/w  flush                                     ks 18 mär 88
  Forth definitions

  : (r/w   ( addr blk fcb r/wf -- *f )  over fcb !  over
     IF  IF  fblock@ false exit  THEN  fblock! false exit
     THEN  >r drop  /drive ?drive
     r> IF  block@ exit  THEN  block! ;

  ' (r/w Is r/w


  Dos definitions

| : filebuffer?   ( fcb -- fcb bufaddr / fcb ff )
     prev  BEGIN  @ dup WHILE  2dup 2+ @  = UNTIL ;

: (flush-file-buffers     ( fcb -- )
     BEGIN  filebuffer? ?dup
     WHILE  dup backup emptybuf  REPEAT  drop ;

' (flush-file-buffers IS flush-file-buffers


\ *** Block No. 81, Hexblock 51

  Forth definitions

\ +load thru +thru --> rdepth depth               ks 26 jul 87

  : (load  ( blk offset -- )   isfile@ >r
     loadfile @ >r   fromfile @ >r   blk @ >r   >in @ >r
     >in !   blk !  isfile@ loadfile !  .status  interpret
     r> >in !   r> blk !   r> fromfile !   r> loadfile !
     r> isfile ! ;

  : load   ( blk -- )     ?dup 0=exit  0 (load ;
  ' load IS include-load

  : +load    ( offset -- )       blk @ + load ;

  : thru     ( from to -- )      1+ swap DO I  load LOOP ;

  : +thru    ( off0 off1 -- )    1+ swap DO I +load LOOP ;

  : -->        1 blk +! >in off .status ; immediate

  : loadfrom     ( n -- )   pushfile  use load close ;

  : \\       b/blk >in ! ; immediate

  : list ( scr -- )  dup capacity u<
     IF  scr !  ."  Scr " scr @ .
         ." Dr " drv .  isfile@ .file
         l/s 0 DO  cr I 2 .r space   scr @ block
                   I c/l * +   c/l -trailing type
               LOOP  cr exit
     THEN  9 ?diskerror ;

  : view   'file list ;
  : help   'file capacity 2/ + list ;




\ *** Block No. 122, Hexblock 7a

\ Disk capacities                                 ks 08 aug 88
  Dos definitions

  6 Constant #drives

  Create capacities   $4B0 , $4B0 , $1B31 , $1B31 , $1B0F , 0 ,

| Code ?capacity ( +n -- cap )  D shl   capacities # W mov
     D W add   W ) D mov   Next   end-code








\ *** Block No. 123, Hexblock 7b

\ MS-dos disk handlers direct access              ks 31 jul 87

| Code block@  ( addr blk drv -- ff )
     D- A- mov   D pop   C pop   R push   U push
     I push   C R mov   2 # C mov   D shl   $25 int
  Label end-r/w    I pop   I pop   U pop   R pop   0 # D mov
     CS ?[  D+ A+ mov   A error# #) mov   D dec  ]?  Next
  end-code

| Code block!  ( addr blk drv -- ff )  D- A- mov   D pop
     C pop   R push   U push   I push   C R mov   2 # C mov
     D shl   $26 int   end-r/w # jmp
  end-code




\ *** Block No. 124, Hexblock 7c

\ MS-dos disk handlers direct access              ks cas 18jul20

| : ?drive  ( +n -- +n )   dup #drives u< ?exit
     Error" beyond drive capacity" ;

  : /drive ( blk1 -- blk2 drive )  0 swap  #drives 0
     DO  dup I ?capacity under u< IF drop LEAVE THEN
         - swap 1+ swap  LOOP  swap ;

  : blk/drv  ( -- capacity )  drv ?capacity ;

  Forth definitions

  : >drive    ( blk1 +n -- blk2 )   ?drive
     0 swap  drv  2dup u> dup >r  0= IF  swap  THEN
     ?DO  I ?capacity + LOOP  r> IF  negate  THEN - ;

\ *** Block No. 143, Hexblock 8f

\ drive  drv  capacity   drivenames               ks 18 mär 88

  : drive ( n -- )   isfile@ IF  ~select exit  THEN
     ?drive   offset off 0 ?DO  I ?capacity offset +!  LOOP ;

  : drv   ( -- n )
     isfile@ IF  ~disk? exit  THEN  offset @ /drive nip ;

  : capacity   ( -- n )   isfile@ ?dup
     IF  dup f.handle @ 0= IF  dup freset  THEN
         f.size 2@ /block swap 0<> - exit  THEN  blk/drv ;

| : Drv:   Create c,  Does> c@ drive ;

  0 Drv: A:     1 Drv: B:     2 Drv: C:     3 Drv: D:
  4 Drv: E:     5 Drv: F:     6 Drv: G:     7 Drv: H:

\ *** Block No. 98, Hexblock 62

\ Allocating buffers                              ks 31 oct 86

  : allotbuffer ( -- )
     first @  r0 @  -  b/buf 2+  u< ?exit
     b/buf negate first +!  first @ dup emptybuf
     prev @ over !  prev ! ;

  : freebuffer ( -- )   first @ limit b/buf - u<
     IF first @  backup  prev
       BEGIN dup @  first @ -  WHILE  @  REPEAT
     first @  @ swap !  b/buf first +!  THEN ;

  : all-buffers  BEGIN  first @ allotbuffer first @ =  UNTIL ;

| : (init-buffers    prev off  limit first !  all-buffers ;

' (init-buffers IS init-buffers
