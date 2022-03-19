
\ *** Block No. 90, Hexblock 5a

\ Struktur der Blockpuffer                       ks 04 jul 87

\   0 : link zum naechsten Puffer
\   2 : file     0 = direct access
\               -1 = leer,
\               sonst adresse eines file control blocks
\   4 : blocknummer
\   6 : statusflags   Vorzeichenbit kennzeichnet update
\   8 : Data ... 1 Kb ...








\ *** Block No. 91, Hexblock 5b

\ buffer mechanism                                ks 04 okt 87

  Variable isfile      isfile off   \ addr of file control block
  Variable fromfile    fromfile off \ fcb in kopieroperationen

  Variable prev        prev off     \ Listhead
| Variable buffers     buffers off  \ Semaphor

  $408 Constant b/buf               \ physikalische Groesse
  $400 Constant b/blk               \ bytes/block

  Defer r/w                         \ physikalischer Diskzugriff
  Variable error#      error# off   \ Nummer des letzten Fehlers
  Defer ?diskerror                  \ Fehlerbehandlung



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

  Code isfile@  ( -- addr )
     D push   isfile #) D mov   Next   end-code
\ : isfile@ ( -- addr )    isfile @ ;

  : buffer  ( blk -- addr )   isfile@ (buffer ;

  : block   ( blk -- addr )   isfile@ (block ;


\ *** Block No. 97, Hexblock 61

\ block & buffer manipulation                     ks 02 okt 87

  : update          $80 prev @ 6+ 1+ ( Byte-Order! )  c! ;

  : save-buffers    buffers lock
     BEGIN  updates? ?dup WHILE  backup REPEAT  buffers unlock ;

  : empty-buffers   buffers lock prev
     BEGIN  @ ?dup WHILE  dup emptybuf  REPEAT  buffers unlock ;

  : flush   file-link
     BEGIN  @ ?dup WHILE  dup fclose  REPEAT
     save-buffers empty-buffers ;




\ *** Block No. 98, Hexblock 62

\ Allocating buffers                              ks 31 oct 86
  $10000 Constant limit     Variable first

  : allotbuffer ( -- )
     first @  r0 @  -  b/buf 2+  u< ?exit
     b/buf negate first +!  first @ dup emptybuf
     prev @ over !  prev ! ;

  : freebuffer ( -- )   first @ limit b/buf - u<
     IF first @  backup  prev
       BEGIN dup @  first @ -  WHILE  @  REPEAT
     first @  @ swap !  b/buf first +!  THEN ;

  : all-buffers  BEGIN  first @ allotbuffer first @ =  UNTIL ;

| : init-buffers    prev off  limit first !  all-buffers ;
