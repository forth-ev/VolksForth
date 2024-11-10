\ *** Block No. 94, Hexblock 5e

\ buffer mechanism                             20Oct86   07Oct87

Variable prev        0 prev !     \ Listhead
| Variable buffers   0 buffers !  \ Semaphor
$408 Constant b/buf               \ physikalische Groesse
\ \\ Struktur eines Buffers:    0 : link
\                               2 : file
\                               4 : blocknummer
\                               6 : statusflags
\                               8 : Data ... 1 Kb ...
\ Statusflag bits : 15   1 -> updated
\ file :  -1 -> empty buffer,  0 -> no fcb, direct access
\         else addr of fcb  ( system dependent )

\ *** Block No. 95, Hexblock 5f

\ search for blocks in memory                            30Jun86
| Variable pred
\ DE:blk  BC:file  HL:bufadr

Label thisbuffer?   ( Zero = this buffer )
   H push   H inx   H inx   M A mov   C cmp   0=
   ?[ H inx   M A mov   B cmp   0= ?[ H inx   M A mov   E cmp
      0= ?[ H inx   M A mov   D cmp ]? ]? ]?   H pop   ret

Code (core?  ( blk file -- adr\blk file )
   IP H mvx   Ipsave shld
   user' offset D lxi   UP lhld   D dad
   M E mov   H inx   M D mov
   B pop   H pop   H push   B push   D dad   xchg
   prev lhld
   thisbuffer? call 0= ?[

\ *** Block No. 96, Hexblock 60

\ search for blocks in memory                            30Jun86

Label blockfound
   D pop   D pop   8 D lxi   D dad   H push   ' exit @ jmp ]?
   [[ pred shld
      M A mov   H inx   M H mov   A L mov
      H ora 0= ?[ IPsave lhld   H IP mvx   Next ]?
      thisbuffer? call   0= ?]
      xchg   pred lhld   D ldax   A M mov
      H inx  D inx   D ldax   A M mov   D dcx
      prev lhld    xchg    E M mov  H inx  D M mov
      H dcx   prev shld
   blockfound jmp   end-code




\ *** Block No. 97, Hexblock 61

\ (core?                                                 29Jun86
\ \\
\
\ | : this? ( blk file bufadr -- flag )
\      dup 4+ @  swap 2+ @  d= ;
\
\ | : (core? ( blk file -- dataaddr / blk file )
\      BEGIN  over offset @ + over prev @ this?
\         IF  rdrop 2drop prev @ 8 + exit  THEN
\         2dup >r offset @ + >r prev @
\         BEGIN dup @ ?dup 0= IF  rdrop rdrop drop exit  THEN
\               dup r> r> 2dup >r >r rot this?  0=
\         WHILE nip REPEAT
\         dup @ rot ! prev @ over ! prev ! rdrop rdrop
\      REPEAT ;


\ *** Block No. 98, Hexblock 62

\ (diskerr                                     29Jul86   07Oct87

: (diskerr
   ." error! r to retry "  key $FF and
   capital Ascii R = not Abort" aborted" ;

Defer diskerr
' (diskerr Is diskerr

Defer r/w







\ *** Block No. 99, Hexblock 63

\ backup emptybuf readblk                                20Oct86

  : backup ( bufaddr -- )       dup 6+ @ 0<
     IF 2+ dup @ 1+         \ buffer empty if file = -1
       IF input push output push standardi/o
         BEGIN  dup 6+ over 2+ @ 2 pick @ 0 r/w
         WHILE ." write " diskerr
         REPEAT  THEN  4+ dup @ $7FFF and over ! THEN  drop ;

: emptybuf ( bufaddr -- )      2+ dup on 4+ off ;

| : readblk ( blk file addr -- blk file addr )
     dup emptybuf
     input push output push standardi/o >r
     BEGIN  over offset @ + over r@ 8 + -rot 1 r/w
     WHILE  ." read " diskerr  REPEAT r>  ;

\ *** Block No. 100, Hexblock 64

\ take mark updates? core?                     10Mar86   19Nov87

| : take ( -- bufaddr)    prev
     BEGIN dup @ WHILE @ dup 2+ @ -1 = UNTIL
     buffers lock   dup backup ;

| : mark ( blk file bufaddr -- blk file )
     2+ >r  2dup r@ !  offset @ + r@ 2+ !  r> 4+ off
     buffers unlock ;

| : updates? ( -- bufaddr / flag)
     prev  BEGIN  @ dup  WHILE dup 6+ @ 0<  UNTIL ;

: core? ( blk file -- addr /false )   (core? 2drop false ;



\ *** Block No. 101, Hexblock 65

\ block & buffer manipulation                  20Oct86   18Nov87

: (buffer ( blk file -- addr )
    BEGIN  (core? take mark  REPEAT ;

: (block ( blk file -- addr )
    BEGIN  (core? take readblk mark  REPEAT ;

: buffer ( blk -- addr )   isfile@ (buffer ;

: block  ( blk -- addr )   isfile@ (block ;

: (blk-source   ( -- addr len)
    blk @ ?dup IF loadfile @ (block  b/blk  exit THEN
    tib #tib @ ;

' (blk-source IS source

\ : isfile@ ( -- addr )    isfile @ ;

\ *** Block No. 102, Hexblock 66

\ block & buffer manipulation                            05Oct87

: update          $80 prev @ 6+ 1+ ( Byte-Order! )  c! ;

: (save-buffers ( -- )   buffers lock
   BEGIN updates? ?dup WHILE backup REPEAT save-dos-buffers
   buffers unlock ;
' (save-buffers IS save-buffers

: empty-buffers ( -- )   buffers lock prev
   BEGIN @ ?dup  WHILE dup emptybuf REPEAT  buffers unlock ;

: flush           save-buffers empty-buffers ;



\ *** Block No. 103, Hexblock 67

\ Allocating buffers                                     10Oct87
Variable first

: allotbuffer ( -- )
   first @  r0 @  -  b/buf 2+  u< ?exit
   b/buf negate first +!  first @ dup emptybuf
   prev @ over !  prev ! ;

: freebuffer ( -- )  first @ limit b/buf - u<
   IF first @  backup  prev
     BEGIN dup @  first @ -  WHILE  @  REPEAT
   first @  @ swap !  b/buf first +!  THEN ;

: all-buffers  BEGIN  first @ allotbuffer first @ =  UNTIL ;

| : (init-buffers    prev off  limit first ! all-buffers  flush ;
' (init-buffers IS init-buffers

\ *** Block No. 125, Hexblock 7d

\ Default Disk Interface: read/write                     14Feb88

Target Dos also

| : rec# ( 'dosfcb -- 'rec# )  &33 + ;

: (r/w  ( adr blk file r/wf -- flag )  >r
    dup 0= Abort" no Direct Disk IO supported! " >dosfcb
    swap rec/blk *  over rec#   0 over 2+ c!   !
    r> rot  b/blk bounds
    DO I dma!  2dup IF rec@ drop
       ELSE rec! IF 2drop true endloop exit THEN THEN
       over rec#   0 over 2+ c!  1 swap +!
    b/rec +LOOP  2drop false ;

' (r/w Is r/w

: list ( blk -- )
   scr ! ." Scr " scr @ u.
   l/s 0 DO
     cr I 2 .r space scr @ block I c/l * + c/l -trailing type
   LOOP cr ;
