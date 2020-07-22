\ *** Block No. 102, Hexblock 66
66 fthpage

( buffer mechanism            15dec83ks)

User file           0 file !
        \ adr of file control block
Variable prev       0 prev !
        \ Listhead
Variable buffers  0 buffers !
        \ Semaphore
0408 Constant b/buf
        \ Physical Size

\ Structure of Buffer:
\  0 : link
\  2 : file
\  4 : blocknr
\  6 : statusflags
\  8 : Data .. 1 KB ..

\ Statusflag bits: 15   1 -> updated

\ file = -1 empty buffer
\      = 0 no fcb , direct access
\      = else  adr of fcb
\      ( system   dependent )


\ *** Block No. 103, Hexblock 67
67 fthpage

( search for blocks in memory 11jun85bp)

Label thisbuffer?        2 # ldy
   [[  N 4 + )Y lda N 2- ,Y cmp
 0= ?[[  iny  6 # cpy 0= ?] ]? rts
              \ zero if this buffer )

| Code (core?
 ( blk file -- addr / blk  file )
 \ N-Area : 0 blk 2 file 4 buffer
 \          6 predecessor
 3 # ldy
   [[ SP )Y lda  N ,Y sta  dey  0< ?]
 user' offset # ldy
 clc  UP )Y lda  N 2+  adc  N 2+  sta
 iny  UP )Y lda  N 3 + adc  N 3 + sta
 prev    lda  N 4 + sta
 prev 1+ lda  N 5 + sta
 thisbuffer? jsr    0= ?[







\ *** Block No. 104, Hexblock 68
68 fthpage

(   "                         11jun85bp)

Label blockfound     SP 2inc
 1 # ldy
 8 #   lda  clc N 4 + adc SP X) sta
 N 5 + lda        0 # adc SP )Y sta
 ' exit @ jmp  ]?
 [[ N 4 + lda  N 6 + sta
    N 5 + lda  N 7 + sta
    N 6 + X) lda  N 4 + sta  1 # ldy
    N 6 + )Y lda  N 5 + sta  N 4 + ora
     0= ?[ ( list empty )  Next jmp ]?
   thisbuffer? jsr 0= ?] \ found, relink
 N 4 + X) lda  N 6 + X) sta  1 # ldy
 N 4 + )Y lda  N 6 + )Y sta
 prev    lda  N 4 + X) sta
 prev 1+ lda  N 4 + )Y sta
 N 4 + lda  prev    sta
 N 5 + lda  prev 1+ sta
 blockfound jmp    end-code






\ *** Block No. 105, Hexblock 69
69 fthpage

\ (core?                       23sep85bp

\ | : this?   ( blk file bufadr -- flag )
\    dup 4+ @  swap 2+ @  d= ;

\ | : (core?
\    ( blk file -- dataaddr / blk file )
\   BEGIN  over offset @ + over  prev @
\     this? IF rdrop 2drop prev @ 8 + exit
\           THEN
\     2dup >r offset @ + >r prev @
\     BEGIN  dup @ ?dup
\        0= IF rdrop rdrop drop exit THEN
\       dup r> r> 2dup >r >r  rot this? 0=
\     WHILE  nip  REPEAT
\     dup @ rot !  prev @ over !  prev !
\     rdrop rdrop
\   REPEAT ;


\ *** Block No. 106, Hexblock 6a
6a fthpage

( (diskerr                    11jun85bp)

: (diskerr   ." error !  r to retry "
 key dup Ascii r =  swap Ascii R =
 or not  Abort" aborted"  ;


Defer diskerr  ' (diskerr  Is diskerr

Defer r/w
















\ *** Block No. 107, Hexblock 6b
6b fthpage

( backup emptybuf readblk     11jun85bp)

| : backup   ( bufaddr --)
 dup 6+ @  0<
 IF  2+  dup @ 1+
            \ buffer empty if file = -1
  IF input push output push standardi/o
   BEGIN dup 6+ over 2+ @ 2 pick @ 0 r/w
   WHILE ." write " diskerr
   REPEAT   THEN
  080 over 4+ 1+ ctoggle  THEN
 drop ;

| : emptybuf  ( bufaddr --)
   2+ dup on 4+ off ;

| : readblk
   ( blk file addr -- blk file addr)
 dup emptybuf  input push  output push
 standardi/o   >r
 BEGIN over offset @ + over
       r@ 8 +  -rot   1  r/w
 WHILE ." read " diskerr
 REPEAT  r>  ;


\ *** Block No. 108, Hexblock 6c
6c fthpage

( take mark updates? full? core?     bp)

| : take   ( -- bufaddr)    prev
 BEGIN  dup @  WHILE  @  dup 2+ @ -1 =
 UNTIL
 buffers lock   dup backup ;

| : mark
 ( blk file bufaddr -- blk file )
 2+ >r 2dup r@ !  offset @ +  r@ 2+ !
 r> 4+ off  buffers unlock ;

| : updates?  ( -- bufaddr / flag)
 prev  BEGIN  @ dup  WHILE  dup 6+ @
   0<  UNTIL ;

| : full?   ( -- flag)
 prev BEGIN @ dup @ 0= UNTIL  6+ @ 0< ;

: core?  ( blk file -- addr /false)
 (core? 2drop false ;





\ *** Block No. 109, Hexblock 6d
6d fthpage

( block & buffer manipulation 11jun85bp)

: (buffer ( blk file -- addr)
 BEGIN  (core? take mark
 REPEAT ;

: (block  ( blk file -- addr)
 BEGIN  (core? take readblk mark
 REPEAT ;

| Code file@  ( -- n )
 user' file # ldy
 UP )Y lda  pha  iny  UP )Y lda
 Push jmp  end-code

: buffer  ( blk -- addr )
 file@  (buffer ;

: block   ( blk -- addr )
 file@  (block ;






\ *** Block No. 110, Hexblock 6e
6e fthpage

( block & buffer manipulation 09sep84ks)

: update   080 prev @  6+ 1+ c! ;

: save-buffers
 buffers lock BEGIN   updates? ?dup
              WHILE backup REPEAT
 buffers unlock  ;

: empty-buffers
 buffers lock  prev
 BEGIN @ ?dup
 WHILE dup emptybuf
 REPEAT  buffers unlock ;

: flush    save-buffers empty-buffers ;










\ *** Block No. 111, Hexblock 6f
6f fthpage

( moving blocks               15dec83ks)

 : (copy   ( from to --)
 dup file@
 core? IF prev @ emptybuf THEN
 full? IF  save-buffers   THEN
 offset @ + swap block 2- 2- !  update ;

 : blkmove  ( from to quan --)
 save-buffers >r
 over r@ + over u> >r  2dup u< r> and
  IF  r@ r@ d+  r> 0 ?DO -1 -2 d+
                         2dup (copy LOOP
  ELSE          r> 0 ?DO 2dup (copy 1
                             1 d+   LOOP
  THEN  save-buffers 2drop ;

: copy    ( from to --)   1 blkmove ;

: convey  ( [blk1 blk2] [to.blk --)
 swap  1+  2 pick -   dup 0> not
 Abort" no!!"  blkmove ;




\ *** Block No. 112, Hexblock 70
70 fthpage

\ Allocating buffers          clv12jul87

E400 Constant limit     Variable first

: allotbuffer   ( -- )
 first @  r0 @ -  b/buf 2+ u< ?exit
 b/buf negate first +!
 first @ dup emptybuf
 prev  @ over !  prev !   ;

: freebuffer    ( -- )
 first @   limit b/buf - u<
  IF first @ backup prev
    BEGIN  dup @ first @  -
    WHILE  @  REPEAT
  first @  @ swap ! b/buf first +!
  THEN ;

: all-buffers
 BEGIN  first @ allotbuffer
        first @  = UNTIL ;
