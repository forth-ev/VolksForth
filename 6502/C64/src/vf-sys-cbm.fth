
\ *** Block No. 131, Hexblock 83
83 fthpage

( #bs #cr ..keyboard         clv12.4.87)

: c64key  ( -- 8b)
 curon BEGIN pause c64key?  UNTIL
 curoff getkey ;

14 Constant #bs   0D Constant #cr

: c64decode
 ( addr cnt1 key -- addr cnt2)
  #bs case?  IF  dup  IF del 1- THEN
                            exit  THEN
  #cr case?  IF  dup span ! exit THEN
  >r  2dup +  r@ swap c!  r> emit  1+ ;

: c64expect ( addr len1 -- )
 span !  0
 BEGIN  dup span @  u<
 WHILE  key  decode
 REPEAT 2drop space ;

Input: keyboard   [ here input ! ]
 c64key c64key? c64decode c64expect ;


\ *** Block No. 132, Hexblock 84
84 fthpage

( con! printable?            clv11.4.87)

Code con!  ( 8b --)   SP X) lda
Label (con!     ConOut jsr    SP 2inc
Label (con!end  CurFlg stx InsCnt stx
 1 # ldy ;c:  pause ;

Label (printable?   \ for CBM-Code !
                    \ CS is printable
  80 # cmp  CC ?[   bl # cmp  rts  ]?
 0E0 # cmp  CC ?[  0C0 # cmp  rts  ]?
 clc  rts  end-code

Code printable? ( 8b -- 8b flag)
 SP X) lda  (printable? jsr CS ?[ dex ]?
 txa  PushA jmp     end-code


\ *** Block No. 133, Hexblock 85
85 fthpage

( emit cr del page at at?    clv11.4.87)

Code c64emit  ( 8b -- )
 SP X) lda  (printable? jsr
    CC ?[  Ascii . # lda ]?
 (con! jmp   end-code

: c64cr     #cr con! ;

: c64del    9D con!  space  9D con! ;

: c64page   93 con! ;

Code c64at  ( row col --)
 2 # lda  Setup jsr
 N 2+ ldx  N ldy  clc  PLOT jsr
(C16 \ ) 0D3 ldy  0D1 )Y lda   0CE sta
 xyNext jmp  end-code

Code c64at?  ( -- row col)
 SP 2dec txa  SP )Y sta
 sec  PLOT jsr
 28 # cpy  tya  CS ?[ 28 # sbc ]?
 pha  txa  0 # ldx  SP X) sta  pla
 Push0A jmp  end-code


\ *** Block No. 134, Hexblock 86
86 fthpage

( type display (bye          clv11.4.87)

Code  c64type  ( adr len -- )
 2 # lda  Setup jsr  0 # ldy
  [[  N cpy  0<>
  ?[[  N 2+ )Y lda  (printable? jsr
         CC ?[  Ascii . # lda  ]?
 ConOut jsr  iny  ]]?
 (con!end jmp   end-code

Output: display   [ here output ! ]
 c64emit c64cr c64type c64del c64page
 c64at c64at? ;


\ *** Block No. 135, Hexblock 87
87 fthpage

\ b/blk drive >drive drvinit  clv14:2x87

400 Constant b/blk

0AA Constant blk/drv

Variable (drv    0 (drv !

| : disk ( -- dev.no )   (drv @ 8 + ;

: drive  ( drv# -- )
 blk/drv *  offset ! ;

: >drive ( block drv# -- block' )
 blk/drv * +   offset @ - ;

: drv?    ( block -- drv# )
 offset @ + blk/drv / ;

: drvinit  noop ;


\ *** Block No. 136, Hexblock 88
88 fthpage

( i/o busoff                  10may85we)

Variable i/o  0 i/o !  \ Semaphore

Code busoff  ( --)   CLRCHN jsr
Label unlocki/o  1 # ldy  0 # ldx
 ;c:  i/o unlock ;

Label nodevice     0 # ldx  1 # ldy
 ;c:  busoff   buffers unlock
      true Abort" no device" ;


\ *** Block No. 137, Hexblock 89
89 fthpage

\ ?device                     clv12jul87

Label (?dev
 90 stx (C16 $ae sta ( ) LISTEN jsr
        \ because of error in OS
 60 # lda  SECOND jsr  UNLSN jsr
 90 lda  0<> ?[ pla pla nodevice jmp ]?
 rts    end-code

 Code (?device  ( dev --)
 SP X) lda  (?dev jsr  SP 2inc
 unlocki/o jmp  end-code

: ?device  ( dev -- )
 i/o lock  (?device ;

 Code (busout  ( dev 2nd -- )
 MsgFlg stx  2 # lda  Setup jsr
 N 2+ lda  (?dev jsr
 N 2+ lda  LISTEN jsr
 N lda  60 # ora SECOND jsr
 N 2+ ldx  OutDev stx
 xyNext jmp  end-code


\ *** Block No. 138, Hexblock 8a
8a fthpage

\ busout/open/close/in        clv12jul87

: busout    ( dev 2nd -- )
 i/o lock (busout ;

: busopen   ( dev 2nd -- )
 0F0 or busout ;

: busclose  ( dev 2nd -- )
 0E0 or busout busoff ;

 Code (busin  ( dev 2nd -- )
 MsgFlg stx  2 # lda  Setup jsr
 N 2+ lda  (?dev jsr
 N 2+ lda  TALK jsr
 N lda  60 # ora (C16 $ad sta ( )
 TKSA jsr
\ because of error in old C16 OS
 N 2+ ldx  InDev stx
 xyNext jmp end-code

: busin  ( dev 2nd -- )
 i/o lock  (busin ;


\ *** Block No. 139, Hexblock 8b
8b fthpage

( bus-!/type/@/input derror?  24feb85re)

Code bus!  ( 8b --)
 SP X) lda  CIOUT jsr  (xydrop jmp
 end-code

: bustype  ( adr n --)
 bounds  ?DO  I c@ bus!  LOOP pause ;

Code bus@  ( -- 8b)
 ACPTR jsr Push0A jmp  end-code

: businput  ( adr n --)
 bounds  ?DO  bus@ I c! LOOP pause ;

: i/o-status?  $90 c@ ;

: derror?  ( -- flag )
 disk $F busin bus@  dup Ascii 0 -
   IF  BEGIN emit bus@ dup #cr =  UNTIL
   0= cr ELSE BEGIN bus@ #cr = UNTIL
   THEN   0=  busoff ;
