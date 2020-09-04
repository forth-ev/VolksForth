
include vf-lbls-cbm.fth

7f fthpage

\ X16 labels

0fede >label console_put_char
 028c >label MsgFlg
 028b >label OutDev
 028a >label  InDev
09f2c >label BrdCol
 0266 >label BkgCol
 0284 >label PenCol
   8a >label PrgEnd  \ aka eal; seems unused
 0292 >label IOBeg   \ aka stal; seems unused
 0381 >label CurFlg  \ aka qtsw
 0385 >label InsCnt  \ aka insrt

Label ConOut  clc  console_put_char jmp

\ C64 labels that X16 doesn't have:

\ 028a >label KeyRep  \ aka rptflg


\ *** Block No. 129, Hexblock 81
81 fthpage

\ C64 c64key? getkey

Code c64key? ( -- flag)
 0 # lda  9f61 sta
 0a00a lda
 0<> ?[  0FF # lda  ]? pha
 Push jmp  end-code

Code getkey  ( -- 8b)
 0 # lda  9f61 sta
 0a00a lda  0<>
 ?[  sei  0a000 ldy
  [[  0a000 1+ ,X lda  0a000 ,X sta  inx
      0a00a cpx  0= ?]
  0a00a dec  tya  cli  0A0 # cmp
  0= ?[  bl # lda  ]?
 ]?
 Push0A jmp   end-code


\ *** Block No. 130, Hexblock 82
82 fthpage

\ C64 curon curoff

00C837 >label screen_get_char_color
00C8CC >label screen_restore_state
00C8B4 >label screen_save_state
00C830 >label screen_set_char_color
  037B >label blnsw  \ C64: $cc
  037C >label blnct  \ C64: $cd
  037D >label gdbln  \ C64: $ce
  037E >label blnon  \ C64: $cf
  0262 >label pnt    \ C64: $d1
  0380 >label pntr   \ C64: $d3
  0373 >label gdcol

Code curon   ( --)
\ 0D3 ldy  0D1 )Y lda  0CE sta  0CC stx
  screen_save_state jsr
  pntr ldy  screen_get_char_color jsr  gdbln sta  gdcol stx
  0 # ldx  blnsw stx  \ TODO: use stz
  screen_restore_state jsr
 xyNext jmp   end-code

Code curoff   ( --)
\ iny  0CC sty  0CD sty  0CF stx
\ 0CE lda  0D3 ldy  0D1 )Y sta
\ 1 # ldy  Next jmp   end-code
  screen_save_state jsr
  2 # ldy  blnsw sty  blnct sty  0 # ldx  blnon stx  \ TODO: use stz
  gdbln lda  gdcol ldx  pntr ldy  screen_set_char_color jsr
  screen_restore_state jsr
 xyNext jmp   end-code


include vf-sys-cbm.fth


\ *** Block No. 143, Hexblock 8f
\ ... continued
8f fthpage

Create ink-pot
\ border bkgnd pen  0
  6 c,   6 c,  3 c, 0 c,  \ Forth
 0E c,   6 c,  3 c, 0 c,  \ Edi
  6 c,   6 c,  3 c, 0 c,  \ User


\ *** Block No. 144, Hexblock 90
90 fthpage

\ x16 restore

\ 00E00A .nnmi
\ 00FFE1 .stop
\ 00E01F .prend

Label restore   pha txa pha tya pha cld
\ TODO: Replace with phx phy once 65c02 asm is available
  $ffe1 jsr ( stop )  0<> ?[ $e01f jmp ( prend ) ]?
' restart @ jmp  end-code


\ *** Block No. 145, Hexblock 91
91 fthpage

\ C64:Init                     06nov87re

: init-system
 [ restore ] Literal $318 ! ;  \ NMI-Vector

Label first-init
 sei cld  \ $db c,  \ for activating X16 monitor
 IOINIT jsr  CINT jsr  RESTOR jsr
  \ init. and set I/O-Vectors
 ink-pot    lda BrdCol sta \ border
 ink-pot 1+ lda BkgCol sta \ backgrnd
 ink-pot 2+ lda PenCol sta \ pen
\ $17 # lda  $D018 sta  \ low/upp +
\   0 # lda  $D01A sta  \ VIC-IRQ off
\ $1B # lda  $D011 sta  \ Textmode on
\   4 # lda   $288 sta  \ low screen
 cli rts end-code
first-init dup bootsystem 1+ !
               warmboot   1+ !
Code c64init first-init jsr
 xyNext jmp end-code
