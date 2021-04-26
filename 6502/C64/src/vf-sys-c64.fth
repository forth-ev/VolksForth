
include vf-lbls-cbm.fth

\ *** Block No. 127, Hexblock 7f
7f fthpage

\ C64-Labels                 clv13.4.87)

0E716 >label ConOut
  090 >label IOStatus
  09d >label MsgFlg
  09a >label OutDev
  099 >label  InDev
0d020 >label BrdCol
0d021 >label BkgCol
 0286 >label PenCol
  0ae >label PrgEnd  \ aka eal; seems unused
  0c1 >label IOBeg   \ aka stal; seems unused
  0d4 >label CurFlg
  0d8 >label InsCnt
 028a >label KeyRep

  0cc >label blnsw
  0cd >label blnct
  0ce >label gdbln
  0cf >label blnon
  0d1 >label pnt
  0d3 >label pntr

\ *** Block No. 129, Hexblock 81
81 fthpage

\ C64 c64key? getkey

Code c64key? ( -- flag)
 0C6 lda
 0<> ?[  0FF # lda  ]? pha
 Push jmp  end-code

Code getkey  ( -- 8b)
 0C6 lda  0<>
 ?[  sei  0277 ldy
  [[  0277 1+ ,X lda  0277 ,X sta  inx
      0C6 cpx  0= ?]
  0C6 dec  tya  cli  0A0 # cmp
  0= ?[  bl # lda  ]?
 ]?
 Push0A jmp   end-code


\ *** Block No. 130, Hexblock 82
82 fthpage

\ C64 curon curoff

Code curon   ( --)
 pntr ldy  pnt )Y lda  gdbln sta  blnsw stx
 xyNext jmp   end-code

Code curoff   ( --)
 iny  blnsw sty  blnct sty  blnon stx
 gdbln lda  pntr ldy  pnt )Y sta
 1 # ldy  Next jmp   end-code


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

\ C64 restore

Label asave 0 c,    Label 1save 0 c,

Label continue
 pha  1save lda  1 sta  pla  rti

Label restore   sei  asave sta
 continue $100 /mod
 # lda pha  # lda pha  php  \ for RTI
 asave lda pha  txa pha  tya pha
 1 lda 1save sta
 $36 # lda   1 sta  \ Basic off ROM on
 $7F # lda  $DD0D sta
 $DD0D ldy  0< ?[
Label 6526-NMI $FE72 jmp  ]?
 UDTIM jsr STOP jsr  \ RUN/STOP ?
 6526-NMI bne        \ not >>-->
 ' restart @ jmp  end-code


\ *** Block No. 145, Hexblock 91
91 fthpage

\ C64:Init                     06nov87re

: init-system   $FF40 dup $C0 cmove
 [ restore ] Literal  dup
 $FFFA ! $318 ! ;  \ NMI-Vector to RAM

Label first-init
 sei cld
 IOINIT jsr  CINT jsr  RESTOR jsr
  \ init. and set I/O-Vectors
 $36 # lda   01 sta        \ Basic off
 ink-pot    lda BrdCol sta \ border
 ink-pot 1+ lda BkgCol sta \ backgrnd
 ink-pot 2+ lda PenCol sta \ pen
$80 # lda KeyRep sta  \ repeat all keys
$17 # lda  $D018 sta  \ low/upp +
  0 # lda  $D01A sta  \ VIC-IRQ off
$1B # lda  $D011 sta  \ Textmode on
  4 # lda   $288 sta  \ low screen
 cli rts end-code
first-init dup bootsystem 1+ !
               warmboot   1+ !
Code c64init first-init jsr
 xyNext jmp end-code

| Create (bye  $FCE2  here 2- !
