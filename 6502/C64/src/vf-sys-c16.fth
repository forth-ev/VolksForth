
include vf-lbls-cbm.fth

\ *** Block No. 128, Hexblock 80
80 fthpage

\ C16-Labels                 clv13.4.87)

0ff4c >label ConOut
  090 >label IOStatus
  0ae >label CurDev
0ff19 >label BrdCol
0ff15 >label BkgCol
 053b >label PenCol
  0cb >label CurFlg
  0cf >label InsCnt
 0540 >label KeyRep

 055d >label PKeys


\ *** Block No. 129, Hexblock 81
81 fthpage

\ C16 c64key? getkey

Code c64key? ( -- flag)
 0ef lda  055d ora
 0<> ?[  0FF # lda  ]? pha
 Push jmp  end-code

Code getkey  ( -- 8b)
 0ebdd jsr
  0A0 # cmp 0= ?[  bl # lda  ]?
 Push0A jmp   end-code


\ *** Block No. 130, Hexblock 82
82 fthpage

\ C16 curon curoff

Code curon \ --
0ca lda clc 0c8 adc 0ff0d sta
0c9 lda     0 # adc 0b # sbc 0ff0c sta
next jmp end-code

Code curoff \ --
0ff # lda ff0c sta 0ff0d sta Next jmp
end-code


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


include vf-sys-cbm.fth


\ *** Block No. 143, Hexblock 8f
\ ... continued
8f fthpage

Create ink-pot
\ border bkgnd pen  0
 f6 c, 0f6 c, 03 c, 0 c,  \ Forth
0eE c, 0f6 c, 03 c, 0 c,  \ Edi
0f6 c, 0f6 c, 03 c, 0 c,  \ User


\ *** Block No. 146, Hexblock 92
92 fthpage

\ C16:Init                01oct87clv/re)

Code init-system $F7 # ldx  txs
 xyNext jmp end-code

(C16+ include vf-c16+irq.fth )

\ *** Block No. 147, Hexblock 93
93 fthpage

\ C16:..Init              01oct87clv/re)

Label first-init
   \ will be called in ROM first time
   \ later called from RAM
 sei (C16+ rom ( )
 \ new IRQ install
 (C16+ RAMIRQ $100 u/mod  # lda >IRQ 1+ sta  # lda >IRQ sta ( )
 $FF84 normJsr  $FF8A normJsr
    \ CIAs init. and set I/O-Vectors
 ink-pot    lda BrdCol sta \ border
 ink-pot 1+ lda BkgCol sta \ backgrnd
 ink-pot 2+ lda PenCol sta \ pen
 $80 # lda KeyRep sta \ repeat all keys
 $FF13 lda 04 # ora $FF13 sta \ low/upp
 (C16+ ram ( ) cli rts end-code

first-init dup bootsystem 1+ !
               warmboot   1+ !

Code c64init first-init jsr
 xyNext jmp end-code


\ *** Block No. 148, Hexblock 94
94 fthpage

\ C16-Pushkeys C64-like   01oct87clv/re)

Label InitPKs \ Pushkeys: Daten
00 c, 00 c,  \ curr. numb Char, currPtr
01 c, 01 c, 01 c, 01 c, \ StrLength
01 c, 01 c, 01 c, 01 c, \   "

85 c, 86 c, 87 c, 89 c, \ Content
8a c, 8b c, 8c c, 88 c, \   "


here InitPKs - >label InitPKlen


Code C64fkeys \ Pushkeys a la C64
  InitPKlen # ldx
  [[ dex  0>= ?[[
    InitPKs ,X lda PKeys ,x sta ]]?
  xyNext jmp end-code

(C16- | Create (bye  $FFF6  here 2- ! )

(C16+ | CODE   (bye  rom $FFF6 jmp  end-code )
