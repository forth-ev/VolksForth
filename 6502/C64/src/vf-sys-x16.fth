
include vf-lbls-cbm.fth

7f fthpage

\ X16 labels

0ffd2 >label ConOut
0febd >label KbdbufPeek
0feab >label ExtApi
09f2c >label BrdCol
\ I'm tentatively removing QtSw & Insrt from the X16 variant;
\ see comment at the top of vf-sys-cbm.fth
\ 0381 >label QtSw
\ 0385 >label Insrt

1 >label RomBank
0 >label RamBank

\ *** Block No. 129, Hexblock 81
81 fthpage

\ X16 x16key? getkey

Code x16key? ( -- flag)
 KbdbufPeek jsr
 txa  pha
 Push jmp  end-code

Code getkey  ( -- 8b)
 GETIN jsr
 Push0A jmp   end-code


\ *** Block No. 131, Hexblock 83
83 fthpage

( #bs #cr ..keyboard         clv12.4.87)

: x16key  ( -- 8b)
 BEGIN pause x16key? UNTIL getkey ;

14 Constant #bs   0D Constant #cr

: x16decode
 ( addr cnt1 key -- addr cnt2)
  #cr case?  IF  dup span ! exit THEN
  >r  2dup +  r> swap c!  1+ ;

Code basin  ( -- 8b)
 CHRIN jsr
 Push0A jmp   end-code

: x16expect ( addr len1 -- )
 span !  0
 BEGIN  dup span @  u<
 WHILE  basin  x16decode
 REPEAT 2drop space ;

Input: keyboard   [ here input ! ]
 x16key x16key? x16decode x16expect ;


include vf-sys-cbm.fth


\ *** Block No. 143, Hexblock 8f
\ ... continued
8f fthpage

Create x16-ink-pot
\ border bkgnd-color-petscii pen-color-petscii
  6 c,   $1f c,  $9f c,  \ Forth


\ *** Block No. 144, Hexblock 90
90 fthpage

\ X16 restore

Label restore   pha txa pha tya pha cld
\ TODO: Replace with phx phy once 65c02 asm is available
  $ffe1 jsr ( stop )  0<> ?[ $e01f jmp ( prend ) ]?
' restart @ jmp  end-code


\ *** Block No. 145, Hexblock 91
91 fthpage

\ X16:Init

: init-system  \ TODO(pzembrod): Check if this works and is needed
 [ restore ] Literal $318 ! ;  \ NMI-Vector

Label first-init
 sei cld
 RomBank lda  $f8 # and  RomBank sta \ map in KERNAL ROM
 IOINIT jsr  CINT jsr  RESTOR jsr  \ init. and set I/O-Vectors
 x16-ink-pot    lda BrdCol sta  \ border
 x16-ink-pot 1+ lda  ConOut jsr  \ backgrnd
 1 # lda  ConOut jsr  \ swap backgrnd <-> pen
 x16-ink-pot 2+ lda  ConOut jsr  \ pen
 $0e # lda  ConOut jsr  \ lower/uppercase
 cli rts end-code
first-init dup bootsystem 1+ !
               warmboot   1+ !
Code c64init first-init jsr
 xyNext jmp end-code

| CODE (bye  $FFFC ) jmp  end-code
