
\ *** Block No. 132, Hexblock 84
84 fthpage

( con! printable?            clv11.4.87)

Code con!  ( 8b --)   SP X) lda
Label (con!     ConOut jsr    SP 2inc
Label (con!end
\ So far VolksForth switches off quote switch and insert count
\ after every printed character. This introduces a dependency
\ on Kernal variables QtSw and Insrt that are undesirable on the
\ X16 where their addresses may change between Kernal versions.
\ Therefore we'll try how the system behaves without them on the
\ X16. Possibly this isn't needed at all, in the end.
(C64 QtSw stx Insrt stx )
(C16 QtSw stx Insrt stx )
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

Code i/o-status?  ( -- n )
  READST jsr  Push0A jmp  end-code

Variable (drv    0 (drv !

| : disk ( -- dev.no )   (drv @ 8 + ;

' noop alias drvinit
