\NEEDS CODE   INCLUDE" D:TAS65.F"

( Call Machine Routine at "addr" )
( return value is A-Reg and Y-Reg)
HEX
 CODE CALL ( addr -- res )
   4C # lda   n sta
   SP x) lda  n 1+ sta
   SP )y lda  n 2+ sta
   n jsr
   n sta
   n 1+ sty
   00 # ldx
   01 # ldy
   n    lda  SP x) sta
   n 1+ lda  SP )y sta
   next jmp end-code
