\ *** Block No. 58, Hexblock 3a

\ Multitasker               BP 13.9.84 )

\needs Code include trns6502asm.fth

Code stop
 SP 2dec  IP    lda  SP X) sta
          IP 1+ lda  SP )Y sta
 SP 2dec  RP    lda  SP X) sta
          RP 1+ lda  SP )Y sta
 6 # ldy  SP    lda  UP )Y sta
     iny  SP 1+ lda  UP )Y sta
 1 # ldy  tya  clc  UP adc  W sta
 txa  UP 1+ adc  W 1+ sta
 W 1- jmp   end-code

| Create taskpause   Assembler
 $2C # lda  UP X) sta  ' stop @ jmp
end-code

: singletask
 [ ' pause @ ] Literal  ['] pause ! ;

: multitask   taskpause ['] pause ! ;
