
\ *** Block No. 8, Hexblock 8
8 fthpage

\ ram rom jsr NormJsr f.C16+ clv12.4.87)

Assembler also definitions

\ C16+Macros for Bankswitching

: ram $ff3f sta ;   : rom $ff3e sta ;

' Jsr Alias NormJsr   Defer Jsr

: C16+Jsr dup $c000 u>
 IF rom NormJsr ram ELSE NormJsr THEN ;

' C16+Jsr Is Jsr
