
hex

2 drive 27 30 thru

1 drive

Onlyforth hex
  c    load   \ clear memory and
  d e  thru   \ clr labels  .status

\ *** Block No. 9, Hexblock 9

\ Target-Machine              clv06dec88

cr .( Host is: )
    (64  .( C64) C)
    (16  .( C16) C)

       : )     ; immediate
       : (C    ; immediate

\      : (C64  ; immediate
       : (C16  ; immediate
       : (C16+ ; immediate
\      : (C16- ; immediate

       : (C64  [compile] ( ; immediate
\      : (C16  [compile] ( ; immediate
\      : (C16+ [compile] ( ; immediate
       : (C16- [compile] ( ; immediate

\ *** Block No. 10, Hexblock a

\ load/remove  JSR-Macros    clv14.4.87)

Assembler also definitions

\needs C16+Jsr          8 load
' C16+Jsr Is Jsr .( JSR Is:C16+  )


include vf-main.fth
