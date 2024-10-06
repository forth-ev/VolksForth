
\ rom ram                     cas16aug06

\ macros switching the C64 BASIC ROM ($A000-$Bfff)
\ on and off. By default VolksForth runs with BASIC ROM
\ switched off.

Assembler also definitions

\ Can't swich on BASIC ROM (A000-BFFF) if current code
\ location is under the BASIC ROM.
: rom here 9 + $A000 u> abort" not here"
      $37 # lda 1 sta ;
: ram $36 # lda 1 sta ;
