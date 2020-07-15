\ 2! 2@ 2variable 2constant

cr .( Loading 2words ...)

\needs code INCLUDE" D:TAS65.FTH"

code 2! ( d addr -- )
  tya setup jsr  3 # ldy
  [[ sp )y lda  n )y sta  dey  0< ?]
  1 # ldy  poptwo jmp  end-code

code 2@ ( addr -- d )
  sp x) lda  n sta  sp )y lda  n 1+ sta
  sp 2dec  3 # ldy
  [[ n )y lda  sp )y sta dey  0< ?]
  xyNext jmp  end-code

: 2VARIABLE ( -- )
  create 4 allot ;

: 2CONSTANT ( d -- )
  CREATE , , DOES> ( -- d ) 2@ ;

\ 2dup exists
\ 2swap exists
\ 2drop exists

cr .( 2Words loaded. )
