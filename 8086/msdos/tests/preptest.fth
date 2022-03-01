
\ *** Block No. 0, Hexblock 0

\ include file to bundle what test-*.fth need        phz 30jan22
\ on top of kernel.com















\ *** Block No. 1, Hexblock 1

\ loadscreen to prepare kernel.com for test-*.fth    phz 31jan22

  include multi.vid
\ include asm.fb
\ include extend.fb

  : arguments  ( n -- )
      depth 1- > Error" too few params" ;
  : blank  ( addr count -- )  bl fill ;

  include dos.fb
  include include.fb
  include log2file.fb



