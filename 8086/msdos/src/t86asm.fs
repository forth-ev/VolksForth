
\ *** Block No. 2, Hexblock 2

\ conditional Assembler compiler                     cas 10nov05
  here

  : maybe-include-tmp-asm  ( addr -- )  hide  last off  dp !
     " ASSEMBLER"  find nip ?exit   here $1800 + sp@ u>
     IF  display cr ." Assembler won't fit" abort  THEN
     here   sp@ $1800 - dp !
     include
     dp ! ;

  maybe-include-tmp-asm 86asm.fs
