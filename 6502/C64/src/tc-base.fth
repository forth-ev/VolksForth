
\ loadfile for the resident part of the target compiler.

' list alias edit

: .blk  ( -)
 blk @ ?dup IF  ."  Blk " u. ?cr  THEN ;

' .blk is .status

hex

2 drive 10 load

