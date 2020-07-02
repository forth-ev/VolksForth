
: edit  list ;

: .blk  ( -)
 blk @ ?dup IF  ."  Blk " u. ?cr  THEN ;

' .blk is .status

hex

2 drive 10 load

