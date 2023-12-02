
$0001 | constant RomBank
create xed-banks  1 c, $ff c,  \ first and last RAM bank xed uses.

| Code (xed?  ( -- f )
  $4c # lda  $c000 cmp 0= ?[ $c003 cmp 0= ?[ $c006 cmp 0= ?[
    Push0A jmp ]? ]? ]?  txa Push0A jmp  end-code

| Code (xed  ( -- )
  xed-banks ldx  xed-banks 1+ ldy  $c003 jsr
  xyNext jmp  end-code

  : xed  ( -- )
   bl word count $4 ! $2 !  RomBank c@  $d RomBank c!
   (xed? IF (xed  RomBank c! ELSE RomBank c! ." no x16edit" THEN ;
