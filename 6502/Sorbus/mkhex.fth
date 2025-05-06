( forth version of the mkhex tool                     )
( creates a hex file to load into the Apple 1 monitor )
( usage: gforth mkhex.fs > f6502.hex                  )

: read-image ( file-id -- addr size )
  dup file-size drop d>s              ( file-id size ) 
  dup allocate drop dup >r            ( file-id size addr )
  swap rot read-file drop r> swap     ( addr size )
;

: find-end ( end-addr -- new-end-addr )
  begin dup @ 0= while 1- repeat 
;

: dump16 ( addr len -- )
  over + swap ?do I c@ 3 .r loop ;

: dump-hex ( end begin -- )
  hex ." 300:" 
  do I $10 dump16 cr ." :" $10 +loop
;

S" f6502.com" r/o open-file drop read-image 
over + find-end cr swap dump-hex

bye
