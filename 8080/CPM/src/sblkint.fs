
Dos definitions

: file-r/w  ( buffer block fcb f  -- f )
    over 0= Abort" no Direct Disk IO supported! "
    >r  dup  (open   2dup in-range r> (r/w ;

\ backup was made visible in vf-blk.fth so no need to peek its address
\ ' (save-buffers >body $0C + @ | Alias backup

| : filebuffer?  ( fcb -- fcb bufaddr/flag )
   prev  BEGIN @ dup WHILE  2dup 2+ @ = UNTIL ;

| : (flushfile  ( fcb -- )  \ flush file buffers
     BEGIN  filebuffer?  ?dup  WHILE
            dup backup  emptybuf  REPEAT  drop ;

' (flushfile is flushfile

Forth definitions

: list  ( n -- )   3 spaces  file? list ;

\ *** Block No. 15, Hexblock f

\ words for viewing                                   UH 10Oct87

Forth definitions

| $200 Constant viewoffset \ max. %512 kB files

: (makeview  ( -- n )  \ calc. view filed for a name
   blk @ dup  0= ?exit
   loadfile @ ?dup IF fileno @ viewoffset * + THEN ;

: (view      ( blk -- blk' ) \ select file and leave block
   dup 0=exit
   viewoffset u/mod  file-link
   BEGIN @ dup WHILE 2dup fileno @ = UNTIL
   !files drop ;        \ not found: direct access


\ *** Block No. 17, Hexblock 11

\ print a list of all buffers                         UH 20Oct86

: .buffers
   prev BEGIN  @ ?dup WHILE  stop? abort" stopped"
           cr dup u. dup 2+ @ dup 1+
           IF ."    Block: " over 4+ @ 5 .r
              ."    File : " [ Dos ] .file
              dup 6 + @ 0< IF ."     updated" THEN
           ELSE ." Buffer empty" drop THEN  REPEAT ;








: loadfrom     ( n -- )
               isfile push  fromfile push  use load   close ;

| : addblock  ( n -- )  \ add block n to file
     dup buffer under   b/blk  bl fill
     isfile@   rec/blk  over filesize +!  false file-r/w
     IF close Abort" disk full!" THEN ;

: more ( n -- )   open >fileend
   capacity swap bounds  ?DO I addblock LOOP close
   open close ;

\ *** Block No. 22, Hexblock 16

\ Status                                              UH 10OCt87


: .blk ( -- )   blk @ ?dup 0=exit
   dup 1 = IF cr file? THEN base push hex ."  Blk " . ?cr ;

' .blk Is .status

' (makeview     Is makeview
' file-r/w      Is r/w

