
  include log2file.fs
  logopen output.log

  Onlyforth   \ \needs Assembler   2 loadfrom asm.fb

  : c+!     ( 8b addr -- )      dup c@ rot + swap c! ;

  ' find $22 + @ Alias found

  : search   ( string 'vocab -- acf n / string ff )
     dup @ [ ' Forth @ ] Literal - Abort" no vocabulary"
     >body (find IF  found exit  THEN  false ;

  use meta.fb
  3 &27 thru  Onlyforth

  logclose
  savesystem metafile.com
  logreopen

  cr .( Metacompiler saved as metafile.com) cr

  logclose
