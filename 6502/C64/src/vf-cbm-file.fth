

  : dos-error  ( dev -- )
   f busin
   BEGIN bus@ con! i/o-status? UNTIL
   busoff ;

  : lo/hi> ( lo hi -- u )
     ff and 100 * swap ff and + ;


\   fload-dev freadline        25apr20pz

  create fload-dev  8 ,
  create fload-2nd  f ,
| 84 constant /fib
  create fib /fib allot
  variable #fib

| : eol? ( c -- f )
   dup 0= swap #cr = or IF 0 exit THEN
   i/o-status? IF 1 exit THEN  -1 ;

| : freadline ( -- eof )
 fload-dev @ fload-2nd @ busin
 fib /fib bounds
 DO bus@ dup eol? under
     IF I c! ELSE drop THEN
 dup 0<
   IF drop ELSE I + fib - #fib ! UNLOOP
   i/o-status? busoff exit THEN
 LOOP /fib #fib !
 ." warning: line exceeds max " /fib .
 cr ." extra chars ignored" cr
 BEGIN bus@ eol? 1+ UNTIL
 i/o-status? busoff ;


\   fload-open  fload-close    30jun20pz

| : i/o-status?abort  i/o-status? IF cr
   fload-dev @ dos-error abort THEN ;

  defer on-fload  ' noop is on-fload
| : fload-open ( addr c -- )
 on-fload  fload-dev @
 fload-2nd @ 1- dup fload-2nd !
 busopen bustype
 " ,s,r" count bustype busoff
 i/o-status?abort ;

| : fload-close ( -- )
 fload-dev @ fload-2nd @
 dup 1+ fload-2nd !
 busclose ;

  : factive? ( -- flag )
 fload-2nd @ f < ;

  : fload-close-all ( -- )
 factive? IF f fload-2nd @ DO
   fload-dev @ I busclose  -1 +LOOP
 f fload-2nd ! THEN ;


\   include                    09jun20pz

 create >tib-orig >tib @ ,
 fib >tib !

  : interpret-via-fib
 BEGIN freadline >r  >in off
 #fib @ #tib !  interpret  r> UNTIL ;

  : include ( -- )
 blk @ Abort" no include from blk"
 bl parse  fload-open
   interpret-via-fib
 fload-close
 #tib off >in off ;


  : .filename  2dup cr type ;

  ' .filename IS on-fload
