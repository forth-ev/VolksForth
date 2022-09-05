

  : dos-error  ( dev -- )
   f busin
   BEGIN bus@ con! i/o-status? UNTIL
   busoff ;

  : lo/hi> ( lo hi -- u )
     ff and 100 * swap ff and + ;


\   fload-dev freadline        25apr20pz

  create fload-dev  8 ,
  create fload-2nd  f ,

| : eolf? ( c -- f )
   \ f=-1: not yet eol; store c and continue
   \ f=0: eol but not yet eof; return line and flag continue
   \ f=1: not eol but eof; store c, return line and flag eof
   dup 0= swap #cr = or IF 0 exit THEN
   i/o-status? IF 1 exit THEN  -1 ;

| : i/o-status?abort  i/o-status? 3 and IF c64cr
   fload-dev @ dos-error abort THEN ;

| : freadline ( -- eof )
 fload-dev @ fload-2nd @ busin
 i/o-status?abort
 tib /tib bounds
 DO bus@ i/o-status?abort dup eolf? under
     IF I c! ELSE drop THEN
 dup 0<
   IF drop ELSE I + tib - #tib ! UNLOOP
   i/o-status? busoff exit THEN
 LOOP /tib #tib !
 ." warning: line exceeds max " /tib .
 cr ." extra chars ignored" cr
 BEGIN bus@ eolf? 1+ UNTIL
 i/o-status? busoff ;


\   fload-open  fload-close    30jun20pz

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

  : interpret-via-tib
 BEGIN freadline >r  .status
 >in off interpret  r> UNTIL ;

  : include ( -- )
 blk @ Abort" no include from blk"
 bl parse  fload-open
   interpret-via-tib
 fload-close
 #tib off >in off ;


  : .filename  2dup cr type ;

  ' .filename IS on-fload
