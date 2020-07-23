\ dir dos cat                  09jun20pz
| : dev fload-dev @ ;

: dir  ( -- )
   dev 0 busopen  ascii $ bus! busoff
   dev 0 busin bus@ bus@ 2drop
   BEGIN cr bus@ bus@ 2drop
   i/o-status? 0= WHILE
   bus@ bus@ lo/hi> u.
   BEGIN bus@ ?dup WHILE con! REPEAT
   REPEAT busoff  dev 0 busclose ;

: dos  ( -- )
   bl word count ?dup
      IF dev $f busout bustype
      busoff cr ELSE drop THEN
   dev dos-error ;

: cat   ( -- ) cr
   dev 2 busopen  bl word count bustype busoff
   i/o-status? IF cr dev dos-error abort THEN
   dev 2 busin  BEGIN bus@ con! i/o-status? UNTIL busoff
   dev 2 busclose ;
