
: cells  2* ;

: s"  [compile] " ; immediate

: [char]  [compile] ascii ; immediate

: invert  not ;

: lshift  0 DO 2* LOOP ;

: rshift  0 DO 2/ 32767 and LOOP ;

: 2over  3 pick 3 pick ;
