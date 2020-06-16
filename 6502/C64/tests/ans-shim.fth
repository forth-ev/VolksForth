
: cells  2* ;

: s"  [compile] " ; immediate

: [char]  [compile] ascii ; immediate

: invert  not ;

: lshift  0 ?DO 2* LOOP ;

: rshift  0 ?DO 2/ 32767 and LOOP ;

: 2over  3 pick 3 pick ;

: s>d  extend ;

: fm/mod  m/mod ;

: sm/rem  dup >r  2dup xor >r  m/mod
    over IF r> 0< IF 1+ swap r> - swap ELSE rdrop THEN
    ELSE rdrop rdrop THEN ;
