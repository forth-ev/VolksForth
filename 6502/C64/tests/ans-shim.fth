
: \vf  [compile] \ ;

: cells  2* ;

: s"  [compile] " compile count ; immediate

: [char]  [compile] ascii ; immediate
: char  [compile] ascii ;

: invert  not ;

: lshift  0 ?DO 2* LOOP ;

: rshift  0 ?DO 2/ 32767 and LOOP ;

: 2over  3 pick 3 pick ;

: s>d  extend ;

: fm/mod  m/mod ;

: sm/rem  dup >r  2dup xor >r  m/mod
    over IF r> 0< IF 1+ swap r> - swap ELSE rdrop THEN
    ELSE rdrop rdrop THEN ;

: postpone  ' dup >name c@ $40 and
    IF , ELSE [compile] compile compile , THEN ; immediate

: align ;
: aligned ;
: cell+  2+ ;
: char+  1+ ;
: chars ;

: 2@  dup 2+ @ swap @ ;
: 2!  under ! 2+ ! ;

: recurse  last @ name> , ; immediate

: >number  ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
 BEGIN  dup 0= IF exit THEN
 >r  count digit? WHILE accumulate r> 1- REPEAT 1- r> ;

: accept  expect span @ ;
