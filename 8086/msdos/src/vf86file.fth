
| variable tibeof tibeof off

| : eolf? ( c -- f )
    \ f=-1: not yet eol; store c and continue
    \ f=0: eol but not yet eof; return line and flag continue
    \ f=1: eof: return line and flag eof
   tibeof off
    dup #lf = IF drop 0 exit THEN
    -1 = IF tibeof on  1 ELSE -1 THEN ;




\ *** Block No. 3, Hexblock 3

\ incfile incpos inc-fgetc                           phz 06feb22

  variable incfile
  variable incpos 2 allot

| : inc-fgetc  ( -- c )
    incfile @ f.handle @ 0= IF
      incpos 2@  incfile @  fseek THEN
    incfile @ fgetc
    incpos 2@ 1. d+ incpos 2! ;







\ *** Block No. 4, Hexblock 4

\ freadline probe-for-fb                             phz 06feb22

| : freadline ( -- eof )
  tib /tib bounds DO
    inc-fgetc dup eolf? under 0< IF I c! ELSE drop THEN
    0< 0= IF I tib - #tib ! ENDLOOP tibeof @ exit THEN
  LOOP /tib #tib !
  ." warning: line exteeds max " /tib . cr
  ." extra chars ignored" cr
  BEGIN inc-fgetc eolf? 1+ UNTIL tibeof @ ;

| : probe-for-fb  ( -- flag )
    \ probes whether current file looks like a block file
  /tib 2+ 0 DO isfile@ fgetc #lf = IF ENDLOOP false exit THEN
  LOOP true ;


\ *** Block No. 5, Hexblock 5

\ save/restoretib                                    phz 16jan22

| $50 constant /stash
| create stash[  /stash allot  here constant ]stash
| variable stash>   stash[ stash> !
| : clear-tibstash  stash[ stash> ! ;

| : savetib  ( -- n )
      #tib @ >in @ -  dup stash> @ + ]stash u>
        abort" tib stash overflow"   >r
      tib >in @ +  stash> @  r@ cmove
      r@ stash> +!  r> ;

| : restoretib  ( n -- )
      dup >r negate stash> +!   stash> @ tib r@ cmove
      r> #tib !  >in off ;


\ *** Block No. 6, Hexblock 6

\ interpret-via-tib include                          phz 06feb22

| : interpret-via-tib
  BEGIN freadline >r .status >in off interpret
  r> UNTIL ;

  Defer include-load
| : block-not-implemented 1 abort" block file access not implemented" ;
  ' block-not-implemented IS include-load

  : include ( -- )
  pushfile  use  cr file?
  probe-for-fb isfile@ freset IF 1 include-load close exit THEN
  incfile push  isfile@ incfile !
  incpos push  incpos off  incpos 2+ dup push off
  savetib >r  interpret-via-tib close  r> restoretib ;
