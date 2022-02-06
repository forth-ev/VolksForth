
\ *** Block No. 0, Hexblock 0

\ include for stream sources                         phz 06jan22
















\ *** Block No. 1, Hexblock 1

\ load screen                                        phz 16jan22

  1 5 +thru














\ *** Block No. 2, Hexblock 2

\ fib /fib #fib eolf?                                phz 06jan22

  context @ dos also context !
  $50 constant /tib
  variable tibeof tibeof off

  : eolf? ( c -- f )
    \ f=-1: not yet eol; store c and continue
    \ f=0: eol but not yet eof; return line and flag continue
    \ f=1: eof: return line and flag eof
   tibeof off
    dup #lf = IF drop 0 exit THEN
    -1 = IF tibeof on  1 ELSE -1 THEN ;




\ *** Block No. 3, Hexblock 3

\ freadline probe-for-fb                             phz 06feb22
  variable incfile

  : freadline ( -- eof )
  tib /tib bounds DO
    incfile @ fgetc dup eolf? under 0< IF I c! ELSE drop THEN
    0< 0= IF I tib - #tib ! ENDLOOP tibeof @ exit THEN
  LOOP /tib #tib !
  ." warning: line exteeds max " /tib . cr
  ." extra chars ignored" cr
  BEGIN incfile @ fgetc eolf? 1+ UNTIL tibeof @ ;

| : probe-for-fb  ( -- flag )
    \ probes whether current file looks like a block file
  /tib 2+ 0 DO isfile@ fgetc #lf = IF ENDLOOP false exit THEN
  LOOP true ;

\ *** Block No. 4, Hexblock 4

\ save/restoretib                                    phz 16jan22

  $50 constant /stash
  create stash[  /stash allot  here constant ]stash
  variable stash>   stash[ stash> !

  : savetib  ( -- n )
      #tib @ >in @ -  dup stash> @ + ]stash u>
        abort" tib stash overflow"   >r
      tib >in @ +  stash> @  r@ cmove
      r@ stash> +!  r> ;

  : restoretib  ( n -- )
      dup >r negate stash> +!   stash> @ tib r@ cmove
      r> #tib !  >in off ;


\ *** Block No. 5, Hexblock 5

\ interpret-via-tib include                          phz 06feb22

  : interpret-via-tib
  BEGIN freadline >r .status >in off interpret
  r> UNTIL ;

  : include ( -- )
  pushfile  use  cr file?
  probe-for-fb isfile@ freset IF 1 load close exit THEN
  incfile push  isfile@ incfile !
  savetib >r  interpret-via-tib close  r> restoretib ;

  : (stashquit  stash[ stash> !  (quit ;
  : stashrestore  ['] (stashquit IS 'quit ;
  ' stashrestore IS 'restart


\ *** Block No. 6, Hexblock 6

\ \                                                  phz 16jan22

  : \  blk @ IF >in @ negate  c/l mod  >in +!
       ELSE #tib @ >in ! THEN ; immediate












