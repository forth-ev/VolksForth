
\ *** Block No. 0, Hexblock 0

\ include for stream sources for cp/m                phz 02apr22
















\ *** Block No. 1, Hexblock 1

\ load screen                                        phz 06feb22

  1 6 +thru














\ *** Block No. 2, Hexblock 2

\ fib /fib #fib eolf?                                phz 05apr22

  context @ dos also context !
  $50 constant /tib
  variable tibeof tibeof off
\\
  : eolf? ( c -- f )
    \ f=-1: not yet eol; store c and continue
    \ f=0: eol but not yet eof; return line and flag continue
    \ f=1: eof: return line and flag eof
   tibeof off
    dup #lf = IF drop 0 exit THEN
    -1 = IF tibeof on  1 ELSE -1 THEN ;




\ *** Block No. 3, Hexblock 3

\ incfile incpos inc-fgetc                           phz 05apr22

  variable incfile
  variable incpos 2 allot
\\
  : inc-fgetc  ( -- c )
    incfile @ f.handle @ 0= IF
      incpos 2@  incfile @  fseek THEN
    incfile @ fgetc
    incpos 2@ 1. d+ incpos 2! ;







\ *** Block No. 4, Hexblock 4

\ freadline probe-for-fb                             phz 05apr22

  : freadline ( -- eof )
( tib /tib bounds DO
    inc-fgetc dup eolf? under 0< IF I c! ELSE drop THEN
    0< 0= IF I tib - #tib ! ENDLOOP tibeof @ exit THEN
  LOOP /tib #tib !
  ." warning: line exteeds max " /tib . cr
  ." extra chars ignored" cr
  BEGIN inc-fgetc eolf? 1+ UNTIL tibeof @ ;
) ;
| : probe-for-fb  ( -- flag )
    \ probes whether current file name has no .FTH extension
  isfile@ extension dup @ $5446 = swap 2+ c@ $48 = and not ;



\ *** Block No. 5, Hexblock 5

\ save/restoretib                                    phz 05apr22
\\
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


\ *** Block No. 6, Hexblock 6

\ interpret-via-tib include                          phz 05apr22

( : interpret-via-tib
  BEGIN freadline >r .status >in off interpret
  r> UNTIL ; )
  : pushfile r> isfile push fromfile push >r ; restrict
  : include ( -- )
  pushfile  use  cr file?
  probe-for-fb                IF 1 load       exit THEN
  incfile push  isfile@ incfile ! ; \\
  incpos push  incpos off  incpos 2+ dup push off
  savetib >r  interpret-via-tib close  r> restoretib ;

  : (stashquit  stash[ stash> !  (quit ;
  : stashrestore  ['] (stashquit IS 'quit ;
  ' stashrestore IS 'restart

\ *** Block No. 7, Hexblock 7

\ \                                                  phz 05apr22
\\
  : \  blk @ IF >in @ negate  c/l mod  >in +!
       ELSE #tib @ >in ! THEN ; immediate

\ : \needs  have 0=exit
\      blk @ IF >in @ negate  c/l mod  >in +!
\      ELSE #tib @ >in ! THEN ;








