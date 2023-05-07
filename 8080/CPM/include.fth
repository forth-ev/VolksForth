
\ *** Block No. 0, Hexblock 0

\ include for stream sources for cp/m                phz 10apr23

\ loadscreen content while debugging read-seq esp. dos-error?

  1 +load  \ /tib tibeof eolf?
  create tib  /tib 1+ allot    variable #tib #tib off
  2 3 +thru
  : pushfile r> isfile push fromfile push >r ; restrict
  : iopen ( -- )
  pushfile  use  cr file?
                isfile@ incfile !   b/rec rec-offset c!
  incpos push  incpos off  incpos 2+ dup push off
  0 incfile @ record 1- c! ;
  : iread ( -- )
   freadline cr . cr tib #tib @ type cr ;


\ *** Block No. 1, Hexblock 1

\ load screen                                        phz 06mai23

  onlyforth dos also forth definitions

  : idos-error?  ( n -- f )  0<> ;
  : iread-seq  ( dosfcb -- f )  $14 bdosa  idos-error? ;

  1 6 +thru









\ *** Block No. 2, Hexblock 2

\ fib /fib #fib eolf?                                phz 06okt22

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

\ incfile incpos inc-fgetc                           phz 07mai23

  variable incfile
  variable incpos 2 allot
  create rec-offset 1 allot   $80 constant dmabuf

  : inc-fgetc  ( -- c )
    rec-offset c@ b/rec u< 0= IF dmabuf dma!
      incfile @ drive  iread-seq IF ." eof" -1 exit THEN
      0 rec-offset c! THEN
    rec-offset c@ dup 1+ rec-offset c! dmabuf + c@
           ; \\
    incfile @ f.handle @ 0= IF
      incpos 2@  incfile @  fseek THEN
    incfile @ fgetc
    incpos 2@ 1. d+ incpos 2! ;

\ *** Block No. 4, Hexblock 4

\ freadline probe-for-fb                             phz 06okt22

  : freadline ( -- eof )
  tib /tib bounds DO
    inc-fgetc dup eolf? under 0< IF I c! ELSE drop THEN
    0< 0= IF I tib - #tib ! ENDLOOP tibeof @ exit THEN
  LOOP /tib #tib !
  ." warning: line exteeds max " /tib . cr
  ." extra chars ignored" cr
  BEGIN inc-fgetc eolf? 1+ UNTIL tibeof @ ;

| : probe-for-fb  ( -- flag )
    \ probes whether current file name has no .FTH extension
  isfile@ extension dup @ $dfdf and $5446 =
  swap 2+ c@ $df and $48 = and not ;


\ *** Block No. 5, Hexblock 5

\ save/restoretib                                    phz 06okt22

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

\ interpret-via-tib include                          phz 07mai23
  : xinterpret  tib #tib @ type cr ;
  : interpret-via-tib
  BEGIN freadline >r .status >in off xinterpret
  r> UNTIL ;
  : pushfile r> isfile push fromfile push >r ; restrict
  : xinclude ( -- )
  pushfile  use  cr file? cr
  probe-for-fb                IF 1 load       exit THEN
  incfile push  isfile@ incfile !   b/rec rec-offset c!
  incpos push  incpos off  incpos 2+ dup push off
  0 incfile @ record 1- c!
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









\ *** Block No. 8, Hexblock 8

















