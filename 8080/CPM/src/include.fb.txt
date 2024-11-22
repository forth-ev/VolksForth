
\ *** Block No. 0, Hexblock 0

\ include for stream sources for cp/m                phz 30aug23
















\ *** Block No. 1, Hexblock 1

\ load screen                                        phz 02sep23

  onlyforth dos also forth definitions

  : idos-error?  ( n -- f )  0<> ;
  : iread-seq  ( dosfcb -- f )  $14 bdosa  idos-error? ;
  : cr+ex@  ( fcb -- cr+256*ex )
    dup &34 + c@  swap &14 + c@ $100 * + ;
  : cr+ex!  ( cr+256*ex fcb -- )
    >r $100 u/mod  r@ &14 + c!  r> &34 + c! ;

  1 7 +thru





\ *** Block No. 2, Hexblock 2

\ fib /fib #fib eolf?                                phz 09okt24

  context @ dos also context !
  \ $50 constant /tib
  variable tibeof tibeof off
  $1a constant ctrl-z

  : eolf? ( c -- f )
    \ f=-1: not yet eol; store c and continue
    \ f=0: eol but not yet eof; return line and flag continue
    \ f=1: eof: return line and flag eof
   tibeof off
    dup #lf = IF drop 0 exit THEN
    ctrl-z = IF tibeof on  1 ELSE -1 THEN ;



\ *** Block No. 3, Hexblock 3

\ incfile incpos inc-fgetc                           phz 02sep23

  variable incfile
  variable increc
  variable rec-offset
  $80 constant dmabuf    | $ff constant dmabuf-last

  : readrec  ( fcb -- f )
    dup cr+ex@ increc !
    rec-offset off  dmabuf dma!  drive iread-seq ;

  : inc-fgetc  ( -- c )
    rec-offset @ b/rec u< 0=
      IF incfile @ readrec IF ctrl-z exit THEN THEN
    rec-offset @ dmabuf + c@  1 rec-offset +! ;


\ *** Block No. 4, Hexblock 4

\ freadline probe-for-fb                             phz 25aug23

  : freadline ( -- eof )
  tib /tib bounds DO
    inc-fgetc dup eolf? under 0< IF I c! ELSE drop THEN
    0< 0= IF I tib - #tib ! ENDLOOP tibeof @ exit THEN
  LOOP /tib #tib !
  ." warning: line exteeds max " /tib . cr
  ." extra chars ignored" cr
  BEGIN inc-fgetc eolf? 1+ UNTIL tibeof @ ;

| : probe-for-fb  ( -- flag )
  dmabuf BEGIN dup c@ #lf = IF drop 0 exit THEN
         1+ dup dmabuf-last u> UNTIL drop 1 ;



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

\ interpret-via-tib inner-include                    phz 02sep23

  : interpret-via-tib
  BEGIN freadline >r .status >in off  interpret  r> UNTIL ;

  : include-inner ( -- )
  increc push  0 isfile@ cr+ex!
  isfile@ readrec Abort" can't read start of file"
  probe-for-fb IF 1 load exit THEN
  incfile push  isfile@ incfile !
  savetib >r  interpret-via-tib close  r> restoretib ;






\ *** Block No. 7, Hexblock 7

\ include                                            phz 02sep23

  : include ( -- )
  rec-offset push  isfile push  fromfile push
  use  cr file?
  include-inner
  incfile @
    IF increc @ incfile @ cr+ex!
    incfile @ readrec Abort" error re-reading after include"
    THEN ;







\ *** Block No. 8, Hexblock 8

\ \                                                  phz 02sep23

  : (stashquit  stash[ stash> !  incfile off  increc off
    (quit ;
  : stashrestore  ['] (stashquit IS 'quit ;
  ' stashrestore IS 'restart

  : \  blk @ IF >in @ negate  c/l mod  >in +!
       ELSE #tib @ >in ! THEN ; immediate

\ : \needs  have 0=exit
\      blk @ IF >in @ negate  c/l mod  >in +!
\      ELSE #tib @ >in ! THEN ;



