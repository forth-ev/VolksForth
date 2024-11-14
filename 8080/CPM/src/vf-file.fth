
$80 .pagestatus

Target Dos also

  : cr+ex@  ( fcb -- cr+256*ex )
    dup &34 + c@  swap &14 + c@ $100 * + ;
  : cr+ex!  ( cr+256*ex fcb -- )
    >r $100 u/mod  r@ &14 + c!  r> &34 + c! ;

| variable tibeof tibeof off
| $1a constant ctrl-z

| : eolf? ( c -- f )
    \ f=-1: not yet eol; store c and continue
    \ f=0: eol but not yet eof; return line and flag continue
    \ f=1: eof: return line and flag eof
   tibeof off
    dup #lf = IF drop 0 exit THEN
    ctrl-z = IF tibeof on  1 ELSE -1 THEN ;

  variable incfile
  variable increc
  variable increc-offset
| $80 constant dmabuf
| $ff constant dmabuf-last

  : increadrec  ( fcb -- f )
    dup cr+ex@ increc !
    increc-offset off  dmabuf dma!  >dosfcb read-seq ;

| : inc-fgetc  ( -- c )
    increc-offset @ b/rec u< 0=
      IF incfile @ increadrec IF ctrl-z exit THEN THEN
    increc-offset @ dmabuf + c@  1 increc-offset +! ;

| : freadline ( -- eof )
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

| $50 constant /stash
| create stash[  /stash allot  here | constant ]stash
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

| : interpret-via-tib
  BEGIN freadline >r .status >in off  interpret  r> UNTIL ;

  Defer include-load
| : block-not-implemented 1 abort" block file access not implemented" ;
  ' block-not-implemented IS include-load

  : include-isfile ( -- )
  increc push  0 isfile@ cr+ex!
  isfile@ increadrec Abort" can't read start of file"
  probe-for-fb IF 1 include-load exit THEN
  incfile push  isfile@ incfile !
  savetib >r  interpret-via-tib  r> restoretib
  incfile @ 2+ closefile Abort" error closing file" ;
