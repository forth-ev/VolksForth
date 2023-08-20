
\ *** Block No. 0, Hexblock 0


















\ *** Block No. 1, Hexblock 1

\ log2file loadscreen                                phz 20aug23

  1 3 +thru

\\
  : .1x  ( n -- )  $30 + dup $39 > IF 7 + THEN (emit ;

  : .4x  ( n -- )
    ascii $ (emit 4 0 DO $10 u/mod LOOP drop .1x .1x .1x .1x
    $20 (emit ;

  : .2x  ( n -- )
    ascii $ (emit 2 0 DO $10 u/mod LOOP drop .1x .1x
    $20 (emit ;



\ *** Block No. 2, Hexblock 2

\ logfile                                            phz 20aug23

  Dos also Forth definitions

  $18 constant fcb\nam
  create logfile  ," LOGFILE TXT"  fcb\nam allot  1 logfile c!
  create logdma  b/rec allot
  variable logoffset  0 logoffset !

  : logflush  logdma dma!  logfile $15 bdos  $80 dma! ;

  : logc!  ( c -- )
    logoffset @ dup >r  logdma +  c!  r> 1+
    dup logoffset !  b/rec =
      IF logflush  0 logoffset ! THEN ;


\ *** Block No. 3, Hexblock 3

\ log-emit log-type log-cr alsologtofile            pphz 01jul23

  : log-emit  ( char -- )
      dup (emit  logc! ;

  : log-type  ( addr count -- )
      2dup (type  0 ?DO count logc! LOOP drop ;

  : log-cr  ( -- )
      (cr  #cr logc!  #lf logc! ;

Output: alsologtofile
  log-emit log-cr log-type (del (page (at (at? ;




\ *** Block No. 4, Hexblock 4

\ logopen                                            phz 20aug23

  : logopen  ( -- )
      logfile filenamelen + 1+ fcb\nam erase
      0 logoffset !
      logfile killfile
      logfile createfile
      alsologtofile ;

  : logclose  ( -- )
      cr display  &26 logc!  logflush  logfile closefile ;





