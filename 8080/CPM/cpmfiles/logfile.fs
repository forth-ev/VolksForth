
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

\ log-emit log-type log-cr alsologtofile            pphz 03sep23

  : log-emit  ( char -- )
      dup (emit  logc! ;

  : log-type  ( addr count -- )
      0 ?DO count log-emit LOOP drop ;

  : log-cr  ( -- )
      (cr  #cr logc!  #lf logc! ;

Output: alsologtofile
  log-emit log-cr log-type (del noop 2drop (at? ;


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
