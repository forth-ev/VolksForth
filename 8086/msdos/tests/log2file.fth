
\ *** Block No. 0, Hexblock 0

\ logging to a text file                             phz 03jan22
















\ *** Block No. 1, Hexblock 1

\ load screen                                        phz 04jan22

  1 2 +thru














\ *** Block No. 2, Hexblock 2

\ log-type log-emit log-cr alsologtofile             phz 04jan22
  context @ dos also context !
\ vocabulary log dos also log definitions
  file logfile
  variable logfcb

  : log-type  2dup (type  ds@ -rot logfcb @ lfputs ;

  : log-emit  dup (emit  logfcb @ fputc ;

  : log-cr    (cr  #cr logfcb @ fputc  #lf logfcb @ fputc ;

Output: alsologtofile
 log-emit log-cr log-type (del (page (at (at? ;



\ *** Block No. 3, Hexblock 3

\ logopen logclose                                   phz 11jan22

  : logopen ( -- )
     isfile push
     logfile make  isfile@ dup freset logfcb !
     alsologtofile ;

  : logclose ( -- )  display  logfcb @ fclose ;








