

\ Experimental code and test for text logs that can be closed
\ and reopened for appending.
\ Already integrated into log2file.fb/.fth
\ Yet to be done: A more permanent test for m+!
\ and an extension of logtest.fb/.fth to also cover the reopen feature.


\  Code +!     ( 16b addr -- )
\     D W mov   A pop   A W ) add   D pop   Next   end-code

  Code m+!     ( 16b addr -- )
     D W mov   W inc  W inc  A pop   A W ) add
     CS ?[  W dec  W dec  W ) inc ]?
     D pop   Next   end-code





\ *** Block No. 2, Hexblock 2

\ log-type log-emit log-cr alsologtofile             phz 04jan22
  context @ dos also context !
\ vocabulary log dos also log definitions
  file logfile
  variable logfcb
  variable logpos 0 ,

  : log-type
     dup logpos m+!
     2dup (type  ds@ -rot logfcb @ lfputs ;

  : log-emit
     1 logpos m+!
     dup (emit  logfcb @ fputc ;

  : log-cr
     2 logpos m+!
     (cr  #cr logfcb @ fputc  #lf logfcb @ fputc ;

Output: alsologtofile
 log-emit log-cr log-type (del (page (at (at? ;



\ *** Block No. 3, Hexblock 3

\ logopen logclose                                   phz 11jan22

  : logopen ( -- )
     isfile push  logpos dup 2+ off off
     logfile make  isfile@ dup freset logfcb !
     alsologtofile ;

  : logclose ( -- )  display  logfcb @ fclose ;

  : logreopen ( -- )
     logfcb @ freset  logpos 2@  logfcb @ fseek
     alsologtofile ;

  logopen output.log
  .( logtest started) cr
  logpos @ cr u. cr
  .( logtest interrupted) cr
  logclose
  logreopen
  create 2v 4 allot
  hex
  12345. 2v 2!
  1 2v m+!
  2v 2@ d. cr
  1ffff. 2v 2!
  1 2v m+!
  2v 2@ d. cr
  .( logtest done) cr
  logclose
