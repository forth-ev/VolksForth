\ Serial I/O for Atari Portfolio
\ send byte via port
: TXP ( c -- )
    %00001011 $8074 pc!  \ DTR, RTS, and OUT2 enabled
    begin
        $8075 pc@ $20 AND \ Test bit 5 (Transmitter Holding Register Empty)
    until
    $8070 PC!
;

: RXP? ( -- f )
    %00001011 $8074 pc!  \ DTR, RTS, and OUT2 enabled
    $8075 pc@ 1 AND \ data ready?
;    

: RXP ( -- c )
    begin
     rxp?
    until
    %00001001 $8074 pc!  \ DTR and OUT2 enabled, RTS disabled
    $8070 pc@ \ read data
;

: ((cr
    (cr $0D txp $0A txp ;

: ((del
    (del 8 TXP ;

: ((emit
    dup (emit txp ;

: ((TYPE
    0 MAX 0 ?DO DUP C@ ((EMIT 1+ LOOP DROP ;

: ((page \ sends VT100 "clear screen escape code"
    (page &27 TXP ASCII [ TXP ASCII 2 TXP ASCII J TXP ;

  Output: serialout
    ((emit ((cr  ((type ((del ((page (at (at? ;

: ((key?
    rxp? or ;

: ((key
    rxp ;

: ((expect ( addr len1 -- )  span !   0
     BEGIN   dup span @ u< WHILE  ((key decode  REPEAT  2drop ;

  Input: serialin
       ((key ((key? (decode ((expect ;

 : ((error ( string -- )   rdrop r> aborted !  
     space here .name   count type space ?cr
     blk @ ?dup IF  scr ! >in @ r# !  THEN  quit ;

 ' (error errorhandler !

 : serialio
     serialout serialin ;
 
\ receive byte via DOS ( timeout )
\ code RXD
\    d push
\    $0300 # a mov
\    $21 int
\    d d xor
\    a- d- mov
\    next
\ end-code

\ send byte via DOS (works)
\ code TXD
\    4 # a+ mov
\    $21 int
\    d pop
\    next
\ end-code

\ recieve via BIOS (does not work)
\ code RX@ ( -- n )
\    d push
\    2 # a+ mov
\    $14 int
\    d d xor
\    a- d- mov
\    d push
\    a+ d- mov
\    Next
\ end-code
