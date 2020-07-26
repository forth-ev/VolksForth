CR
.( List Directory Command for volksForth )
-1 ?HEAD ! ( move head of DIRX in Heap )
: DIRX
  &6 OPEN-FILE DUP
  $80 > IF ." File Error:" . ABORT THEN
  DROP SOURCE-ID ! CR
  BEGIN $580 &18 SOURCE-ID @ READ-LINE
  $80 < WHILE
    DROP $580 SWAP TYPE
  REPEAT 2DROP
  SOURCE-ID @ CLOSE-FILE DROP CR ;

( Generic Directory listing for )
( current directory )
: DIR " D:*.*" COUNT DIRX ;

( Directory Listing with Parameter )
( Example: DIR" D2:*.COM"
: DIR" FILE" DIRX ;

CR .( DIR and DIR" Command loaded )
CR
