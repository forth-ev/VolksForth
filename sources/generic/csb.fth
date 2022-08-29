( ----- 000 )
\\ Circular String Buffer                            cas 27jul20
 Wil Baden, Costa Mesa, California
 Forth Dimensions July 1996
( ----- 001 )
\ CSB load screen                                    cas 27jul20

 1 3 +thru


 .( Circular Ring Buffer loaded. )
( ----- 002 )
\ Get-Buf   >PAD                                     cas 27jul20

1000 CONSTANT /CSB
CREATE CSB 0 ,  /CSB ALLOT

 : GET-BUF  ( n -- c_addr )
   DUP CSB @ > IF  /CSB CSB !  THEN
   NEGATE CSB +!
   CSB  2+  CSB @ + ;

 : >PAD  ( a u -- 'a u )
   DUP GET-BUF  SWAP
   2DUP  >R >R MOVE  R> R> ;
( ----- 003 )
\ S"                                                 cas 27jul20

  : S"   ( "ccc<quote>" -- | c_addr u )
    ASCII " PARSE
    STATE @ IF
      POSTPONE  SLITERAL
    ELSE
      >PAD
    THEN ;  IMMEDIATE
