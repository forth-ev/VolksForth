\ Random Numbers

: RND ( -- n ) \ Random Number 0-255
  $D20A C@ ;

: RANDOM ( n -- 0..n-1 )
  RND $100 * RND + UM* NIP ;


