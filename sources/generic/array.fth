\ Arrays with bounds checking

| : (ARRAYERROR 
    ABORT" Array out of bounds!" ; 

: ARRAY ( size -- )
  CREATE DUP , 2* ALLOT
  DOES> ( i -- addr )
  OVER        0< (ARRAYERROR
  2DUP @ 1- - 0> (ARRAYERROR
  SWAP 1+ 2* + ;

: CARRAY ( size -- )
  CREATE DUP , ALLOT
  DOES> ( i -- addr )
  OVER        0< (ARRAYERROR
  2DUP @ 1- - 0> (ARRAYERROR
  + 1+ ;

