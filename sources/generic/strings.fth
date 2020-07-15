\ String Ext.

CR .( loading String extensions )

: S" ASCII " PARSE
  HERE 2DUP ! 1+
  OVER 1+ ALLOT
  2DUP 4 ROLL SWAP ROT
  1+ MOVE SWAP ;

CR .( String extensions loaded )
