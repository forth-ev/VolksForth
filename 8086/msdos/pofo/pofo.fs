
\ Atari Portfolio Systemenhancements and Patches     cas 14feb25

\ ?CR WORDS 
.( Atari Portfolio System Enhancements ) cr
: ?CR col c/row 8 - u> 0=EXIT CR ;

: WORDS [COMPILE] ASCII CAPITAL >R CONTEXT @
        BEGIN @ DUP STOP? 0= AND
        WHILE ?CR DUP 2+ R@ BL = OVER
          1+ C@ R@ = OR
          IF .name space ELSE DROP THEN
        REPEAT DROP RDROP ; IMMEDIATE

: POFOLOGO PAGE ." volksFORTH-83" CR ." Atari Portfolio" CR
  [ errorhandler ] Literal @ errorhandler !
  ['] noop Is 'abort end-trace clearstack standardi/o
  interpret quit ;

 .( loaded ) CR









