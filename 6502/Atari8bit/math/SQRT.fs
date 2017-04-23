\ SQRT

\needs code    INCLUDE" D:TAS65.FS"

CODE D2*  ( D1 - D2)
  2 # LDA SETUP JSR
  N 2+ ASL N 3 + ROL  N ROL N 1+ ROL
  SP 2DEC N 3 + LDA SP )Y STA
  N 2+ LDA SP X) STA
  SP 2DEC N 1+ LDA SP )Y STA
  N LDA SP X) STA
NEXT JMP END-CODE

: DU< &32768 + ROT &32768 + ROT ROT D< ;

| : EASY-BITS  ( N1 -- N2)
   0 DO
    >R D2* D2*  R@ -  DUP 0<
    IF   R@ +   R> 2* 1-
    ELSE        R> 2* 3 +
    THEN
  LOOP ;

| : 2'S-BIT
   >R D2* DUP 0<
   IF    D2* R@ - R> 1+
   ELSE  D2* R@ 2DUP U<
   IF DROP R> 1-  ELSE -  R> 1+  THEN
   THEN ;

| : 1'S-BIT
    >R DUP 0<
    IF 2DROP R> 1+
    ELSE D2* &32768 R@  DU< 0=
    NEGATE R> +
    THEN ;

: SQRT    ( UD1 - U2)
   0 1  8 EASY-BITS
   ROT DROP 6 EASY-BITS
   2'S-BIT 1'S-BIT ;

\ Test
\
\ : XX  
\ &16 * &62500 UM*
\ SQRT 0 <# # # # ASCII . HOLD #S #>
\ TYPE SPACE ;
