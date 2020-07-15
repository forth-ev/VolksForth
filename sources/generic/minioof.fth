\ Mini-OOF by Bernd Paysan

CR .( loading Mini-OOF ... )

: METHOD ( m v -- m' v )
  CREATE OVER , SWAP 2+ SWAP
  DOES> ( ... o -- ... )
  @ OVER @ + @ EXECUTE ;

: VAR ( m v size -- )
  CREATE OVER , +
  DOES> ( o -- addr )
  @ + ;

: CLASS ( class -- class methods vars )
  DUP 2@ ;

: END-CLASS ( -- class methods vars )
  CREATE HERE >R , DUP ,
  4 ?DO ['] NOOP , 2 +LOOP
  2+ DUP 2+ R> ROT @ 4 /STRING MOVE ;

: DEFINES ( xt class -- )
  ' >BODY @ + ! ;

: NEW ( class -- o )
  HERE OVER @ ALLOT SWAP OVER ! ;

: :: ( class "name" -- )
  ' >BODY @ + @ , ;

CREATE OBJECT 2 , 4 ,

CR .( Mini-OOF loaded. )
