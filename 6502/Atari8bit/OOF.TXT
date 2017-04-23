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

\ Mini OOF Example

\needs class  INCLUDE" D:MINIOOF.F"

CR .( loading OOF Example )

CR .( creating object class "animal" )

object class
  2 var sound
  2 var color 
  2 var kind
  method init
  method say
  method present
end-class animal

CR .( Implementing Methods )

: m-say ( o -- )
  cr ." it says "
  sound @ COUNT TYPE ;

' m-say animal defines say

: m-present ( o -- )
  cr ." This animal is a "
  DUP color @ COUNT TYPE BL EMIT
      kind  @ COUNT TYPE ." !" ;

' m-present animal defines present

: m-init ( say color kind o -- )
  >R
  R@ SOUND ! 
  R@ COLOR ! 
  R> KIND  ! ;

' m-init animal defines init

CR .( creating animal objects )

animal new constant dog
animal new constant cat
animal new constant eagle

CR .( initializing objects )

: S>A DROP 1- ; ( convert string to address )

S" MAMAL"      S>A
S" BLACK"      S>A
S" BARK BARK"  S>A dog init

S" MAMAL"      S>A
S" SILVER"     S>A
S" MEOW MEOW"  S>A cat init

S" BIRD"       S>A
S" BROWN"      S>A
S" ARK ARK"    S>A eagle init

CR .( now lets the objects speak )

CR

dog   present    dog say
cat   present    cat say
eagle present    eagle say

CR .( Finish! )
CR

\ String Ext.

CR .( loading String extensions )

: S" ASCII " PARSE 
  HERE 2DUP ! 1+ 
  OVER 1+ ALLOT     
  2DUP 4 ROLL SWAP ROT           
  1+ MOVE SWAP ;

CR .( String extensions loaded )


\ 2! 2@ 2variable 2constant

cr .( Loading 2words ...) 

\needs code INCLUDE" D:TAS65.F"

code 2! ( d addr -- )
  tya setup jsr  3 # ldy
  [[ sp )y lda  n )y sta  dey  0< ?]
  1 # ldy  poptwo jmp  end-code

code 2@ ( addr -- d )
  sp x) lda  n sta  sp )y lda  n 1+ sta
  sp 2dec  3 # ldy
  [[ n )y lda  sp )y sta dey  0< ?]
  xyNext jmp  end-code

: 2VARIABLE ( -- ) 
  create 4 allot ;

: 2CONSTANT ( d -- )
  CREATE , , DOES> ( -- d ) 2@ ;

\ 2dup exists
\ 2swap exists
\ 2drop exists

cr .( 2Words loaded. )

  

