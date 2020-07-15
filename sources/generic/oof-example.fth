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
