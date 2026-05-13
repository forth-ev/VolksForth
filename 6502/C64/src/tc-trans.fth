\ Transient part of the target compiler
\ The part that is loaded anew each time before
\ a VolksForth kernel is built.

\ *** Block No. 39, Hexblock 27

\ clear Liter. Ascii ['] ."  bp27jun85we

Onlyforth Transient definitions

: clear  True abort" There are ghosts" ;

: Literal  ( n -)  H dup $FF00 and
 IF T compile lit ,
 ELSE compile clit c, H THEN ; immediate

: Ascii  H BL word 1+ c@
 State @
  IF T [compile]  Literal H THEN ;
 immediate

: [']     T ' [compile] Literal H ;
          immediate restrict

: "       T compile ("   ," H  ;
          immediate restrict

: ."      T compile (."  ," H  ;
          immediate restrict



\ *** Block No. 40, Hexblock 28

\ Target compilation  ]  [   bp03jul85we

Forth definitions

: tcompile
 ?stack  >in @
 name find ?dup
 IF 0> IF nip execute   >interpret THEN
    drop dup >in !  name THEN
 gfind IF nip execute   >interpret THEN
 nullstring? IF drop exit THEN
 number? ?dup
  IF 0> IF swap T [compile] Literal THEN
      [compile] Literal H drop
      >interpret THEN
 drop >in ! Word,
 >interpret ; -2 allot

Transient definitions

: ]    H  State on
 ['] tcompile is >interpret ;




\ *** Block No. 41, Hexblock 29

\ Target conditionals        bp27jun85we

: IF      T compile ?branch >mark H 1 ;
          immediate restrict
: THEN    abs 1 T ?pairs >resolve H ;
          immediate restrict
: ELSE    T 1 ?pairs compile branch
          >mark swap >resolve H -1 ;
          immediate restrict
: BEGIN   T <mark H 2 ;
          immediate restrict
: WHILE   T 2 ?pairs  2 compile ?branch
          >mark -2  H 2swap ;
          immediate restrict

| : (repeat  T 2 ?pairs  <resolve H
 BEGIN dup -2 = WHILE drop T >resolve H
 REPEAT ;

: UNTIL   T compile ?branch (repeat H  ;
          immediate restrict
: REPEAT  T compile branch  (repeat H  ;
          immediate restrict



\ *** Block No. 42, Hexblock 2a

\ Target conditionals        bp27jun85we

: DO     T compile (do  >mark H 3 ;
         immediate restrict
: ?DO    T compile (?do >mark H 3 ;
         immediate restrict
: LOOP   T 3 ?pairs  compile (loop
         compile endloop >resolve H ;
         immediate restrict
: +LOOP  T 3 ?pairs  compile (+loop
         compile endloop >resolve H ;
         immediate restrict














\ *** Block No. 43, Hexblock 2b

\ predefinitions             bp27jun85we

: abort"    T compile (abort" ," H ;
 immediate

: error"    T compile (err"   ," H ;
 immediate




Forth definitions

Variable torigin
Variable tudp       0 tudp !

: >user  T c@ H torigin @ + ;









\ *** Block No. 44, Hexblock 2c

\ Datatypes                  bp27jun85we

Transient definitions

: origin!  H torigin !  ;

: user' ( - 8b)   T ' 2 + c@ H ;

: uallot  ( n -) H tudp @ swap tudp +! ;


        Do> >user ;
: User  prebuild User  2 T uallot c, ;

           Do> ;
: Create   prebuild Create  ;










\ *** Block No. 45, Hexblock 2d

\ Datatypes                  bp27jun85we

           Do> T @ H ;
: Constant prebuild Constant  T , ;

: Variable Create 2 T allot ;


 dummy
: Vocabulary
 H >in @  Vocabulary  >in !
 T prebuild Vocabulary  0 , 0 ,
 here H tvoc-link @ T ,  H tvoc-link ! ;

          Do>  ;
: Defer   prebuild Defer 2 T allot ;

: Is  T ' H >body  State @
   IF T compile (is  ,
 H ELSE T ! H THEN  ;   immediate






\ *** Block No. 46, Hexblock 2e

\ target defining words      bp27jun85we

| : dodoes>
 T compile (;code
 H Glast' @  there  resdoes>
 there tdoes> ! ;

: ;code 0 T ?pairs dodoes>  Assembler
 H [compile] [  redefinition ;
 immediate restrict

: does>
 T dodoes>
 $4C c, compile (dodoes> H ;
 immediate restrict

 dummy
: :  H  tdoes> off  >in @ >in: !
 T prebuild : H current @ context !
 T ] H 0 ;






\ *** Block No. 47, Hexblock 2f

\ :  Alias  ;                  02oct87re

: Alias ( n -- )  H Tlast off
 (theader  Ghost  over resolve
 tlast @ T c@ H 20 or tlast @ T c! ,
 H ;

: ;  T 0 ?pairs
 compile unnest [compile] [
 H redefinition  ;
 immediate  restrict

 dummy
: Input:  H  tdoes> off  >in @ >in: !
 T prebuild Input:
 H current @ context !  T ] H 0 ;

 dummy
: Output:  H  tdoes> off  >in @ >in: !
 T prebuild Output:
 H current @ context !  T ] H 0 ;





\ *** Block No. 48, Hexblock 30

\ predefinitions             bp03jul85we

: compile   T compile compile H ;
            immediate  restrict

: Host
 H Onlyforth Ttools also ;

: Compiler
 T Host H Transient also definitions ;

: [compile] H Word, ; immediate restrict

: Onlypatch H there 3 - 0 tdoes> ! 0 ;

Onlyforth

: Target
 Onlyforth Transient also definitions ;


Transient definitions  Ghost c, drop
