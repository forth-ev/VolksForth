
\ *** Block No. 0, Hexblock 0














ende 123



\ *** Block No. 1, Hexblock 1

\ volksFORTH Loadscreen for py65 target         cas    02aug2020
forth definitions
: (C [compile] ( ; IMMEDIATE  \ : ) ; IMMEDIATE

$1000 CONSTANT BASEADDR       \ change target base address here
BASEADDR DISPLACE !

TARGET DEFINITIONS BASEADDR HERE!

hex &01 &126  +THRU
decimal
\ ASSEMBLER NONRELOCATE

.UNRESOLVED                  \ if this prints unresolved
                             \ definitions, check code
CR .( SAVE-TARGET 6502-FORTH83)

\ *** Block No. 2, Hexblock 2

\ FORTH PREAMBLE AND ID                              cas 26jan06


ASSEMBLER
  NOP  0 JMP  HERE 2- >LABEL >COLD
  NOP  0 JMP  HERE 2- >LABEL >RESTART

HERE DUP ORIGIN!









\ *** Block No. 3, Hexblock 3

\ Coldstartvalues and user variables            cas    02aug2020
\

0 JMP  0 JSR  HERE 2- >LABEL >WAKE
 END-CODE

0D6 ALLOT

\ Bootlabel
," VolksForth-83 3.8.1 py65 02aug2020  CS"







\ *** Block No. 4, Hexblock 4

\ ZERO PAGE VARIABLES & NEXT                         cas 26jan06
\ adjust this to match your architecture


20 DUP     >LABEL RP     2+
   DUP     >LABEL UP     2+
   DUP     >LABEL PUTA   1+
   DUP     >LABEL SP     2+
   DUP     >LABEL NEXT
   DUP 5 + >LABEL IP
      13 + >LABEL W

     W 8 + >LABEL N




\ *** Block No. 5, Hexblock 5

\ NEXT, MOVED INTO ZERO PAGE  08APR85BP)

LABEL BOOTNEXT
   -1 STA              \ -1 IS DUMMY SP
   IP )Y LDA  W 1+  STA
   -1 LDA     W STA    \ -1 IS DUMMY IP
   CLC IP LDA  2 # ADC  IP STA
     CS NOT ?[ LABEL WJMP -1 ) JMP ]?
   IP 1+ INC  WJMP BCS END-CODE








\ *** Block No. 6, Hexblock 6

\  Bootnext and Endtrace                             cas 26jan06
HERE BOOTNEXT - >LABEL BOOTNEXTLEN

CODE END-TRACE  ( PATCH NEXT FOR TRACE )
 0A5 # LDA  NEXT 0A + STA
  IP # LDA  NEXT 0B + STA
 069 # LDA  NEXT 0C + STA
  02 # LDA  NEXT 0D + STA
 NEXT JMP   END-CODE








\ *** Block No. 7, Hexblock 7

\ ;C:  NOOP                                          cas 26jan06

CREATE RECOVER  ASSEMBLER
 PLA  W STA  PLA  W 1+ STA
 W WDEC  0 JMP   END-CODE

HERE 2- >LABEL >RECOVER
\ manual forward reference for JMP command


COMPILER ASSEMBLER ALSO DEFINITIONS
 H : ;C:   0 T RECOVER JSR
 END-CODE  ] H ;
TARGET
CODE NOOP   NEXT HERE 2- !  END-CODE


\ *** Block No. 8, Hexblock 8

\ USER VARIABLES                                     cas 26jan06

CONSTANT ORIGIN  8 UALLOT DROP
                 \ FOR MULTITASKER

\ Adjust memory values for data stack and return stack here
USER S0      $5000 S0 !         USER R0      $5500 R0 !
USER DP                         USER OFFSET      0 OFFSET !
USER BASE      &10 BASE !       USER OUTPUT
USER INPUT
USER ERRORHANDLER               \ POINTER FOR  ABORT" -CODE
USER VOC-LINK
USER UDP                \ POINTS TO NEXT FREE ADDR IN USER




\ *** Block No. 9, Hexblock 9

\ MANIPULATE SYSTEM POINTERS  29JAN85BP)        cas    02aug2020

CODE SP@   ( -- ADDR)
 SP LDA  N STA  SP 1+ LDA  N 1+ STA
 N # LDX
LABEL XPUSH
 SP 2DEC  1 ,X LDA  SP )Y STA
 0 ,X LDA  0 # LDX  PUTA JMP   END-CODE

CODE SP!   ( ADDR --)
 SP X) LDA  TAX  SP )Y LDA
 SP 1+ STA  SP STX   0 # LDX
 NEXT JMP   END-CODE




\ *** Block No. 10, Hexblock a

\  UP@ UP!  XPULL (XYDROP (DROP                      cas 26jan06
CODE UP@   ( -- ADDR)
 UP # LDX  XPUSH JMP  END-CODE

CODE UP!   ( ADDR --)      UP # LDX
LABEL XPULL     SP )Y LDA  1 ,X STA
            DEY SP )Y LDA  0 ,X STA
LABEL (XYDROP   0 # LDX  1 # LDY
LABEL (DROP     SP 2INC  NEXT JMP
END-CODE RESTRICT







\ *** Block No. 11, Hexblock b

\ MANIPULATE RETURNSTACK   16FEB85BP/KS)
CODE RP@ ( -- ADDR )
 RP # LDX  XPUSH JMP  END-CODE

CODE RP! ( ADDR -- )
 RP # LDX  XPULL JMP  END-CODE RESTRICT

CODE >R  ( 16B --  )
 RP 2DEC  SP X) LDA   RP X) STA
 SP )Y LDA   RP )Y STA (DROP JMP
END-CODE RESTRICT






\ *** Block No. 12, Hexblock c

\  R> (RDROP (NRDROP                                 cas 26jan06
CODE R>  ( -- 16B)
 SP 2DEC  RP X) LDA  SP X) STA
          RP )Y LDA  SP )Y STA
LABEL (RDROP     2 # LDA

LABEL (NRDROP    CLC  RP ADC  RP STA
    CS ?[  RP 1+ INC  ]?
 NEXT JMP  END-CODE  RESTRICT








\ *** Block No. 13, Hexblock d

\ R@ RDROP  EXIT  ?EXIT       08APR85BP)

CODE R@      ( -- 16B)
 SP 2DEC  RP )Y LDA  SP )Y STA
          RP X) LDA  PUTA JMP
END-CODE
CODE RDROP    (RDROP HERE 2- !
END-CODE   RESTRICT

CODE EXIT
 RP X) LDA  IP STA
 RP )Y LDA  IP 1+ STA
 (RDROP JMP   END-CODE




\ *** Block No. 14, Hexblock e

\ EXECUTE  PERFORM            08APR85BP)

CODE ?EXIT     ( FLAG -- )
 SP X) LDA  SP )Y ORA
 PHP  SP 2INC  PLP
 ' EXIT @ BNE  NEXT JMP
END-CODE

CODE EXECUTE  ( ADDR --)
 SP X) LDA   W STA
 SP )Y LDA   W 1+ STA
 SP 2INC     W 1- JMP   END-CODE

: PERFORM ( ADDR -- )   @ EXECUTE ;



\ *** Block No. 15, Hexblock f

\ C@   C! CTOGGLE             10JAN85BP)

CODE C@ ( ADDR -- 8B)

 SP X) LDA  N STA   SP )Y LDA  N 1+ STA
LABEL (C@    0 # LDA  SP )Y STA
 N X)  LDA   PUTA JMP   END-CODE

CODE C!  ( 16B ADDR --)
 SP X) LDA   N STA   SP )Y LDA  N 1+ STA
 INY  SP )Y LDA  N X) STA DEY
LABEL (2DROP
 SP LDA  CLC  4 # ADC  SP STA
   CS ?[  SP 1+ INC  ]?
 NEXT JMP   END-CODE


\ *** Block No. 16, Hexblock 10

\ @ ! +!                      08APR85BP)               er14dez88

: CTOGGLE   ( 8B ADDR --) UNDER C@ XOR SWAP C! ;

CODE @  ( ADDR -- 16B)
 SP X) LDA  N STA  SP )Y LDA  N 1+ STA
 N )Y LDA  SP )Y STA
 N X) LDA PUTA JMP   END-CODE

CODE !   ( 16B ADDR --)
 SP X) LDA  N STA  SP )Y LDA  N 1+ STA
 INY SP )Y LDA  N X) STA
 INY SP )Y LDA   1 # LDY
LABEL (!
 N )Y STA  (2DROP JMP   END-CODE


\ *** Block No. 17, Hexblock 11

\ +!   DROP                                          cas 26jan06

CODE +!  ( N ADDR --)
 SP X) LDA  N STA  SP )Y LDA  N 1+ STA
 INY  SP )Y LDA  CLC  N X) ADC N X) STA
 INY  SP )Y LDA  1 # LDY  N )Y ADC
 (! JMP   END-CODE

CODE DROP  ( 16B --)
 (DROP HERE 2- !  END-CODE







\ *** Block No. 18, Hexblock 12

\ SWAP                                               cas 26jan06
CODE SWAP  ( 16B1 16B2 -- 16B2 16B1 )
 SP )Y LDA  TAX
 3 # LDY  SP )Y LDA  N STA
 TXA  SP )Y STA
 N LDA  1 # LDY  SP )Y STA
 INY  0 # LDX
 SP )Y LDA  N STA  SP X) LDA  SP )Y STA
 DEY
 N LDA PUTA JMP   END-CODE







\ *** Block No. 19, Hexblock 13

\ DUP  ?DUP                   08MAY85BP)             cas 26jan06

CODE DUP   ( 16B -- 16B 16B)
 SP 2DEC
 3 # LDY  SP )Y LDA  1 # LDY  SP )Y STA
 INY  SP )Y LDA  DEY
 PUTA JMP   END-CODE

CODE ?DUP     ( 16B -- 16B 16B / FALSE)
 SP X) LDA  SP )Y ORA
   0= ?[  NEXT JMP  ]?
 ' DUP @ JMP END-CODE
\\ ?DUP and DUP in FORTH
\   : ?DUP   ( 16B -- 16B 16B / FALSE)
\    DUP  IF  DUP  THEN ;
\   : DUP    SP@  @  ;

\ *** Block No. 20, Hexblock 14

\ OVER ROT                    13JUN84KS)             cas 26jan06

CODE OVER  ( 16B1 16B2 - 16B1 16B3 16B1)
 SP 2DEC  4 # LDY  SP )Y LDA  SP X) STA
 INY  SP )Y LDA  1 # LDY  SP )Y STA
 NEXT JMP   END-CODE

\\ ROT OVER in FORTH
\   : ROT   >R SWAP R> SWAP ;
\   : OVER  >R DUP R> SWAP ;







\ *** Block No. 21, Hexblock 15

\ ROT                                                cas 26jan06
CODE ROT ( 16B1 16B2 16B3 -- 16B2 16B3 16B1)
 3 # LDY  SP )Y LDA  N 1+ STA
 1 # LDY  SP )Y LDA  3 # LDY  SP )Y STA
 5 # LDY  SP )Y LDA  N STA
 N 1+ LDA  SP )Y STA
 1 # LDY  N LDA  SP )Y STA
 INY  SP )Y LDA  N 1+ STA
 SP X) LDA  SP )Y STA
 4 # LDY  SP )Y LDA  SP X) STA
 N 1+ LDA  SP )Y STA
 1 # LDY  NEXT JMP   END-CODE





\ *** Block No. 22, Hexblock 16

\ -ROT NIP UNDER PICK ROLL    24DEC83KS)             cas 26jan06
: -ROT ( 16B1 16B2 16B3 -- 16B3 16B1 16B2)
        ROT ROT ;

: NIP   ( 16B1 16B2 -- 16B2) SWAP DROP ;

: UNDER ( 16B1 16B2 -- 16B2 16B1 16B2) SWAP OVER ;

: PICK  ( N -- 16B.N )   1+ 2* SP@ + @ ;

: ROLL  ( N --) DUP >R PICK SP@ DUP 2+ R> 1+ 2* CMOVE>  DROP ;

\\ : -ROLL ( N --)
 >R DUP SP@  DUP 2+ DUP 2+ SWAP
 R@ 2* CMOVE R> 1+ 2* + ! ;


\ *** Block No. 23, Hexblock 17

\ DOUBLE WORD STACK MANIP.    21APR83KS)

: 2SWAP ( 32B1 32B2 -- 32B2 32B1) ROT >R ROT R> ;

CODE 2DROP ( 32B -- )
 (2DROP HERE 2- !   END-CODE

: 2DUP  ( 32B -- 32B 32B) OVER OVER ;

\  : 2DROP ( 32B -- )     DROP DROP ;







\ *** Block No. 24, Hexblock 18

\ + AND OR XOR                08APR85BP)
COMPILER  ASSEMBLER ALSO DEFINITIONS

H : DYADOP ( OPCODE --)  T
   INY  SP X) LDA  DUP C, SP C, SP )Y STA
   DEY  SP )Y LDA  3 # LDY  C, SP C, SP )Y STA
   (XYDROP JMP  H ;
TARGET

CODE +     ( N1 N2 -- N3) CLC     071 DYADOP   END-CODE

CODE OR    ( 16B1 16B2 -- 16B3)   011 DYADOP   END-CODE

CODE AND   ( 16B1 16B2 -- 16B3)   031 DYADOP   END-CODE

CODE XOR   ( 16B1 16B2 -- 16B3)   051 DYADOP   END-CODE

\ *** Block No. 25, Hexblock 19

\ -  NOT  NEGATE              24DEC83KS)

CODE -    ( N1 N2 -- N3)
 INY SP )Y LDA  SEC SP X) SBC SP )Y STA INY SP )Y LDA
 1 # LDY  SP )Y SBC  3 # LDY  SP )Y STA (XYDROP JMP   END-CODE

CODE NOT   ( 16B1 -- 16B2)   CLC
LABEL (NOT TXA  SP X) SBC  SP X) STA  TXA SP )Y SBC  SP )Y STA
       NEXT JMP   END-CODE

CODE NEGATE   ( N1 -- N2 ) SEC  (NOT BCS   END-CODE

\ : -       NEGATE + ;




\ *** Block No. 26, Hexblock 1a

\ DNEGATE SETUP D+            14JUN84KS)

CODE DNEGATE ( D1 -- -D1)
 INY  SEC
 TXA  SP )Y SBC  SP )Y STA  INY
 TXA  SP )Y SBC  SP )Y STA
 TXA  SP X) SBC  SP X) STA  1 # LDY
 TXA  SP )Y SBC  SP )Y STA
 NEXT JMP  END-CODE
LABEL SETUP  ( QUAN  IN A)
 .A ASL TAX TAY  DEY
    [[ SP )Y LDA  N ,Y STA  DEY  0< ?]
 TXA  CLC  SP ADC  SP STA
    CS ?[  SP 1+ INC  ]?
 0 # LDX  1 # LDY   RTS   END-CODE


\ *** Block No. 27, Hexblock 1b

\ D+                                                 cas 26jan06
CODE D+      ( D1 D2 -- D3)
 2 # LDA  SETUP JSR  INY
 SP )Y LDA  CLC N 2+  ADC SP )Y STA INY
 SP )Y LDA      N 3 + ADC SP )Y STA
 SP X) LDA  N    ADC SP X) STA  1 # LDY
 SP )Y LDA  N 1+ ADC SP )Y STA
 NEXT JMP   END-CODE









\ *** Block No. 28, Hexblock 1c

\ 1+ 2+ 3+    1- 2-           08APR85BP)

CODE 1+   ( N1 -- N2)   1 # LDA
LABEL N+  CLC  SP X) ADC
 CS NOT   ?[  PUTA JMP  ]?
 SP X) STA  SP )Y LDA  0 # ADC SP )Y STA
 NEXT JMP  END-CODE

CODE 2+   ( N1 -- N2) 2 # LDA   N+ BNE     END-CODE

CODE 3+   ( N1 -- N2) 3 # LDA   N+ BNE     END-CODE

| CODE 4+ ( N1 -- N2) 4 # LDA   N+ BNE     END-CODE

| CODE 6+ ( N1 -- N2) 6 # LDA   N+ BNE     END-CODE


\ *** Block No. 29, Hexblock 1d

\ NUMBER CONSTANTS            24DEC83KS)
CODE 1-   ( N1 -- N2)   SEC
LABEL (1-     SP X) LDA  1 # SBC
   CS ?[ PUTA JMP ]?
 SP X) STA  SP )Y LDA  0 # SBC SP )Y STA
 NEXT JMP  END-CODE
CODE 2-   ( N1 -- N2)  CLC (1- BCC  END-CODE

-1 CONSTANT TRUE    0 CONSTANT FALSE
' TRUE ALIAS -1     ' FALSE ALIAS 0

1 CONSTANT 1        2 CONSTANT 2
3 CONSTANT 3        4 CONSTANT 4

: ON    ( ADDR -- )  TRUE  SWAP ! ;
: OFF   ( ADDR -- )  FALSE SWAP ! ;

\ *** Block No. 30, Hexblock 1e

\ WORDS FOR NUMBER LITERALS   24MAY84KS)               cs08aug05

CODE CLIT  ( -- 8B)
 SP 2DEC  IP X) LDA  SP X) STA TXA   SP )Y STA  IP WINC
 NEXT JMP   END-CODE RESTRICT

CODE LIT   ( -- 16B)
 SP 2DEC  IP )Y LDA  SP )Y STA IP X) LDA  SP X) STA
LABEL (BUMP   IP 2INC NEXT JMP  END-CODE RESTRICT
: LITERAL   ( 16B --) DUP 0FF00 AND
   IF  COMPILE LIT   , EXIT THEN COMPILE CLIT C,  ;
                                         IMMEDIATE RESTRICT

\\ : LIT     R> DUP 2+ >R  @  ;
   : CLIT    R> DUP 1+ >R  C@ ;


\ *** Block No. 31, Hexblock 1f

\ COMPARISION CODE WORDS      13JUN84KS)
CODE 0<   ( N -- FLAG) SP )Y LDA  0< ?[
     LABEL PUTTRUE    0FF # LDA  024 C, ]?
     LABEL PUTFALSE   TXA  SP )Y STA
                      PUTA JMP   END-CODE

CODE 0=   ( 16B -- FLAG)
 SP X) LDA  SP )Y ORA PUTTRUE  BEQ PUTFALSE BNE  END-CODE

CODE UWITHIN  ( U1 [LOW UP[  -- FLAG)
 2 # LDA  SETUP JSR 1 # LDY  SP X) LDA  N CMP
          SP )Y LDA  N 1+ SBC
  CS NOT ?[ ( N>SP) SP X) LDA N 2+  CMP
                    SP )Y LDA N 3 + SBC
           PUTTRUE BCS ]?
 PUTFALSE JMP  END-CODE

\ *** Block No. 32, Hexblock 20

\ COMPARISION CODE WORDS      13JUN84KS)

CODE <    ( N1 N2 -- FLAG)
 SP X) LDA  N STA  SP )Y LDA  N 1+ STA
 SP 2INC
 N 1+ LDA  SP )Y EOR  ' 0< @  BMI
 SP X) LDA  N CMP  SP )Y LDA  N 1+ SBC
 ' 0< @ 2+ JMP    END-CODE

CODE U<   ( U1 U2 -- FLAG)
 SP X) LDA  N STA  SP )Y LDA  N 1+ STA
 SP 2INC
 SP X) LDA  N CMP  SP )Y LDA  N 1+ SBC
   CS NOT ?[  PUTTRUE JMP  ]?
              PUTFALSE JMP  END-CODE


\ *** Block No. 33, Hexblock 21

\ COMPARISION WORDS           24DEC83KS)        cas    02aug2020

| : 0<   8000 AND  0<> ;

: >   ( N1 N2 -- FLAG)  SWAP < ;
: 0>  ( N --     FLAG)  DUP 0< SWAP 0= OR NOT ;
: 0<> ( N --     FLAG)  0= NOT ;
: U>  ( U1 U2 -- FLAG)  SWAP U< ;
: =   ( N1 N2 -- FLAG)  - 0= ;
: D0= ( D --     FLAG)  OR 0= ;
: D=  ( D1 D2 -- FLAG)  DNEGATE D+ D0= ;
: D<  ( D1 D2 -- FLAG)  ROT 2DUP -
                        IF > NIP NIP  ELSE 2DROP U< THEN ;




\ *** Block No. 34, Hexblock 22

\ MIN MAX UMAX UMIN EXTEND DABS ABS                  cas 26jan06

| : MINIMAX  ( N1 N2 FLAG -- N3)
   RDROP  IF SWAP THEN DROP ;

: MIN   ( N1 N2 -- N3) 2DUP  > MINIMAX ;   -2 ALLOT
: MAX   ( N1 N2 -- N3) 2DUP  < MINIMAX ;   -2 ALLOT
: UMAX  ( U1 U2 -- U3) 2DUP U< MINIMAX ;   -2 ALLOT
: UMIN  ( U1 U2 -- U3) 2DUP U> MINIMAX ;   -2 ALLOT

: EXTEND  ( N -- D)     DUP 0< ;

: DABS    ( D -- UD)  EXTEND IF  DNEGATE THEN ;
: ABS     ( N -- U)   EXTEND IF   NEGATE THEN ;



\ *** Block No. 35, Hexblock 23

\ LOOP PRIMITIVES          08FEB85BP/KS)

| : DODO  RDROP R> 2+ DUP >R ROT >R SWAP >R >R ;


: (DO  ( LIMIT STAR -- ) OVER - DODO ;  -2 ALLOT RESTRICT

: (?DO ( LIMIT START -- )
 OVER - ?DUP  IF DODO THEN R> DUP @ +  >R DROP ; RESTRICT

: BOUNDS  ( START COUNT -- LIMIT START ) OVER + SWAP ;

CODE  ENDLOOP 6 # LDA (NRDROP JMP   END-CODE RESTRICT

\\ DODO PUTS  "INDEX \ LIMIT \
 ADR.OF.DO"  ON RETURN-STACK

\ *** Block No. 36, Hexblock 24

\ (LOOP (+LOOP                08APR85BP)
CODE (LOOP
 CLC  1 # LDA  RP X) ADC RP X) STA
   CS ?[  RP )Y LDA  0 # ADC RP )Y STA
      CS ?[ NEXT JMP ]? ]?
LABEL DOLOOP  5 # LDY
 RP )Y LDA  IP 1+ STA  DEY
 RP )Y LDA  IP    STA  1 # LDY
 NEXT JMP    END-CODE RESTRICT

CODE (+LOOP
 CLC SP X) LDA  RP X) ADC  RP X) STA
     SP )Y LDA  RP )Y ADC  RP )Y STA
 .A ROR  SP )Y EOR
 PHP  SP 2INC  PLP DOLOOP BPL
 NEXT JMP    END-CODE RESTRICT

\ *** Block No. 37, Hexblock 25

\ LOOP INDICES                08APR85BP)

CODE I  ( -- N)    0 # LDY
LABEL LOOPINDEX    SP 2DEC   CLC
 RP )Y LDA  INY  INY
 RP )Y ADC  SP X) STA  DEY
 RP )Y LDA  INY  INY
 RP )Y ADC  1 # LDY  SP )Y STA
 NEXT JMP   END-CODE RESTRICT

CODE J  ( -- N)
 6 # LDY  LOOPINDEX BNE
            END-CODE  RESTRICT




\ *** Block No. 38, Hexblock 26

\ BRANCHING                   24DEC83KS)

CODE BRANCH
 CLC  IP    LDA  IP X) ADC  N STA
      IP 1+ LDA  IP )Y ADC  IP 1+ STA  N LDA IP STA
 NEXT JMP     END-CODE RESTRICT

CODE ?BRANCH
 SP X) LDA  SP )Y ORA PHP  SP 2INC  PLP
 ' BRANCH @ BEQ    (BUMP JMP  END-CODE   RESTRICT

\\  : BRANCH   R> DUP @ + >R ; RESTRICT

    : ?BRANCH
     0= R> OVER NOT OVER 2+  AND -ROT
     DUP @ + AND OR >R ;       RESTRICT

\ *** Block No. 39, Hexblock 27

\ RESOLVE LOOPS AND BRANCHES  03FEB85BP)

: >MARK     ( -- ADDR)  HERE   0 , ;

: >RESOLVE  ( ADDR --)  HERE OVER -   SWAP !  ;

: <MARK     ( -- ADDR)  HERE ;

: <RESOLVE  ( ADDR --)  HERE - ,  ;

: ?PAIRS  ( N1 N2 -- ) -  ABORT" UNSTRUCTURED" ;






\ *** Block No. 40, Hexblock 28

\ CASE?                       04MAY85BP)

LABEL  PUSHA
 0 # CMP  0< ?[ PHA  0FF # LDA ][
LABEL  PUSH0A   PHA   0  # LDA  ]?
LABEL  PUSH     TAX   SP 2DEC
 TXA  1 # LDY  SP )Y STA
 PLA  0 # LDX   PUTA JMP

CODE CASE?
 ( 16B1 16B2 -- 16B1 FALSE / TRUE )
 1 # LDA  SETUP JSR N LDA  SP X) CMP
  0= ?[   N 1+ LDA  SP )Y CMP 0= ?[ PUTTRUE JMP ]?  ]?
 TXA  PUSH0A JMP   END-CODE
\\ : CASE?
 ( 16B1 16B2 -- 16B1 FALSE / TRUE ) OVER = DUP  IF  NIP  THEN ;

\ *** Block No. 41, Hexblock 29

\ BRANCHING                   03FEB85BP)

: IF     COMPILE ?BRANCH >MARK  1 ;          IMMEDIATE RESTRICT
: THEN   ABS 1   ?PAIRS  >RESOLVE ;          IMMEDIATE RESTRICT
: ELSE   1 ?PAIRS  COMPILE BRANCH >MARK
         SWAP >RESOLVE  -1 ;                 IMMEDIATE RESTRICT
: BEGIN  <MARK 2 ;                           IMMEDIATE RESTRICT
: WHILE  2 ?PAIRS  2   COMPILE ?BRANCH
         >MARK -2  2SWAP  ;                  IMMEDIATE RESTRICT
| : (REPTIL   <RESOLVE   BEGIN DUP -2
    = WHILE  DROP >RESOLVE  REPEAT  ;

: REPEAT 2 ?PAIRS   COMPILE BRANCH (REPTIL ; IMMEDIATE RESTRICT

: UNTIL  2 ?PAIRS  COMPILE ?BRANCH (REPTIL ; IMMEDIATE RESTRICT


\ *** Block No. 42, Hexblock 2a

\ LOOPS                    29JAN85KS/BP)

: DO     COMPILE (DO  >MARK  3 ;     IMMEDIATE RESTRICT

: ?DO    COMPILE (?DO >MARK  3 ;     IMMEDIATE RESTRICT

: LOOP   3 ?PAIRS  COMPILE (LOOP
         COMPILE ENDLOOP  >RESOLVE ; IMMEDIATE RESTRICT

: +LOOP  3 ?PAIRS  COMPILE (+LOOP
         COMPILE ENDLOOP  >RESOLVE ; IMMEDIATE RESTRICT

: LEAVE  ENDLOOP R> 2- DUP @ + >R ;            RESTRICT

\\ RETURNSTACK: CALLADR \ INDEX
                  LIMIT \ ADR OF DO

\ *** Block No. 43, Hexblock 2b

\ UM*                      BP/KS13.2.85)
CODE UM*  ( U1 U2 -- UD)
 SP )Y LDA N STA  SP X) LDA  N 1+ STA
 INY N 2 + STX N 3 + STX  010 # LDX
  [[ N 3 + ASL N 2+ ROL N 1+ ROL N ROL
   CS ?[ CLC SP )Y LDA  N 3 + ADC  N 3 + STA
             INY  SP )Y LDA DEY N 2 + ADC   N 2 + STA
           CS ?[  N 1+ INC  0= ?[  N   INC  ]? ]? ]?
    DEX  0= ?]
 N 3 + LDA   SP )Y STA   INY N 2 + LDA   SP )Y STA   1 # LDY
 N     LDA   SP )Y STA       N 1+ LDA    SP X) STA
 NEXT JMP   END-CODE

\\ : UM*   ( U1 U2 -- UD3) >R 0 0 0 R>  010 0
  DO  DUP 2/ >R  1 AND IF 2OVER D+ THEN
      >R >R 2DUP D+ R> R> R>  LOOP DROP 2SWAP 2DROP ;

\ *** Block No. 44, Hexblock 2c

\ M* 2*                       04JUL84KS)

: M*     ( N1 N2 -- D)
 DUP 0< DUP >R IF NEGATE THEN
 SWAP DUP  0<  IF NEGATE R> NOT >R THEN
 UM*  R> IF DNEGATE THEN ;

: *      ( N N -- PROD)   UM* DROP ;

CODE 2*  ( N1 -- N2)
 SP X) LDA  .A ASL  SP X) STA
 SP )Y LDA  .A ROL  SP )Y STA
 NEXT JMP    END-CODE
| : 2*   DUP + ;



\ *** Block No. 45, Hexblock 2d

\ UM/MOD                      04JUL84KS)

| : DIVOVL
   TRUE ABORT" DIVISION OVERFLOW" ;

CODE UM/MOD  ( UD U -- UREM UQUOT)
 SP X) LDA  N 5 + STA
 SP )Y LDA  N 4 + STA   SP 2INC
 SP X) LDA  N 1+  STA
 SP )Y LDA  N     STA   INY
 SP )Y LDA  N 3 + STA   INY
 SP )Y LDA  N 2+  STA   011 # LDX  CLC
  [[ N 6 + ROR  SEC  N 1+ LDA  N 5 + SBC
     TAY  N LDA  N 4 + SBC
    CS NOT ?[  N 6 + ROL ]?
      CS ?[ N STA  N 1+ STY ]?

\ *** Block No. 46, Hexblock 2e

\
     N 3 + ROL N 2+ ROL N 1+ ROL N ROL
  DEX  0= ?]
 1 # LDY  N ROR  N 1+ ROR
   CS ?[ ;C: DIVOVL ; ASSEMBLER ]?
 N 2+  LDA SP )Y STA INY
 N 1+  LDA SP )Y STA INY
 N     LDA SP )Y STA 1 # LDY
 N 3 + LDA
 PUTA JMP    END-CODE







\ *** Block No. 47, Hexblock 2f

\ 2/ M/MOD                    24DEC83KS)

: M/MOD  ( D N -- MOD QUOT)
 DUP >R ABS  OVER
   0< IF  UNDER + SWAP  THEN
 UM/MOD R@
 0< IF NEGATE OVER IF SWAP R@ + SWAP 1-
 THEN THEN RDROP ;

CODE 2/  ( N1 -- N2)
 SP )Y LDA  .A ASL
 SP )Y LDA  .A ROR  SP )Y STA
 SP X) LDA  .A ROR
 PUTA JMP      END-CODE



\ *** Block No. 48, Hexblock 30

\ /MOD / MOD */MOD */ U/MOD  UD/MOD  KS)

: /MOD  ( N1 N2 -- REM QUOT)  >R EXTEND R> M/MOD ;

: /     ( N1 N2 -- QUOT)     /MOD NIP ;

: MOD   ( N1 N2 -- REM)      /MOD DROP ;

: */MOD ( N1 N2 N3 -- REM QUOT)  >R M*  R> M/MOD ;

: */    ( N1 N2 N3 -- QUOT)  */MOD NIP ;

: U/MOD  ( U1 U2  -- UREM UQUOT)  0 SWAP UM/MOD ;

: UD/MOD ( UD1 U2 -- UREM UDQUOT)
     >R  0  R@  UM/MOD  R> SWAP >R  UM/MOD  R>  ;

\ *** Block No. 49, Hexblock 31

\ CMOVE CMOVE> (CMOVE>       BP 08APR85)

CODE CMOVE  ( FROM TO QUAN --)
 3 # LDA SETUP JSR  DEY
 [[ [[  N CPY  0= ?[  N 1+ DEC  0< ?[
                1 # LDY  NEXT JMP  ]? ]?
     N 4 + )Y LDA  N 2+ )Y STA INY 0= ?]
     N 5 + INC N 3 + INC  ]]  END-CODE









\ *** Block No. 50, Hexblock 32

\  CMOVE> MOVE                                       cas 26jan06
CODE CMOVE> ( FROM TO QUAN --)
 3 # LDA SETUP JSR
 CLC N 1+ LDA  N 3 + ADC  N 3 + STA
 CLC N 1+ LDA  N 5 + ADC  N 5 + STA
 N 1+ INC  N LDY  CLC CS ?[
LABEL (CMOVE>
 DEY  N 4 + )Y LDA  N 2+ )Y STA ]?
 TYA  (CMOVE> BNE
 N 3 + DEC  N 5 + DEC  N 1+ DEC
 (CMOVE> BNE  1 # LDY
 NEXT JMP   END-CODE

: MOVE   ( FROM TO QUAN --) >R 2DUP U<  IF R> CMOVE> EXIT THEN
                            R> CMOVE ;


\ *** Block No. 51, Hexblock 33

\ PLACE COUNT  ERASE       16FEB85BP/KS)

: PLACE ( ADDR LEN TO --) OVER >R ROT OVER 1+ R> MOVE C! ;

CODE COUNT ( ADDR -- ADDR+1 LEN)
 SP X) LDA N STA CLC 1 # ADC SP X) STA
 SP )Y LDA N 1+ STA  0 # ADC SP )Y STA
 SP 2DEC  (C@ JMP   END-CODE

\ : COUNT ( ADR -- ADR+1 LEN ) DUP 1+  SWAP C@ ;

: ERASE ( ADDR QUAN --)      0 FILL ;





\ *** Block No. 52, Hexblock 34

\ FILL                        11JUN85BP)

CODE FILL  ( ADDR QUAN 8B -- )
 3 # LDA SETUP JSR  DEY
 N LDA  N 3 + LDX
  0<> ?[  [[ [[ N 4 + )Y STA INY 0= ?]
                N 5 + INC    DEX 0= ?]
      ]?  N 2+ LDX
  0<> ?[  [[ N 4 + )Y STA INY DEX 0= ?]
      ]? 1 # LDY
 NEXT JMP   END-CODE

\\ : FILL  ( ADDR QUAN 8B --)   SWAP ?DUP
       IF  >R OVER C! DUP 1+ R> 1- CMOVE   EXIT  THEN  2DROP  ;



\ *** Block No. 53, Hexblock 35

\ HERE PAD ALLOT , C, COMPILE 24DEC83KS)

: HERE  ( -- ADDR)   DP @ ;

: PAD   ( -- ADDR)   HERE 042 + ;

: ALLOT ( N --)      DP +! ;

: ,     ( 16B --)    HERE !  2  ALLOT ;

: C,    ( 8B --)     HERE C! 1  ALLOT ;

: COMPILE            R> DUP 2+ >R @ , ; RESTRICT




\ *** Block No. 54, Hexblock 36

\ INPUT STRINGS               24DEC83KS)

VARIABLE #TIB   0 #TIB !
VARIABLE >TIB   $100 >TIB !   \  050 ALLOT
VARIABLE >IN    0 >IN !
VARIABLE BLK    0 BLK !
VARIABLE SPAN   0 SPAN !

: TIB   ( -- ADDR )    >TIB @ ;

: QUERY TIB  050 EXPECT SPAN @ #TIB !  >IN OFF  BLK OFF ;






\ *** Block No. 55, Hexblock 37

\ SCAN SKIP /STRING           12OCT84BP)

: SCAN  ( ADDR0 LEN0 CHAR -- ADDR1 LEN1)  >R
     BEGIN  DUP  WHILE  OVER C@ R@ -
     WHILE  1- SWAP 1+ SWAP  REPEAT RDROP ;

: SKIP  ( ADDR LEN DEL -- ADDR1 LEN1)  >R
     BEGIN  DUP  WHILE  OVER C@ R@ =
     WHILE  1- SWAP 1+ SWAP  REPEAT     RDROP ;


: /STRING  ( ADDR0 LEN0 +N - ADDR1 LEN1)
     OVER UMIN ROT OVER + -ROT - ;




\ *** Block No. 56, Hexblock 38

\ CAPITAL                     03APR85BP)
(C LABEL (CAPITAL  \ FOR COMMODORE ONLY
 PHA  0DF # AND    \ 2ND UPPER TO LOWER
 ASCII A # CMP
 CS ?[  ASCII Z  1+ # CMP
    CC ?[  PLA CLC ASCII A ASCII A - # ADC RTS
  ]? ]?  PLA  RTS  END-CODE )

LABEL (CAPITAL  \ FOR ASCII ONLY
 ASCII a # CMP
 CS ?[  ASCII z  1+ # CMP
    CC ?[      SEC  ASCII a ASCII A - # SBC
  ]? ]?  RTS  END-CODE

CODE CAPITAL  ( CHAR -- CHAR' )
 SP X) LDA  (CAPITAL JSR  SP X) STA NEXT JMP    END-CODE

\ *** Block No. 57, Hexblock 39

\ CAPITALIZE                  03APR85BP)

CODE CAPITALIZE  ( STRING -- STRING )
 SP X) LDA N    STA  SP )Y LDA  N 1+ STA
  N X) LDA N 2+ STA   DEY
 [[ N 2+ CPY  0= ?[ 1 # LDY  NEXT JMP ]?
   INY N )Y LDA  (CAPITAL JSR  N )Y STA
 ]]   END-CODE

\\ : CAPITALIZE  ( STRING -- STRING )
      DUP  COUNT  BOUNDS ?DO  I C@  CAPITAL  I C!  THEN  LOOP ;

\\ CAPITAL ( CHAR -- CHAR )
   ASCII A  ASCII Z 1+  UWITHIN
   IF  I C@  [ ASCII A  ASCII A - ]  LITERAL -  ;


\ *** Block No. 58, Hexblock 3a

\ (WORD                       08APR85BP)

| CODE (WORD   ( CHAR ADR0 LEN0 -- ADR)
       \ N   : LENGTH OF SOURCE
       \ N+2 : PTR IN SOURCE / NEXT CHAR
       \ N+4 : STRING START ADRESS
       \ N+6 : STRING LENGTH
 N 6 + STX        \ 0 =: STRING_LENGTH
 3 # LDY [[ SP )Y LDA  N ,Y STA DEY  0< ?]
 1 # LDY  CLC  >IN    LDA  N 2+  ADC  N 2+  STA
                  \ >IN+ADR0 =: N+2
 >IN 1+ LDA  N 3 + ADC  N 3 + STA SEC  N LDA  >IN  SBC  N  STA
                  \ LEN0->IN =: N
   N 1+ LDA  >IN 1+ SBC  N 1+ STA
  CC ?[ SP X) LDA  >IN    STA  \ STREAM EXHAUSTED
        SP )Y LDA  >IN 1+ STA

\ *** Block No. 59, Hexblock 3b

\ (WORD                       08APR85BP)

][ 4 # LDY  [[  N LDA  N 1+ ORA         \ SKIP CHAR'S
       0= NOT ?[[ N 2+ X) LDA SP )Y CMP \ WHILE COUNT <>0
       0=     ?[[ N 2+ WINC  N WDEC ]]?
    N 2+  LDA  N 4 + STA       \ SAVE STRING_START_ADRESS
    N 3 + LDA  N 5 + STA
    [[  N 2+ X) LDA  SP )Y CMP PHP      \ SCAN FOR CHAR
        N 2+ WINC  N WDEC PLP
    0= NOT ?[[ N 6 + INC     \ COUNT STRING_LENGTH
       N LDA N 1+ ORA
    0= ?]  ]? ]?              \ FROM COUNT = 0 IN SKIP)
 SEC 2 # LDY
       \ ADR_AFTER_STRING - ADR0 =: >IN)
 N 2+  LDA  SP )Y SBC  >IN    STA  INY
 N 3 + LDA  SP )Y SBC  >IN 1+ STA

\ *** Block No. 60, Hexblock 3c

\ (WORD                       08APR85BP)

]? \ FROM 1ST ][, STREAM WAS EXHAUSTED
   \ WHEN WORD CALLED)
 CLC  4 # LDA  SP ADC  SP STA
  CS ?[ SP 1+ INC ]?  \ 2DROP
 USER' DP # LDY  UP )Y LDA
 SP X) STA N    STA  INY
 UP )Y LDA 1 # LDY
 SP )Y STA N 1+ STA        \ DP @
 DEY N 6 + LDA  \ STORE COUNT BYTE FIRST
 [[  N )Y STA  N 4 + )Y LDA  INY
     N 6 + DEC  0< ?]
 020 # LDA  N )Y STA       \ ADD A BLANK
 1 # LDY   NEXT JMP   END-CODE


\ *** Block No. 61, Hexblock 3d

\ SOURCE WORD PARSE NAME      08APR85BP)

: SOURCE   ( -- ADDR LEN)
    BLK @  ?DUP IF  BLOCK B/BLK EXIT THEN  TIB #TIB @  ;

: WORD  ( CHAR -- ADDR)   SOURCE (WORD ;

: PARSE ( CHAR -- ADDR LEN) >R SOURCE  >IN @  /STRING OVER SWAP
                  R> SCAN >R OVER - DUP R> 0<> - >IN +! ;

: NAME   ( -- ADDR)  BL WORD  CAPITALIZE  EXIT  ;

\\ : WORD  ( CHAR -- ADDR)        >R
 SOURCE  OVER SWAP  >IN @  /STRING  R@ SKIP  OVER  SWAP  R>
 SCAN >R  ROT OVER SWAP  - R> 0<> -  >IN !
 OVER - HERE PLACE  BL HERE COUNT + C! HERE ;

\ *** Block No. 62, Hexblock 3e

\ STATE ASCII ,"  ("  "       24DEC83KS)

VARIABLE STATE    0 STATE !

: ASCII   BL WORD 1+ C@ STATE @
          IF [COMPILE] LITERAL THEN ; IMMEDIATE

: ,"      ASCII "  PARSE  HERE OVER 1+  ALLOT PLACE  ;

: "LIT    R> R> UNDER COUNT + >R >R ;  RESTRICT

: ("      "LIT ; RESTRICT

: "       COMPILE ("  ,"  ; IMMEDIATE RESTRICT



\ *** Block No. 63, Hexblock 3f

\ ." ( .( \ \\ HEX DECIMAL    08SEP84KS)
: (."    "LIT COUNT TYPE ; RESTRICT

: ."     COMPILE (." ," ;  IMMEDIATE RESTRICT

: (      ASCII )  PARSE 2DROP ;          IMMEDIATE

: .(     ASCII )  PARSE TYPE ;           IMMEDIATE

: \      >IN @  C/L /  1+ C/L *  >IN ! ; IMMEDIATE

: \\        B/BLK >IN ! ;  IMMEDIATE

: \NEEDS    NAME FIND NIP  IF  [COMPILE] \  THEN ;

: HEX       010 BASE ! ;        : DECIMAL    0A BASE ! ;

\ *** Block No. 64, Hexblock 40

\ NUMBER CONV.:  DIGIT?  ACCUMULATE  KS)
: DIGIT?  ( CHAR -- DIGIT TRUE/ FALSE )
 ASCII 0 -   DUP 9 U>
 IF [ ASCII A ASCII 9 - 1- ] LITERAL -  DUP 9 U>
     IF [ 2SWAP ( UNSTRUKTURIERT) ] THEN
   BASE @  OVER  U>  ?DUP  ?EXIT    THEN DROP  FALSE ;

: ACCUMULATE ( +D0 ADR DIGIT - +D1 ADR)
 SWAP >R SWAP  BASE @ UM*  DROP  ROT  BASE @ UM*  D+  R>  ;

: CONVERT  ( +D1 ADDR0 -- +D2 ADDR2)
    1+  BEGIN  COUNT DIGIT? WHILE  ACCUMULATE    REPEAT  1- ;

| : END?   ( -- FLAG )  PTR @  0= ;
| : CHAR   ( ADDR0 -- ADDR1 CHAR )   COUNT   -1 PTR +!  ;
| : PREVIOUS  ( ADDR0 -- ADDR0 CHAR) 1-  COUNT ;

\ *** Block No. 65, Hexblock 41

\ ?NONUM ?NUM FIXBASE?        13FEB85KS)

VARIABLE DPL   -1 DPL !

| : ?NONUM   ( FLAG -- EXIT IF TRUE )
     IF RDROP 2DROP DROP RDROP FALSE THEN ;

| : ?NUM     ( FLAG -- EXIT IF TRUE )
     IF RDROP DROP R> IF  DNEGATE  THEN
     ROT DROP  DPL @ 1+  ?DUP ?EXIT DROP TRUE THEN ;
| : FIXBASE? ( CHAR - CHAR FALSE /  NEWBASE TRUE )
     ASCII & CASE?  IF 0A TRUE EXIT THEN
     ASCII $ CASE?  IF 10 TRUE EXIT THEN
     ASCII H CASE?  IF 10 TRUE EXIT THEN
     ASCII % CASE?  IF  2 TRUE EXIT THEN FALSE  ;


\ *** Block No. 66, Hexblock 42

\                             13FEB85KS)

| : PUNCTUATION?   ( CHAR -- FLAG)
     ASCII , OVER = SWAP ASCII . =  OR ;

| : ?DPL   DPL @ -1 = ?EXIT  1 DPL +! ;

| VARIABLE PTR      \ POINTS INTO STRING









\ *** Block No. 67, Hexblock 43

\ (NUMBER  NUMBER             13FEB85KS)
: NUMBER?   ( STRING - STRING FALSE / N 0< / D 0> )
     BASE PUSH  DUP COUNT  PTR !  DPL ON
     0 >R  ( +SIGN)
     0.0 ROT           END? ?NONUM CHAR
     ASCII - CASE?
     IF RDROP TRUE >R  END? ?NONUM CHAR THEN FIXBASE?
     IF  BASE !        END? ?NONUM CHAR THEN
     BEGIN   DIGIT?  0= ?NONUM
       BEGIN  ACCUMULATE  ?DPL  END? ?NUM
          CHAR DIGIT?  0= UNTIL
       PREVIOUS  PUNCTUATION?  0= ?NONUM
       DPL OFF   END? ?NUM CHAR
     REPEAT ;
: NUMBER  ( STRING -- D )
     NUMBER?  ?DUP 0= ABORT" ?" 0< IF EXTEND THEN ;

\ *** Block No. 68, Hexblock 44

\ HIDE REVEAL IMMEDIATE RESTRICT     KS)
VARIABLE LAST     0 LAST !

| : LAST?   ( -- FALSE / ACF TRUE) LAST @ ?DUP ;

: HIDE    LAST?  IF 2- @ CURRENT @ ! THEN ;

: REVEAL  LAST?  IF 2-   CURRENT @ ! THEN ;

: RECURSIVE   REVEAL  ;   IMMEDIATE RESTRICT

| : FLAG!  ( 8B --) LAST? IF UNDER C@ OR OVER C! THEN   DROP ;

: IMMEDIATE  040 FLAG! ;

: RESTRICT   080 FLAG! ;

\ *** Block No. 69, Hexblock 45

\ CLEARSTACK HALLOT HEAP HEAP?                       cas 26jan06

CODE CLEARSTACK USER' S0 # LDY
     UP )Y LDA  SP    STA  INY UP )Y LDA  SP 1+ STA
     1 # LDY  NEXT JMP   END-CODE

: HALLOT ( QUAN -- )  S0 @ OVER - SWAP
     SP@ 2+  DUP ROT -  DUP S0 !
     2 PICK OVER -  MOVE  CLEARSTACK  S0 ! ;

: HEAP   ( -- ADDR)        S0 @  6+ ;

: HEAP?  ( ADDR -- FLAG)   HEAP UP@ UWITHIN ;

| : HEAPMOVE   ( FROM -- FROM) DUP HERE  OVER -
      DUP HALLOT  HEAP SWAP CMOVE HEAP OVER - LAST +!  REVEAL ;

\ *** Block No. 70, Hexblock 46

\ DOES>  ;                 30DEC84KS/BP)

LABEL (DODOES>   RP 2DEC
      IP 1+ LDA RP )Y STA  IP LDA  RP X) STA  \ PUT IP ON RP
      CLC  W X) LDA  3 # ADC IP STA
      TXA  W )Y ADC  IP 1+ STA  \ W@ + 3 -> IP
LABEL DOCREATE
      2 # LDA  CLC W ADC PHA TXA W 1+ ADC  PUSH JMP END-CODE

| : (;CODE    R> LAST @  NAME>  ! ;

: DOES> COMPILE (;CODE  04C C,
    COMPILE (DODOES> ;  IMMEDIATE RESTRICT




\ *** Block No. 71, Hexblock 47

\ 6502-ALIGN  ?HEAD  \        08SEP84BP)

| : 6502-ALIGN/1   ( ADR -- ADR' ) DUP  0FF AND  0FF =  - ;


| : 6502-ALIGN/2   ( LFA -- LFA )
   HERE  0FF AND 0FF =
   IF  DUP DUP 1+  HERE OVER - 1+ CMOVE>  \ LFA NOW INVALID
       1 LAST +! 1 ALLOT  THEN  ;

VARIABLE ?HEAD    0 ?HEAD !

: |     ?HEAD @   ?EXIT   -1 ?HEAD  ! ;




\ *** Block No. 72, Hexblock 48

\ WARNING   CREATE            30DEC84BP)

VARIABLE WARNING  0 WARNING !

| : EXISTS?
    WARNING @ ?EXIT
    LAST @  CURRENT @  (FIND  NIP
    IF SPACE  LAST @ .NAME ." EXISTS " ?CR THEN  ;

: CREATE  HERE BLK @ ,  CURRENT @ @ ,
    NAME  C@ DUP 1 020  UWITHIN NOT  ABORT" INVALID NAME"
    HERE  LAST ! 1+ ALLOT  EXISTS? ?HEAD @
    IF 1 ?HEAD +!  DUP  6502-ALIGN/1 , \ POINTER TO CODE
       HEAPMOVE 020 FLAG! 6502-ALIGN/1 DP !
    ELSE  6502-ALIGN/2  DROP THEN  REVEAL  0 ,
    ;CODE  DOCREATE JMP END-CODE

\ *** Block No. 73, Hexblock 49

\ NFA?                        30DEC84BP)
| CODE NFA?  ( VOCABTHREAD  CFA -- NFA / FALSE)
    SP X) LDA  N 4 + STA  SP )Y LDA  N 5 + STA   SP 2INC
    [[ [[ SP X) LDA  N 2+  STA  SP )Y LDA  N 3 + STA
        N 2+ ORA  0= ?[ PUTFALSE JMP ]?
        N 2+ )Y LDA SP )Y STA  N 1+ STA
        N 2+ X) LDA SP X) STA  N STA
        N 1+ ORA  0= ?[  NEXT JMP  ]?  \ N=LINK
        N 2INC N X) LDA PHA SEC 01F # AND
        N ADC  N STA  CS ?[ N 1+ INC ]?
         PLA  020 # AND  0= NOT
         ?[ N )Y LDA  PHA
            N X) LDA N STA PLA N 1+ STA ]?
        N LDA     N 4 + CMP  0= ?]  \ VOCABTHREAD=0
        N 1+ LDA  N 5 + CMP  0= ?]  \ D.H. LEERES VOCABULARY
    ' 2+ @ JMP       END-CODE       \  IN NFA? IST ERLAUBT

\ *** Block No. 74, Hexblock 4a

\ >NAME NAME> >BODY .NAME     03FEB85BP)

: >NAME   ( CFA -- NFA / FALSE)  VOC-LINK
    BEGIN @ DUP WHILE 2DUP 4 - SWAP
        NFA? ?DUP IF -ROT 2DROP EXIT THEN REPEAT NIP ;

| : (NAME>  ( NFA -- CFA) COUNT 01F  AND + ;

: NAME> ( NFA -- CFA)  DUP (NAME> SWAP C@ 020 AND IF @ THEN ;

: >BODY   ( CFA -- PFA)   2+ ;

: .NAME   ( NFA --)
    ?DUP IF DUP HEAP?  IF ." |" THEN COUNT 01F AND TYPE
         ELSE  ." ???" THEN  SPACE  ;


\ *** Block No. 75, Hexblock 4b

\ : ; CONSTANT VARIABLE    09JAN85KS/BP)

: :  CREATE  HIDE  CURRENT @ CONTEXT ! ] 0
       ;CODE HERE >RECOVER ! \ RESOLVE FWD. REFERENCE
       RP 2DEC IP    LDA  RP X) STA IP 1+ LDA  RP )Y STA
       W LDA  CLC  2 # ADC  IP STA  TXA   W 1+ ADC  IP 1+ STA
       NEXT JMP   END-CODE

: ;        0 ?PAIRS  COMPILE EXIT
 [COMPILE] [ REVEAL ; IMMEDIATE RESTRICT

: CONSTANT  ( 16B --)  CREATE ,
     ;CODE  SP 2DEC  2 # LDY W )Y LDA  SP X) STA  INY
     W )Y LDA   1 # LDY   SP )Y STA NEXT JMP   END-CODE

: VARIABLE   CREATE  2 ALLOT ;

\ *** Block No. 76, Hexblock 4c

\ UALLOT USER ALIAS        10JAN85KS/BP)

: UALLOT ( QUAN -- OFFSET)
    DUP UDP @ +  0FF U> ABORT" USERAREA FULL"
    UDP  @ SWAP UDP +! ;

: USER  CREATE   2 UALLOT C,
   ;CODE  SP 2DEC  2 # LDY W )Y LDA  CLC UP    ADC  SP X) STA
   TXA  INY UP 1+ ADC  1 # LDY  SP )Y STA   NEXT JMP   END-CODE

: ALIAS  ( CFA --)
   CREATE LAST @ DUP C@ 020 AND
   IF   -2 ALLOT  ELSE  020 FLAG! THEN  (NAME> ! ;




\ *** Block No. 77, Hexblock 4d

\ VOC-LINK VP CURRENT CONTEXT ALSO   BP)
CREATE   VP       10 ALLOT

VARIABLE CURRENT

: CONTEXT ( -- ADR  )  VP DUP @ + 2+ ;

| : THRU.VOCSTACK  ( -- FROM TO ) VP 2+ CONTEXT ;
\ "ONLY FORTH ALSO ASSEMBLER" GIVES VP :
\  COUNTWORD = 6 \ONLY\FORTH\ASSEMBLER

: ALSO     VP @
 0A > ERROR" VOCABULARY STACK FULL"
 CONTEXT @   2 VP +!  CONTEXT ! ;

: TOSS   -2 VP +! ;

\ *** Block No. 78, Hexblock 4e

\  VOCABULARY FORTH ONLY FORTH-83 KS/BP)

: VOCABULARY CREATE  0 , 0 ,
    HERE VOC-LINK @ ,  VOC-LINK ! DOES>  CONTEXT ! ;

\ NAME \ CODE \ THREAD \ COLDTHREAD \ VOC-LINK

VOCABULARY FORTH

VOCABULARY ONLY
] DOES>  [ ONLYPATCH ]  0 VP ! CONTEXT !  ALSO  ;  ' ONLY !

: ONLYFORTH  ONLY FORTH ALSO DEFINITIONS ;




\ *** Block No. 79, Hexblock 4f

\ DEFINITIONS ORDER WORDS  13JAN84BP/KS)

: DEFINITIONS   CONTEXT @ CURRENT ! ;

| : .VOC  ( ADR -- ) @ 2- >NAME .NAME ;

: ORDER
 THRU.VOCSTACK  DO  I .VOC  -2  +LOOP  2 SPACES  CURRENT .VOC ;

: WORDS      CONTEXT @
   BEGIN  @ DUP STOP? 0= AND
   WHILE  ?CR DUP 2+ .NAME SPACE REPEAT DROP ;





\ *** Block No. 80, Hexblock 50

\ (FIND                       08APR85BP)

CODE (FIND  ( STRING THREAD
       -- STRING FALSE / NAMEFIELD TRUE)
 3 # LDY [[ SP )Y LDA N ,Y STA DEY 0< ?]
 N 2+ X) LDA 01F # AND N 4 + STA
LABEL FINDLOOP   0 # LDY
 N )Y LDA   TAX   INY
 N )Y LDA  N 1+ STA  N STX  N ORA
  0= ?[ 1 # LDY 0 # LDX PUTFALSE JMP ]?
 INY N )Y LDA  01F # AND  N 4 + CMP
  FINDLOOP BNE       \ COUNTBYTE MATCH
 CLC 2 # LDA N    ADC N 5 + STA
     0 # LDA N 1+ ADC N 6 + STA
 N 4  + LDY
  [[ N 2+ )Y LDA N 5 + )Y CMP

\ *** Block No. 81, Hexblock 51

\
     FINDLOOP BNE   DEY  0= ?]
 3 # LDY N 6 + LDA  SP )Y STA   DEY
         N 5 + LDA  SP )Y STA
 DEY  0 # LDX    PUTTRUE JMP   END-CODE












\ *** Block No. 82, Hexblock 52

\ FOUND                       29JAN85BP)

| CODE FOUND  ( NFA -- CFA N )
 SP X) LDA N STA  SP )Y LDA N 1+ STA
  N X) LDA N 2+ STA  01F # AND  SEC N ADC N STA
  CS ?[ N 1+ INC ]?
 N 2+ LDA 020 # AND
 0= ?[ N    LDA  SP X) STA N 1+ LDA
    ][ N X) LDA  SP X) STA N )Y LDA   ]?  SP )Y STA
  SP 2DEC   N 2+ LDA   0< ?[  INY  ]?
 .A ASL
  0< NOT ?[ TYA 0FF # EOR TAY INY  ]?
  TYA SP X) STA
  0< ?[ 0FF # LDA  24 C, ]?
 TXA  1 # LDY  SP )Y STA
 NEXT JMP  END-CODE

\ *** Block No. 83, Hexblock 53

\\

| : FOUND  ( NFA -- CFA N )
      DUP   C@ >R   (NAME>
            R@ 020 AND  IF @ THEN
        -1  R@ 080 AND  IF 1- THEN
            R> 040 AND  IF NEGATE THEN ;










\ *** Block No. 84, Hexblock 54

\ FIND  ' [']                 13JAN85BP)

: FIND ( STRING -- CFA N / STRING FALSE)
   CONTEXT DUP @ OVER 2- @ = IF 2- THEN
   BEGIN  UNDER @ (FIND IF NIP FOUND EXIT THEN
     OVER VP 2+ U>
   WHILE  SWAP 2-  REPEAT NIP FALSE ;

: '  ( -- CFA ) NAME FIND 0= ABORT" HAEH?"  ;

: [COMPILE]   ' , ;             IMMEDIATE RESTRICT

: [']     ' [COMPILE] LITERAL ; IMMEDIATE RESTRICT

: NULLSTRING? ( STRING -- STRING FALSE  / TRUE)
    DUP C@ 0=  DUP  IF NIP THEN ;

\ *** Block No. 85, Hexblock 55

\ >INTERPRET                  28FEB85BP)

LABEL JUMP
 INY CLC W )Y LDA 2 # ADC IP    STA
 INY     W )Y LDA 0 # ADC IP 1+ STA
 1 # LDY  NEXT JMP   END-CODE
VARIABLE >INTERPRET

JUMP  ' >INTERPRET !

\\ MAKE VARIABLE >INTERPRET TO SPECIAL
   DEFER





\ *** Block No. 86, Hexblock 56

\ INTERPRET INTERACTIVE    31DEC84KS/BP)             cas 26jan06

DEFER  NOTFOUND

: NO.EXTENSIONS ( STRING -- ) ERROR" WHAT?"   ;  \ STRING NOT 0

' NO.EXTENSIONS  IS  NOTFOUND

: INTERPRET     >INTERPRET ;  -2 ALLOT

| : INTERACTIVE  ?STACK  NAME FIND  ?DUP
     IF 1 AND IF EXECUTE >INTERPRET THEN
      ABORT" COMPILE ONLY"  THEN  NULLSTRING? ?EXIT    NUMBER?
  0= IF  NOTFOUND  THEN  >INTERPRET ;  -2 ALLOT

' INTERACTIVE  >INTERPRET !

\ *** Block No. 87, Hexblock 57

\ COMPILING [ ]               20DEC84BP)

| : COMPILING
 ?STACK  NAME FIND   ?DUP
 IF   0> IF  EXECUTE >INTERPRET  THEN
  , >INTERPRET THEN
 NULLSTRING? ?EXIT  NUMBER?   ?DUP
   IF 0> IF SWAP [COMPILE] LITERAL THEN
    [COMPILE] LITERAL
   ELSE  NOTFOUND THEN    >INTERPRET ; -2 ALLOT

: [    ['] INTERACTIVE  IS >INTERPRET STATE OFF ;  IMMEDIATE

: ]    ['] COMPILING    IS >INTERPRET STATE ON ;



\ *** Block No. 88, Hexblock 58

\ PERFOM  DEFER IS            03FEB85BP)

| : CRASH   TRUE ABORT" CRASH" ;

: DEFER   CREATE  ['] CRASH ,
    ;CODE  2 # LDY  W )Y LDA  PHA INY W )Y LDA
    W 1+ STA  PLA W STA  1 # LDY W 1- JMP  END-CODE

: (IS            R>  DUP  2+ >R @ ! ;

| : DEF?  ( CFA -- ) @ ['] NOTFOUND   @ OVER =
     SWAP ['] >INTERPRET @ = OR NOT  ABORT" NOT DEFERRED" ;

: IS      ( ADR -- )  ' DUP  DEF?  >BODY
     STATE  @  IF  COMPILE (IS , EXIT  THEN !  ; IMMEDIATE


\ *** Block No. 89, Hexblock 59

\ ?STACK                      08SEP84KS)          cas 15july2020

| : STACKFULL   ( -- )
 DEPTH 20 > ABORT" TIGHT STACK"
 REVEAL LAST? IF DUP HEAP? IF NAME> ELSE 4 - THEN
       (FORGET THEN  TRUE ABORT" DICTIONARY FULL" ;

CODE ?STACK USER' DP # LDY
      SEC SP    LDA  UP )Y  SBC N STA  INY SP 1+ LDA  UP )Y SBC
  0= ?[ 1 # LDY ;C: STACKFULL ; ASSEMBLER ]?
     USER' S0 # LDY  UP )Y LDA SP    CMP INY
     UP )Y LDA SP 1+ SBC   1 # LDY  CS ?[  NEXT JMP ]?
     ;C: TRUE ABORT" STACK EMPTY" ; -2 ALLOT

\\  : ?STACK  SP@  HERE - 100 U< IF STACKFULL THEN
         SP@  S0 @ U> ABORT" STACK EMPTY" ;

\ *** Block No. 90, Hexblock 5a

\ .STATUS PUSH LOAD           08SEP84KS)

DEFER .STATUS    ' NOOP IS .STATUS

| CREATE PULL  0  ] R> R> ! ;

: PUSH ( ADDR -- )
    R> SWAP DUP >R @ >R  PULL >R >R  ;  RESTRICT


: LOAD   ( BLK --)
    ?DUP 0= ?EXIT BLK PUSH  BLK !
    >IN PUSH  >IN OFF  .STATUS INTERPRET ;




\ *** Block No. 91, Hexblock 5b

\ +LOAD THRU +THRU --> RDEPTH DEPTH  KS)

: +LOAD  ( OFFSET --)  BLK @  + LOAD ;

: THRU  ( FROM TO --)      1+  SWAP  DO  I LOAD  LOOP ;

: +THRU  ( OFF0 OFF1 --)   1+  SWAP  DO  I +LOAD LOOP ;

: -->  1 BLK +! >IN OFF .STATUS  ;  IMMEDIATE

: RDEPTH  ( -- +N)  R0 @  RP@ 2+ - 2/ ;

: DEPTH   ( -- +N)  SP@ S0 @ SWAP - 2/ ;




\ *** Block No. 92, Hexblock 5c

\ QUIT (QUIT ABORT            07JUN85BP)

| : PROMPT  STATE @  IF ."  COMPILING"  EXIT  THEN  ."  OK" ;

: (QUIT
    BEGIN .STATUS CR QUERY INTERPRET PROMPT  REPEAT ;  -2 ALLOT

DEFER 'QUIT    ' (QUIT  IS 'QUIT

: QUIT     R0 @ RP! [COMPILE] [ 'QUIT ;  -2 ALLOT

: STANDARDI/O   [ OUTPUT ] LITERAL OUTPUT 4 CMOVE ;

DEFER 'ABORT   ' NOOP IS 'ABORT

: ABORT CLEARSTACK END-TRACE 'ABORT STANDARDI/O QUIT ; -2 ALLOT

\ *** Block No. 93, Hexblock 5d

\ (ERROR ABORT" ERROR"        20MAR85BP)

VARIABLE SCR    1 SCR !

VARIABLE R#     0 R#  !

: (ERROR  ( STRING -- )
    STANDARDI/O SPACE HERE .NAME COUNT TYPE  SPACE ?CR
    BLK @  ?DUP IF  SCR !  >IN @  R# ! THEN QUIT ; -2 ALLOT

' (ERROR  ERRORHANDLER  !

: (ABORT"    "LIT SWAP IF
     >R CLEARSTACK R> ERRORHANDLER PERFORM
     EXIT THEN  DROP ;  RESTRICT


\ *** Block No. 94, Hexblock 5e

\

| : (ERR"  "LIT SWAP
      IF ERRORHANDLER  PERFORM EXIT THEN DROP ;    RESTRICT

: ABORT"  COMPILE (ABORT" ," ;   IMMEDIATE  RESTRICT

: ERROR"  COMPILE (ERR"   ," ;    IMMEDIATE  RESTRICT









\ *** Block No. 95, Hexblock 5f

\ -TRAILING                   08APR85BP)

020 CONSTANT BL

CODE -TRAILING  ( ADDR N1 -- ADR  N2 )
 TYA   SETUP JSR
 SP X) LDA  N 2+ STA   CLC
 SP )Y LDA  N 1+ ADC  N 3 + STA
 N LDY  CLC   CS ?[
LABEL (-TRAIL
 DEY  N 2+ )Y LDA  BL # CMP
  0<> ?[ INY  0= ?[ N 1+ INC ]?
         TYA PHA  N 1+ LDA PUSH JMP ]?
 ]?   TYA   (-TRAIL BNE
 N 3 + DEC N 1 + DEC  (-TRAIL BPL
 TYA PUSH0A JMP   END-CODE

\ *** Block No. 96, Hexblock 60

\ SPACE SPACES             29JAN85KS/BP)

: SPACE            BL EMIT ;

: SPACES  ( U --)  0  ?DO SPACE LOOP ;

\\
: -TRAILING  ( ADDR N1 -- ADDR N2)
 2DUP  BOUNDS
    ?DO 2DUP + 1- C@ BL -
      IF LEAVE THEN  1- LOOP  ;






\ *** Block No. 97, Hexblock 61

\ HOLD <# #> SIGN # #S        24DEC83KS)
| : HLD  ( -- ADDR)    PAD 2- ;

: HOLD  ( CHAR -- )     -1 HLD +! HLD @ C! ;

: <#                   HLD HLD ! ;

: #>    ( 32B -- ADDR +N ) 2DROP HLD @  HLD OVER - ;

: SIGN  ( N -- )  0< IF ASCII  - HOLD THEN ;

: #     ( +D1 -- +D2) BASE @ UD/MOD ROT 09 OVER <
   IF [ ASCII A ASCII 9 - 1- ] LITERAL +
   THEN  ASCII 0  +  HOLD ;

: #S    ( +D -- 0 0 ) BEGIN # 2DUP  D0= UNTIL ;

\ *** Block No. 98, Hexblock 62

\ PRINT NUMBERS               24DEC83KS)

: D.R  -ROT UNDER DABS <# #S ROT SIGN #>
        ROT OVER MAX OVER - SPACES TYPE  ;

: .R    SWAP EXTEND ROT D.R ;

: U.R   0 SWAP D.R ;

: D.    0 D.R SPACE ;

: .     EXTEND D. ;

: U.    0 D. ;



\ *** Block No. 99, Hexblock 63

\ .S LIST C/L L/S             24DEC83KS)

: .S   SP@  S0 @  OVER - 020 UMIN BOUNDS ?DO I @ U.  2 +LOOP ;

40 CONSTANT C/L    \ SCREEN LINE LENGTH

10 CONSTANT L/S    \ LINES PER SCREEN

: LIST   ( BLK --)
   SCR ! ." SCR " SCR @ DUP U.
       ." DR "  DRV? .
   L/S 0 DO CR I 2 .R SPACE SCR @  BLOCK
            I C/L * + C/L -TRAILING TYPE  LOOP CR ;




\ *** Block No. 100, Hexblock 64

\ MULTITASKER PRIMITIVES      BP03NOV85)
CODE PAUSE   NEXT HERE 2- !  END-CODE

: LOCK  ( ADDR --)
 DUP @  UP@ =  IF  DROP EXIT  THEN
 BEGIN  DUP @  WHILE  PAUSE  REPEAT UP@ SWAP ! ;

: UNLOCK  ( ADDR --)   DUP LOCK OFF ;

LABEL WAKE    WAKE >WAKE !
 PLA  SEC  5 # SBC  UP    STA   PLA       0 # SBC  UP 1+ STA
 04C # LDA  UP X) STA           6 # LDY  UP )Y LDA SP    STA
     INY  UP )Y LDA SP 1+ STA 1 # LDY
 SP X) LDA  RP STA         SP )Y LDA  RP 1+ STA   SP 2INC
 IP  # LDX  XPULL JMP END-CODE


\ *** Block No. 101, Hexblock 65

\ BUFFER MECHANISM            15DEC83KS)             cas 26jan06

USER FILE           0 FILE !    \ ADR OF FILE CONTROL BLOCK

VARIABLE PREV       0 PREV !    \ LISTHEAD

| VARIABLE BUFFERS  0 BUFFERS ! \ SEMAPHOR

0408 CONSTANT B/BUF             \ size of buffer








\ *** Block No. 102, Hexblock 66

\\ structure of buffer (same for all volksFORTH )    cas 26jan06
 0 : LINK
 2 : FILE
 6 : BLOCKNR
 8 : STATUSFLAGS
0A : DATA .. 1 KB ..

STATUSFLAG BITS: 15   1 -> UPDATED

FILE = -1 EMPTY BUFFER
     = 0 NO FCB , DIRECT ACCESS
     = ELSE  ADR OF FCB
     ( SYSTEM   DEPENDENT )




\ *** Block No. 103, Hexblock 67

\ SEARCH FOR BLOCKS IN MEMORY 11JUN85BP)

LABEL THISBUFFER?        2 # LDY
   [[  N 4 + )Y LDA N 2- ,Y CMP
 0= ?[[  INY  6 # CPY 0= ?] ]? RTS \ ZERO IF THIS BUFFER )

| CODE (CORE? ( BLK FILE -- ADDR / BLK  FILE )
 \ N-AREA : 0 BLK 2 FILE 4 BUFFER
 \          6 PREDECESSOR
 3 # LDY
   [[ SP )Y LDA  N ,Y STA  DEY  0< ?]
 USER' OFFSET # LDY  CLC  UP )Y LDA  N 2+  ADC  N 2+  STA
 INY  UP )Y LDA  N 3 + ADC  N 3 + STA  PREV    LDA  N 4 + STA
 PREV 1+ LDA  N 5 + STA  THISBUFFER? JSR    0= ?[



\ *** Block No. 104, Hexblock 68

\   "                         11JUN85BP)

LABEL BLOCKFOUND     SP 2INC  1 # LDY
 8 #   LDA  CLC N 4 + ADC SP X) STA
 N 5 + LDA        0 # ADC SP )Y STA
 ' EXIT @ JMP  ]?
 [[ N 4 + LDA  N 6 + STA N 5 + LDA  N 7 + STA
    N 6 + X) LDA  N 4 + STA  1 # LDY
    N 6 + )Y LDA  N 5 + STA  N 4 + ORA
     0= ?[ ( LIST EMPTY )  NEXT JMP ]?
   THISBUFFER? JSR 0= ?] \ FOUND, RELINK
 N 4 + X) LDA  N 6 + X) STA  1 # LDY N 4 + )Y LDA  N 6 + )Y STA
 PREV    LDA  N 4 + X) STA   PREV 1+ LDA  N 4 + )Y STA
 N 4 + LDA  PREV    STA      N 5 + LDA  PREV 1+ STA
 BLOCKFOUND JMP    END-CODE


\ *** Block No. 105, Hexblock 69

\\ (CORE?                       23SEP85BP
| : this? ( blk file bufadr -- flag )
   DUP 4+ @  SWAP 2+ @  D= ;

| : (CORE?  ( BLK FILE -- DATAADDR / BLK FILE )
  BEGIN  OVER OFFSET @ + OVER  PREV @
    THIS? IF RDROP 2DROP PREV @ 8 + EXIT THEN
    2DUP >R OFFSET @ + >R PREV @
    BEGIN  DUP @ ?DUP
       0= IF RDROP RDROP DROP EXIT THEN
      DUP R> R> 2DUP >R >R  ROT THIS? 0=
    WHILE  NIP  REPEAT DUP @ ROT !  PREV @ OVER !  PREV !
    RDROP RDROP REPEAT ;    -2 ALLOT




\ *** Block No. 106, Hexblock 6a

\ (DISKERR                    11JUN85BP)

: (DISKERR   ." ERROR !  R TO RETRY "
 KEY DUP ASCII R =  SWAP ASCII R =
 OR NOT  ABORT" ABORTED"  ;


DEFER DISKERR  ' (DISKERR  IS DISKERR

DEFER R/W







\ *** Block No. 107, Hexblock 6b

\ BACKUP EMPTYBUF READBLK     11JUN85BP)
| : BACKUP   ( BUFADDR --)
 DUP 6+ @  0<
 IF  2+  DUP @ 1+               \ BUFFER EMPTY IF FILE = -1
  IF INPUT PUSH OUTPUT PUSH STANDARDI/O
   BEGIN DUP 6+ OVER 2+ @ 2 PICK @ 0 R/W
   WHILE ." WRITE " DISKERR
   REPEAT   THEN
  080 OVER 4+ 1+ CTOGGLE  THEN DROP ;

| : EMPTYBUF  ( BUFADDR --)   2+ DUP ON 4+ OFF ;

| : READBLK ( BLK FILE ADDR -- BLK FILE ADDR)
     DUP EMPTYBUF  INPUT PUSH  OUTPUT PUSH  STANDARDI/O >R
     BEGIN OVER OFFSET @ + OVER R@ 8 +  -ROT   1  R/W
     WHILE ." READ " DISKERR REPEAT  R>  ;

\ *** Block No. 108, Hexblock 6c

\ TAKE MARK UPDATES? FULL? CORE?     BP)

| : TAKE   ( -- BUFADDR)    PREV
     BEGIN  DUP @  WHILE  @  DUP 2+ @ -1 = UNTIL
     BUFFERS LOCK   DUP BACKUP ;

| : MARK ( BLK FILE BUFADDR -- BLK FILE )
     2+ >R 2DUP R@ !  OFFSET @ +  R@ 2+ !
     R> 4+ OFF  BUFFERS UNLOCK ;

| : UPDATES?  ( -- BUFADDR / FLAG)
     PREV  BEGIN  @ DUP  WHILE  DUP 6+ @ 0<  UNTIL ;

| : FULL?   ( -- FLAG)  PREV BEGIN @ DUP @ 0= UNTIL  6+ @ 0< ;

: CORE?  ( BLK FILE -- ADDR /FALSE)  (CORE? 2DROP FALSE ;

\ *** Block No. 109, Hexblock 6d

\ BLOCK & BUFFER MANIPULATION 11JUN85BP)

: (BUFFER ( BLK FILE -- ADDR)
    BEGIN  (CORE? TAKE MARK  REPEAT ;   -2 ALLOT

: (BLOCK  ( BLK FILE -- ADDR)
    BEGIN  (CORE? TAKE READBLK MARK REPEAT ;   -2 ALLOT

| CODE FILE@  ( -- N ) USER' FILE # LDY
    UP )Y LDA  PHA  INY  UP )Y LDA PUSH JMP  END-CODE

: BUFFER  ( BLK -- ADDR ) FILE@  (BUFFER ;

: BLOCK   ( BLK -- ADDR ) FILE@  (BLOCK ;



\ *** Block No. 110, Hexblock 6e

\ BLOCK & BUFFER MANIPULATION 09SEP84KS)

: UPDATE   080 PREV @  6+ 1+ C! ;

: SAVE-BUFFERS
   BUFFERS LOCK BEGIN   UPDATES? ?DUP WHILE BACKUP REPEAT
   BUFFERS UNLOCK  ;

: EMPTY-BUFFERS
   BUFFERS LOCK  PREV
   BEGIN @ ?DUP
   WHILE DUP EMPTYBUF
   REPEAT  BUFFERS UNLOCK ;

: FLUSH    SAVE-BUFFERS EMPTY-BUFFERS ;


\ *** Block No. 111, Hexblock 6f

\ MOVING BLOCKS               15DEC83KS)             cas 26jan06
| : (COPY   ( FROM TO --) DUP FILE@
     CORE? IF PREV @ EMPTYBUF THEN
     FULL? IF  SAVE-BUFFERS   THEN
     OFFSET @ + SWAP BLOCK 2- 2- !  UPDATE ;

| : BLKMOVE  ( FROM TO QUAN --) SAVE-BUFFERS >R
     OVER R@ + OVER U> >R  2DUP U< R> AND
     IF  R@ R@ D+  R> 0 ?DO -1 -2 D+ 2DUP (COPY LOOP
     ELSE          R> 0 ?DO 2DUP (COPY 1 1 D+   LOOP
     THEN  SAVE-BUFFERS 2DROP ;

: COPY    ( FROM TO --)   1 BLKMOVE ;

: CONVEY  ( [BLK1 BLK2] [TO.BLK --)
     SWAP  1+  2 PICK -   DUP 0> NOT ABORT" NO!!"  BLKMOVE ;

\ *** Block No. 112, Hexblock 70

\ ALLOCATING BUFFERS          23SEP83KS)               12jan13py

F000 CONSTANT LIMIT     VARIABLE FIRST

: ALLOTBUFFER   ( -- )
   FIRST @ R0 @  - B/BUF 2+ U< ?EXIT
   B/BUF NEGATE FIRST +! FIRST @ DUP EMPTYBUF
   PREV  @ OVER !  PREV !   ;

: FREEBUFFER    ( -- )
   FIRST @   LIMIT B/BUF - U<
   IF SAVE-BUFFERS BEGIN  DUP @ FIRST @  - WHILE  @  REPEAT
   FIRST @  @ SWAP ! B/BUF FIRST +! THEN ;

: ALL-BUFFERS BEGIN  FIRST @ ALLOTBUFFER FIRST @  = UNTIL ;


\ *** Block No. 113, Hexblock 71

\ ENDPOINTS OF FORGET      04JAN85BP/KS)
| : \? ( NFA -- FLAG )   C@  020  AND ;

| : FORGET?  ( ADR NFA -- FLAG ) \ CODE IN HEAP OR ABOVE ADR ?
     NAME> UNDER 1+ U< SWAP  HEAP?  OR ;

|  : ENDPOINTS  ( ADDR -- ADDR SYMB)
 HEAP   VOC-LINK @ >R
  BEGIN R> @ ?DUP    \ THROUGH ALL VOCABS
  WHILE DUP >R 4 - >R \ LINK ON RETURNST.
   BEGIN R> @ >R OVER 1- DUP R@  U<    \ UNTIL LINK  OR
              SWAP  R@ 2+ NAME> U< AND \ CODE UNDER ADR
   WHILE  R@ HEAP?  [ 2DUP ] UNTIL  \ SEARCH FOR A NAME IN HEAP
    R@ 2+ \?  IF  OVER R@ 2+ FORGET?
               IF R@ 2+ (NAME> 2+ UMAX THEN \ THEN UPDATE SYMB
              THEN REPEAT RDROP   REPEAT  ;

\ *** Block No. 114, Hexblock 72

\ REMOVE                       23JUL85WE

| CODE REMOVE ( DIC SYMB THR - DIC SYMB)
   5 # LDY [[ SP )Y LDA N ,Y STA DEY 0< ?] USER' S0 # LDY
   CLC UP )Y LDA 6 # ADC N 6 + STA
   INY UP )Y LDA 0 # ADC N 7 + STA  1 # LDY
   [[ N X) LDA N 8 + STA N )Y LDA N 9 + STA N 8 + ORA  0<>
   ?[[ N 8 + LDA N 6 + CMP N 9 + LDA N 7 + SBC CS
     ?[ N 8 + LDA N 2 + CMP N 9 + LDA N 3 + SBC
     ][ N 4 + LDA N 8 + CMP N 5 + LDA N 9 + SBC
     ]? CC
     ?[ N 8 + X) LDA N X) STA N 8 + )Y LDA N )Y STA
     ][ N 8 + LDA    N    STA N 9 + LDA N 1+ STA ]?
   ]]? (DROP JMP   END-CODE



\ *** Block No. 115, Hexblock 73

\ REMOVE-     FORGET-WORDS    29APR85BP)

| : REMOVE-WORDS ( DIC SYMB -- DIC SYMB)
     VOC-LINK BEGIN @ ?DUP WHILE DUP >R 4 - REMOVE R> REPEAT  ;

| : REMOVE-TASKS  ( DIC --)
     UP@  BEGIN  1+ DUP @ UP@ - WHILE  2DUP @ SWAP HERE UWITHIN
     IF DUP @ 1+ @ OVER ! 1-  ELSE  @ THEN REPEAT  2DROP ;

| : REMOVE-VOCS  ( DIC SYMB -- DIC SYMB)
     VOC-LINK REMOVE THRU.VOCSTACK
      DO  2DUP I @  -ROT  UWITHIN
        IF   [ ' FORTH 2+ ] LITERAL I ! THEN -2 +LOOP
      2DUP   CURRENT @  -ROT   UWITHIN
     IF [ ' FORTH 2+ ] LITERAL CURRENT ! THEN ;


\ *** Block No. 116, Hexblock 74

\  FORGET-WORDS                                      cas 26jan06

| : FORGET-WORDS    ( DIC SYMB --)
     OVER REMOVE-TASKS REMOVE-VOCS
          REMOVE-WORDS
     HEAP SWAP - HALLOT DP !  0 LAST ! ;











\ *** Block No. 117, Hexblock 75

\ DELETING WORDS FROM DICT.   13JAN83KS)

: CLEAR HERE   DUP UP@ FORGET-WORDS   DP ! ;

: (FORGET ( ADR --) DUP  HEAP? ABORT" IS SYMBOL"
    ENDPOINTS FORGET-WORDS ;

: FORGET  ' DUP [ DP ] LITERAL @ U<  ABORT" PROTECTED"
   >NAME DUP HEAP? IF  NAME>  ELSE  2- 2-  THEN (FORGET ;

: EMPTY  [ DP ] LITERAL @
   UP@ FORGET-WORDS [ UDP ] LITERAL @  UDP ! ;





\ *** Block No. 118, Hexblock 76

\ SAVE BYE STOP? ?CR       20OCT84KS/BP)

: SAVE
   HERE UP@ FORGET-WORDS VOC-LINK @
   BEGIN  DUP 2- 2-  @  OVER 2- !  @ ?DUP 0=  UNTIL
   UP@ ORIGIN 0100 CMOVE   ;

: BYE       FLUSH EMPTY (BYE ;

| : END?    KEY   #CR   (C 3 ) = IF TRUE RDROP THEN ;

: STOP?   ( -- FLAG) KEY? IF END? END? THEN FALSE ;

: ?CR   COL  C/L 0A -  U> IF CR THEN ;



\ *** Block No. 119, Hexblock 77

\ IN/OUTPUT STRUCTURE         02MAR85BP)
| : OUT:  CREATE DUP C,  2+ DOES> C@ OUTPUT @ +  PERFORM ;

  : OUTPUT:  CREATE  ]      DOES>  OUTPUT ! ;
0  OUT: EMIT   OUT: CR     OUT: TYPE
   OUT: DEL    OUT: PAGE   OUT: AT    OUT: AT?  DROP

: ROW   ( -- ROW)  AT? DROP ;
: COL   ( -- COL)  AT? NIP ;

| : IN:    CREATE DUP C, 2+ DOES> C@ INPUT @ + PERFORM ;

  : INPUT:  CREATE ]        DOES> INPUT ! ;

0  IN: KEY   IN: KEY?   IN: DECODE  IN: EXPECT   DROP


\ *** Block No. 120, Hexblock 78

\ ALIAS  ONLY DEFINITIONEN    29JAN85BP)

ONLY DEFINITIONS FORTH

: SEAL  0 ['] ONLY  >BODY  ! ;  \ KILL ALL WORDS IN ONLY)

      ' ONLY  ALIAS ONLY
      ' FORTH ALIAS FORTH
      ' WORDS ALIAS WORDS
      ' ALSO  ALIAS ALSO
' DEFINITIONS ALIAS DEFINITIONS
HOST TARGET





\ *** Block No. 121, Hexblock 79

\ 'COLD                       07JUN85BP)        cas    02aug2020
| : INIT-VOCABULARYS   VOC-LINK @
     BEGIN  DUP  2- @  OVER 4 - ! @ ?DUP 0= UNTIL ;

| : INIT-BUFFERS 0 PREV ! LIMIT FIRST !  ALL-BUFFERS ;

DEFER  'COLD    ' NOOP IS 'COLD

| : (COLD INIT-VOCABULARYS  INIT-BUFFERS  PAGE 'COLD ONLYFORTH
   ." volksFORTH-83  3.8.1 py65 202008" CR   RESTART ; -2 ALLOT

DEFER 'RESTART  ' NOOP IS 'RESTART
| : (RESTART ['] (QUIT IS 'QUIT
    DRVINIT 'RESTART  [ ERRORHANDLER ] LITERAL @ ERRORHANDLER !
    [']  NOOP IS 'ABORT ABORT  ;  -2 ALLOT


\ *** Block No. 122, Hexblock 7a

\ COLD BOOTSYSTEM RESTART     09JUL85WE)
CODE COLD        HERE >COLD !
 ' (COLD >BODY 100 U/MOD # LDA PHA  # LDA PHA

LABEL BOOTSYSTEM  CLI 0 # LDY
 CLC S0 LDA 6 # ADC N STA S0 1+ LDA 0 # ADC N 1+ STA
 [[ ORIGIN ,Y LDA N )Y STA INY 0= ?]
LABEL WARMBOOT  BOOTNEXTLEN 1- # LDY
 [[ BOOTNEXT ,Y LDA PUTA ,Y STA DEY 0< ?]
 CLC S0 LDA 6 # ADC UP STA S0 1+ LDA 0 # ADC UP 1+ STA
 USER' S0 # LDY  UP )Y LDA SP STA INY  UP )Y LDA SP 1+ STA
 USER' R0 # LDY  UP )Y LDA RP STA INY  UP )Y LDA RP 1+ STA
 0 # LDX 1 # LDY TXA RP X) STA RP )Y STA
 PLA IP STA PLA IP 1+ STA
LABEL XYNEXT 0 # LDX 1 # LDY NEXT JMP END-CODE


\ *** Block No. 123, Hexblock 7b

\ ( RESTART  PARAM.-PASSING TO FORTH   BP)

CODE RESTART       HERE >RESTART !
 ' (RESTART >BODY 100 U/MOD
 # LDA  PHA  # LDA PHA WARMBOOT JMP   END-CODE












\ *** Block No. 124, Hexblock 7c

\ CODE FOR PARAMETER-PASSING TO FORTH                cas 26jan06

\ Include system dependent Input / Output code
\ (Keyboard and Screen)
include systemio.fb


HOST  ' TRANSIENT 8 + @
TRANSIENT  FORTH  CONTEXT @ 6 + !
TARGET

FORTH ALSO DEFINITIONS

: FORTH-83 ;  \ LAST WORD IN DICTIONARY



\ *** Block No. 125, Hexblock 7d

\ SYSTEM DEPENDENT CONSTANTS      BP/KS)

VOCABULARY ASSEMBLER
ASSEMBLER DEFINITIONS
TRANSIENT  ASSEMBLER
PUSHA  CONSTANT PUSHA           \ PUT A SIGN-EXTENDED ON STACK
PUSH0A CONSTANT PUSH0A          \ PUT A ON STACK
PUSH   CONSTANT PUSH            \ MSB IN A AND LSB ON JSR-STACK
RP     CONSTANT RP
UP     CONSTANT UP
SP     CONSTANT SP
IP     CONSTANT IP
N      CONSTANT N
PUTA   CONSTANT PUTA
W      CONSTANT W
SETUP  CONSTANT SETUP

\ *** Block No. 126, Hexblock 7e

\
NEXT   CONSTANT NEXT
XYNEXT CONSTANT XYNEXT
(2DROP CONSTANT POPTWO
(DROP  CONSTANT POP












\ *** Block No. 127, Hexblock 7f

\ SYSTEM PATCHUP              05JAN85BP)             cas 26jan06

FORTH DEFINITIONS

\ change memory layout for stacks and buffers here
6000 ' LIMIT >BODY !
$5800 S0 !  $5B00 R0 !

S0 @ DUP S0 2- !      6 + S0 7 - !
HERE DP !

HOST  TUDP @          TARGET  UDP !
HOST  TVOC-LINK @     TARGET  VOC-LINK !
HOST  MOVE-THREADS



\ *** Block No. 128, Hexblock 80


















\ *** Block No. 129, Hexblock 81


















\ *** Block No. 130, Hexblock 82


















\ *** Block No. 131, Hexblock 83

















