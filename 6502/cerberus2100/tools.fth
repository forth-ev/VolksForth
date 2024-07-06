
\ *** Block No. 0, Hexblock 0

\ Development Tools                                  cas 26jan06

Interactive Tracer

One-Step Debugger

Traps










\ *** Block No. 1, Hexblock 1

\ TOOLS LOADSCREEN            22MAR85RE)

ONLYFORTH

\NEEDS CODE  abort( Assembler is needed )

VOCABULARY TOOLS

TOOLS ALSO DEFINITIONS
hex
1 &11 +THRU
decimal
ONLYFORTH




\ *** Block No. 2, Hexblock 2

\ HANDLE STEPS              BP 10 02 85)

ASSEMBLER ALSO DEFINITIONS

ONLY FORTH ALSO TOOLS ALSO DEFINITIONS
| VARIABLE (W     | VARIABLE RPT

| CODE STEP
 RPT DEC   RP X) LDA  IP STA
 RP )Y LDA  IP 1+ STA  RP 2INC
 (W LDA  W STA   (W 1+ LDA   W 1+ STA
 W 1- JMP  END-CODE

| CREATE NEXTSTEP  ] STEP [



\ *** Block No. 3, Hexblock 3

\ THROW STATUS ON R-STACK  B  23JUL85RE)

| CREATE NPULL    0  ]
    RP@ COUNT 2DUP + RP! R> SWAP CMOVE ;

: NPUSH  ( ADDR LEN -)
    R> -ROT   OVER  >R RP@ OVER 1+ - DUP RP!  PLACE
    NPULL >R  >R ;

| : ONELINE .STATUS  SPACE  QUERY  INTERPRET
     -82 ALLOT  RDROP ( DELETE QUIT FROM TNEXT )  ;






\ *** Block No. 4, Hexblock 4

\ TRAP AND DISPLAY        KS  26MAR85RE)
LABEL  TNEXT
 IP 2INC  RP LDA   RPT CMP  0<> ?[
  [[  W 1- JMP             SWAP ]?
      RP 1+ LDA  RPT 1+ CMP  0= ?]
LABEL DOTRACE
  RPT INC   ( DISABLE TRACER )
  W LDA  (W STA    W 1+ LDA   (W 1+ STA
 ;C:  R@  NEXTSTEP >R
 INPUT PUSH  KEYBOARD
 OUTPUT PUSH  DISPLAY
 CR 2-  DUP 4 U.R  @ DUP 5 U.R 2 SPACES
 >NAME .NAME  1C COL - 0 MAX SPACES .S
 STATE PUSH  BLK PUSH  >IN PUSH
 [ ' 'QUIT      >BODY ] LITERAL  PUSH
 [ ' >INTERPRET >BODY ] LITERAL  PUSH

\ *** Block No. 5, Hexblock 5

\
 #TIB PUSH  TIB #TIB @ NPUSH  R0 PUSH
 RP@ R0 !  082 ALLOT
 ['] ONELINE IS 'QUIT  QUIT ;  -2 ALLOT













\ *** Block No. 6, Hexblock 6

\ TRACER COMMANDS         BP  23JUL85RE)

| CODE (TRACE TNEXT 0 100 M/MOD
     # LDA  NEXT 0C + STA
     # LDA  NEXT 0B + STA
 04C # LDA  NEXT 0A + STA  NEXT JMP END-CODE

: TRACE' RP@ 2- RPT ! ' (TRACE EXECUTE  END-TRACE ;

: BREAK  RP@ 2+ RPT !  (TRACE  ;  RESTRICT

: TRACEL:   CREATE ,  DOES>  @ RPT +! ;

-6 TRACEL: +DO      6 TRACEL: -DO
-2 TRACEL: +R       2 TRACEL: -R
-6 TRACEL: +PUSH    6 TRACEL: -PUSH

\ *** Block No. 7, Hexblock 7

\ WATCH TRAP               BP 10 02 85 )

| VARIABLE WATCHPT   2 ALLOT

LABEL WNEXT IP 2INC
 WATCHPT    LDA  N STA  WATCHPT 1+ LDA  N 1+ STA
 N X) LDA  WATCHPT 2+ CMP        0<> ?[
  [[  RP    LDA  RPT    STA  RP 1+ LDA  RPT 1+ STA
      ( SET TO TNEXT)   TNEXT 0 100 M/MOD
      # LDA  NEXT 0C + STA # LDA  NEXT 0B + STA
      DOTRACE JMP               SWAP ]?
      N )Y LDA  WATCHPT 3 + CMP   0= ?] W 1- JMP   END-CODE





\ *** Block No. 8, Hexblock 8

\ WATCH COMMANDS           BP 10 02 85 )

| CODE (WATCH WNEXT 0 100 M/MOD
     # LDA  NEXT 0C + STA
     # LDA  NEXT 0B + STA
 04C # LDA  NEXT 0A + STA  NEXT JMP END-CODE

: WATCH'  ( ADR -- )
   DUP  WATCHPT ! @ WATCHPT 2+ ! ' (WATCH  EXECUTE  END-TRACE ;

: CONT  ( -)  WATCHPT @ @ WATCHPT 2+ !  (WATCH ;

( SYNTAX : <VARNAME> WATCH' <PROCEDURE>   )




\ *** Block No. 9, Hexblock 9

\ TOOLS FOR DECOMPILING,   KS 4 APR 83 )
( INTERACTIVE USE                      )
| : ?:  DUP 4 U.R ." :"  ;
| : @?  DUP @ 6 U.R ;
| : C?  DUP C@ 3 .R ;
| : BL  024 COL - 0 MAX SPACES ;

: S  ( ADR - ADR+)  ( PRINT LITERAL STRING)
        ?:  SPACE C? 4 SPACES DUP COUNT TYPE
        DUP C@ + 1+ BL  ;  ( COUNT + RE)

: N  ( ADR - ADR+2) ( PRINT NAME OF NEXT WORD BY ITS CFA)
      ?: @? 2 SPACES DUP @ >NAME .NAME 2+ BL ;

: L  ( ADR - ADR+2) ( PRINT LITERAL VALUE) ?: @? 2+ BL ;


\ *** Block No. 10, Hexblock a

\ TOOLS FOR DECOMPILING, INTERACTIVE   )

: D  ( ADR N - ADR+N) ( DUMP N BYTES)
     2DUP SWAP ?: 3 SPACES  SWAP 0 DO  C? 1+ LOOP
     4 SPACES -ROT TYPE BL ;

: C  ( ADR - ADR+1) ( PRINT BYTE AS UNSIGNED VALUE) 1 D ;

: B  ( ADR - ADR+2) ( PRINT BRANCH TARGET LOCATION )
     ?: @? DUP @  OVER + 6 U.R 2+ BL  ;

( USED FOR : )
( NAME STRING LITERAL DUMP CLIT BRANCH )
( -    -      -       -    -    -      )



\ *** Block No. 11, Hexblock b

\ DEBUGGING UTILITIES      BP 19 02 85 )

: UNRAVEL   \  UNRAVEL PERFORM (ABORT"
    RDROP RDROP RDROP CR ." TRACE DUMP IS "  CR

    BEGIN  RP@   R0 @ -
    WHILE   R>  DUP  8 U.R  SPACE 2- @  >NAME .NAME  CR
    REPEAT (ERROR ;

' UNRAVEL ERRORHANDLER !







\ *** Block No. 12, Hexblock c


















\ *** Block No. 13, Hexblock d


















\ *** Block No. 14, Hexblock e

















