
\ *** Block No. 0, Hexblock 0

* EAKER - CASE für volks4th83  von Heinz Schnitter **jrg 01feb89
















\ *** Block No. 1, Hexblock 1

\ Vorwärtsreferenzen als verkettete Liste       ( 06.jrg 01feb89
| variable caselist

| : initlist    ( list -- addr )
                dup @ swap off
  ;

| : >marklist   ( list -- )
                here over @ , swap !
  ;

| : >resolvelist ( addr list -- )
                BEGIN dup @
                WHILE dup dup @ dup @ rot ! >resolve
                REPEAT !
  ;

\ *** Block No. 2, Hexblock 2

\ CASE  ELSECASE  ENDCASE                       ( 09.jrg 01feb89

: CASE          caselist initlist 4
; immediate restrict


: ELSECASE      4 ?pairs
                compile drop 6
; immediate restrict


: ENDCASE       dup 4 =
                IF   drop compile drop
                ELSE 6 ?pairs
                THEN caselist >resolvelist
; immediate restrict

\ *** Block No. 3, Hexblock 3

\ OF  ENDOF  Control                            ( 09.jrg 01feb89
: OF            4 ?pairs
                compile over
                compile =
                compile ?branch
                >mark compile drop 5
; immediate restrict

: ENDOF         5 ?pairs
                compile branch
                caselist >marklist
                >resolve 4
; immediate restrict

: Control       bl word 1+ c@ $bf and state @
                IF [compile] Literal THEN ; immediate

\ *** Block No. 4, Hexblock 4

\ Test                                          ( 09.jrg 01feb89
: test
  ." exit mit ctrl x" cr
  BEGIN key
        CASE control A OF ." action ^a " cr false ENDOF
             control B OF ." action ^b " cr false ENDOF
             control C OF ." action ^c " cr false ENDOF
             control D OF ." action ^d " cr false ENDOF
             control X OF ." exit "         true  ENDOF
        ELSECASE
                   ." befehl unbekannt " cr false
        ENDCASE
  UNTIL
;



\ *** Block No. 5, Hexblock 5

\ nehmen  trinken  links  rechts  schieben           jrg 01feb89

: nehmen   bright ." ein Glas nehmen"        normal  2 spaces ;
: trinken  bright ." alle Gläser austrinken" normal  2 spaces ;
: links    bright ." ein Glas nach LINKS"    normal  2 spaces ;
: rechts   bright ." ein Glas nach RECHTS"   normal  2 spaces ;

: schieben  ;


: Anfrage  ." Sollen Sie nehmen, trinken oder schieben? "
       cr  ." Bitte Ihre Augenzahl und <cr> : "   ;

: Glückwunsch  ." Viel Glück beim nächsten Wurf ... " ;

cr .( Sprüche geladen )

\ *** Block No. 6, Hexblock 6

\  Auswerten mit CASE OF ENDOF END-CASE              jrg 01feb89

: Auswertung  ( 1<= n <=6  -- )

    CASE 1  OF nehmen  ENDOF
         6  OF trinken ENDOF
         4  OF links   ENDOF
         5  OF links   ENDOF
         2  OF rechts  ENDOF
         3  OF rechts  ENDOF
    ELSECASE
         ." Betrug! "
    ENDCASE
;



\ *** Block No. 7, Hexblock 7

\ Das CRAPS Programm wie in PASCAL etc.              jrg 07okt88

: CRAPS

    cr  Anfrage cr

          input#
          Auswertung

    cr  Glückwunsch
;






\ *** Block No. 8, Hexblock 8

















