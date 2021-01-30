
\ *** Block No. 0, Hexblock 0


















\ *** Block No. 1, Hexblock 1

\ F83-number?  input#                                jrg 05feb89

  : F83-number?   ( string -- d f )
       number? ?dup  IF  0< IF extend ENDIF
                         true exit
                     THEN  drop  0 0  false ;

  : input#   ( <string> -- n )
       pad c/l 1- >expect
       pad F83-number?  2drop ;







\ *** Block No. 2, Hexblock 2

\ nehmen  trinken  links  rechts  schieben           jrg 05feb89

: nehmen   bright ." ein Glas nehmen"        normal  2 spaces ;
: trinken  bright ." alle Gläser austrinken" normal  2 spaces ;
: links    bright ." ein Glas nach LINKS"    normal  2 spaces ;
: rechts   bright ." ein Glas nach RECHTS"   normal  2 spaces ;

: schieben  ;


: Anfrage      cr ." Sollen Sie nehmen, trinken oder schieben? "
               cr ." Bitte Ihre Augenzahl und <cr> : "   ;

: Glückwunsch  cr ." Viel Glück beim nächsten Wurf ... " ;

cr .( Sprüche geladen )

\ *** Block No. 3, Hexblock 3

\ Auswertung.1 mit IF...ELSE...THEN                  jrg 05feb89

: Auswertung.1  ( Wurfergebnis --)

     dup 1 = IF nehmen ELSE
             dup 2 = IF links schieben ELSE
             dup 3 = IF links schieben ELSE
                     dup 4 = IF rechts schieben ELSE
                     dup 5 = IF rechts schieben ELSE
                             dup 6 = IF trinken THEN
                             THEN
                             THEN
                     THEN
                     THEN
             THEN
     1 6 between not IF invers ." Betrug!" normal ENDIF ;

\ *** Block No. 4, Hexblock 4

\ Auswertung.2 mit IF...THEN / ENDIF                 jrg 05feb89

' THEN Alias ENDIF  immediate restrict

: Auswertung.2  ( Wurfergebnis --)

     dup 1 = IF nehmen          ENDIF
     dup 2 = IF links  schieben ENDIF
     dup 3 = IF links  schieben ENDIF
     dup 4 = IF rechts schieben ENDIF
     dup 5 = IF rechts schieben ENDIF
     dup 6 = IF trinken         ENDIF

     1 6 between not IF invers ." Betrug!" normal ENDIF
;


\ *** Block No. 5, Hexblock 5

\ Auswertung.3 mit IF...ENDIF  und  CASE?            jrg 05feb89

: Auswertung.3   ( Wurfergebnis --)

      1 case? IF nehmen           exit ENDIF
      2 case? IF links  schieben  exit ENDIF
      3 case? IF links  schieben  exit ENDIF
      4 case? IF rechts schieben  exit ENDIF
      5 case? IF rechts schieben  exit ENDIF
      6 case? IF trinken          exit ENDIF

  1 6 between not IF
                   invers ." Betrugsversuch" normal
                  ENDIF
;


\ *** Block No. 6, Hexblock 6

\ =or                                                jrg 06okt88

  code =or       ( n1 f1 n2 -- n1 f2 )
     A D xchg  D pop
     S W mov
       W ) A cmp
       0= ?[ -1 # D mov ]?
    next
  end-code

\ : =or  ( n1 f1 n2 -- n1 f2 )  2 pick = or ;






\ *** Block No. 7, Hexblock 7

\ Auswertung.4 mit IF...THEN  und =or                jrg 05feb89

: Auswertung.4  ( Wurfergebnis --)
     dup
     1 6 between IF
                  dup 1 =       IF  nehmen           ENDIF
                  dup 2 = 3 =or IF  links schieben   ENDIF
                  dup 4 = 5 =or IF  rechts schieben  ENDIF
                  dup 6 =       IF  trinken          ENDIF
                 ELSE
                  invers ." Betrug!" normal
                 ENDIF
     drop
;



\ *** Block No. 8, Hexblock 8

****** Beginn der Kommentare ************************jrg 05feb89
















\ *** Block No. 9, Hexblock 9

                                                     jrg 03feb89
















\ *** Block No. 10, Hexblock a

\\ So ist es schrecklich !                           jrg 03feb89
















\ *** Block No. 11, Hexblock b

\\ ENDIF  und  CASE?                                 jrg 03feb89

ENDIF  macht deutlich(er), warum FORTH ohne Verbundanweisung wie
       z.B. PASCAL auskommt.
AUSWERTUNG führt entsprechend einem Selektor genau eine von 6
       möglichen Prozeduren aus.











\ *** Block No. 12, Hexblock c

                                                     jrg 03feb89
















\ *** Block No. 13, Hexblock d

\\ =OR                                               jrg 03feb89

=OR     prüft eine Zahl n2 auf Gleichheit mit einem Testwert n1
        und verknüpft resultierende Ergebnis mit einem bereits
        vorliegenden flag f1. Es werden das neue flag f2 und der
        "alte" Testwert n1 übergeben.










