
\ *** Block No. 0, Hexblock 0

***************** CRAPS *****************************jrg 06okt88
nach Wil Baden

Da es in Deutschland das Würfelspiel CRAPS nicht gibt, habe ich
diesem Begriff ein Würfel- und Trinkspiel aus der Schulzeit
unterlegt.
Bei diesem Spiel steht in der Tischmitte ein Vorrat an gefüllten
Gläsern. Danach soll ein Mitspieler abhängig von seinem Wurf

entweder   ein neues Glas aus der Tischmitte vor sich stellen
oder       eines seiner Gläser seinem Nachbarn zur linken oder
           zur rechten zuschieben
oder       alle vor ihm stehenden Gläser austrinken.

Zuordnung: 1=nehmen, 2/3=links, 4/5=rechts, 6 trinken


\ *** Block No. 1, Hexblock 1

\ nehmen  trinken  links  rechts  schieben           jrg 03feb89

: nehmen   bright ." ein Glas nehmen"        normal  2 spaces ;
: trinken  bright ." alle Gläser austrinken" normal  2 spaces ;
: links    bright ." ein Glas nach LINKS"    normal  2 spaces ;
: rechts   bright ." ein Glas nach RECHTS"   normal  2 spaces ;

: schieben  ;


: Anfrage      ." Sollen Sie nehmen, trinken oder schieben? "
               cr ." Bitte Ihre Augenzahl und <cr> : "   ;

: Glückwunsch  cr ." Viel Glück beim nächsten Wurf ... " ;

cr .( Sprüche geladen )

\ *** Block No. 2, Hexblock 2

\ Auswertung mit IF...THEN / ENDIF                   jrg 03feb89

' THEN Alias ENDIF  immediate restrict

: Auswertung  ( 1<= Wurfergebnis <=6  -- )

     dup 1 = IF nehmen          ENDIF
     dup 2 = IF links  schieben ENDIF
     dup 3 = IF links  schieben ENDIF
     dup 4 = IF rechts schieben ENDIF
     dup 5 = IF rechts schieben ENDIF
     dup 6 = IF trinken         ENDIF

     1 6 between not  IF invers ." Betrug!" normal ENDIF
;


\ *** Block No. 3, Hexblock 3

\ =or                                                jrg 06okt88

  code =or       ( n1 f1 n2 -- n1 f2 )
     A D xchg  D pop
     S W mov
       W ) A cmp
       0= ?[ -1 # D mov ]?
    next
  end-code

\ : =or  ( n1 f1 n2 -- n1 f2 )  2 pick = or ;






\ *** Block No. 4, Hexblock 4

\  Auswertung mit IF...THEN  und =or                 jrg 06okt88

: Auswertung  ( 1<= Wurfergebnis <=6  -- )
     dup
     1 6 between IF
                  dup 1 =       IF  nehmen   ENDIF
                  dup 6 =       IF  trinken  ENDIF
                  dup 2 = 3 =or IF  links    ENDIF
                  dup 4 = 5 =or IF  rechts   ENDIF
                 ELSE
                  invers ." Betrugsversuch" normal
                 ENDIF
     drop
;



\ *** Block No. 5, Hexblock 5

\ CASE   OF  ENDOF   END-CASE     BREAK              jrg 30mai89

 : CASE      ( n -- n n )  dup ;  restrict

 : OF        [compile] IF compile drop ;  immediate restrict

 : ENDOF     [compile] ELSE 4+ ;  immediate restrict

 : ENDCASE   compile drop
             BEGIN
               3 case?
             WHILE
               >resolve
             REPEAT ;  immediate restrict

 : BREAK     compile exit  [compile] THEN ;  immediate restrict

\ *** Block No. 6, Hexblock 6

\  Auswerten mit CASE OF ENDOF ENDCASE              jjrg 05feb89

: Auswertung  ( 1<= n <=6  -- )

    dup  1 6 between  not IF ." Betrug" drop exit ENDIF

    CASE 1 =  OF nehmen  ENDOF
    CASE 6 =  OF trinken ENDOF
    CASE 4 <  OF links   ENDOF
    CASE 3 >  OF rechts  ENDOF
    ENDCASE
;



\ Man beachte die Stellung der Plausibilitätsprüfung

\ *** Block No. 7, Hexblock 7

\  Auswerten mit =or und BREAK                       jrg 05feb89

: Auswertung  ( 1<= n <=6  -- )

    CASE 1 =        OF  nehmen    BREAK
    CASE 2 = 3 =or  OF  links     BREAK
    CASE 4 = 5 =or  OF  rechts    BREAK
    CASE 6 =        OF  trinken   BREAK
    ENDCASE

  invers ." Betrugsversuch" normal
;





\ *** Block No. 8, Hexblock 8

\ Das CRAPS Programm wie in PASCAL etc.              jrg 07okt88

: CRAPS

    cr  Anfrage cr

          input#
          Auswertung

    cr  Glückwunsch
;






\ *** Block No. 9, Hexblock 9

\ ------------- VECTOR EXECUTION --------------------jrg 07okt88
















\ *** Block No. 10, Hexblock a

\  4TH braucht Prozeduren                            jrg 05feb89


: bewegen  ( adr n -- cfa )
    2* +  perform ;

: richtig  ( n  --  0<= n <= 3 )
    swap
    1 max  6 min                        \ ein bißchen Sicherheit
    3 case? IF  2  1- exit  ENDIF
    5 case? IF  4  1- exit  ENDIF
    1-  ;                               \ ein bißchen Justage





\ *** Block No. 11, Hexblock b

\  Die möglichen Bewegungen mit  ] [  oder Create:   jrg 05feb89

Create  Glas
        ]  nehmen  links schieben
                   rechts schieben  trinken  [


\ oder:

Create: Glas
         nehmen
         links schieben
         rechts schieben
         trinken ;



\ *** Block No. 12, Hexblock c

\  Create: ; :Does>                                  jrg 05feb89

Create: Auswertung
          nehmen
           links schieben
           rechts schieben
          trinken ;
      :Does>
         richtig bewegen ;








\ *** Block No. 13, Hexblock d

\  Das vollständige Programm                         jrg 05feb89

: CRAPS
     cr  Anfrage cr
           input#
           Glas richtig bewegen
     cr  Glückwunsch
;

\ ausschließlich als Datenstruktur
: CRAPS
     cr  Anfrage cr
           input#
           Auswertung
     cr  Glückwunsch
;

\ *** Block No. 14, Hexblock e

\ #### positional CASE def.words  Case: Associative: jrg 01feb89
: Case:     ( -- )
   Create:
   Does>    ( pfa -- )  swap 2* +  perform ;

: Associative:           ( n -- )
   Constant   Does>      ( n - index )
   dup @ -rot
   dup @ 0
   DO  2+  2dup  @ =
       IF  2drop drop  I  0 0  LEAVE  THEN
   LOOP 2drop ;

\ alternative Definition für CASE:
  : Case:
      :  Does>   ( pfa -- )  swap 2* +  perform ;

\ *** Block No. 15, Hexblock f

\  CASE: in der Anwendung                            jrg 01feb89

         Case:  bewegen                   \ besteht aus :
                   nehmen
                    links  links
                    rechts rechts
                   trinken ;


6 Associative:  auswerten

                  1 ,
                2 , 3 ,
                4 , 5 ,
                  6 ,


\ *** Block No. 16, Hexblock 10

\  CASE:  und  Associative:                          jrg 01feb89

: CRAPS   ( -- )

     cr  Anfrage cr
         input#
            auswerten
            bewegen
     cr  Glückwunsch
;







\ *** Block No. 17, Hexblock 11

************* Beginn der Kommentare *****************jrg 07okt88






SCHIEBEN   gefällt mir deshalb so gut, weil es vorher nur als
           Füllsel arbeitet, aber hinterher als Dummy in der
           Tabelle die wichtige Funktion hat, sechs mögliche
           Würfe sauber abzuarbeiten.






\ *** Block No. 18, Hexblock 12

\\ Auswertung mit IF...THEN / ENDIF                  jrg 01feb89

ENDIF  macht deutlich(er), warum FORTH ohne Verbundanweisung wie
       z.B. PASCAL auskommt.
AUSWERTUNG führt entsprechend einem Selektor genau eine von 6
       möglichen Prozeduren aus.

Auch eine mögliche Form der Auswertung mit CASE? :
: Auswertung  ( 1<= Wurfergebnis <=6  -- )
      1 case? IF nehmen           exit ENDIF
      2 case? IF links  schieben  exit ENDIF
      3 case? IF links  schieben  exit ENDIF
      4 case? IF rechts schieben  exit ENDIF
      5 case? IF rechts schieben  exit ENDIF
      6 case? IF trinken          exit ENDIF
  1 6 between not  IF invers ." Betrugsversuch" normal ENDIF ;

\ *** Block No. 19, Hexblock 13

\\ hilfreiche Prozeduren für das kommende CASE

=OR     prüft eine Zahl n2 auf Gleichheit mit einem Testwert n1
        und verknüpft resultierende Ergebnis mit einem bereits
        vorliegenden flag f1. Es werden das neue flag f2 und der
        "alte" Testwert n1 übergeben.











\ *** Block No. 20, Hexblock 14

\\ bedingte Verzweigung mit IF .. ELSE .. ENDIF
















\ *** Block No. 21, Hexblock 15

\\ Die Definitionen für die CASE Anweisung           jrg 07okt88














BREAK   ist ein EXIT aus der CASE-Anweisung; return to caller

\ *** Block No. 22, Hexblock 16

\\ Auswertung mit CASE OF ENDOF



Sicherheit gegen falsche Zahlen












\ *** Block No. 23, Hexblock 17

\\ Die elegantere Auswertung mit BREAK               jrg 07okt88

BREAK  = Verlassen des Callee







Wird trotz BREAK dieser Prozedurteil erreicht, muß die Zahl un-
gültig gewesen sein.





\ *** Block No. 24, Hexblock 18

                                                     jrg 07okt88
















\ *** Block No. 25, Hexblock 19

\ Ui jui jui / Test für ein CRAPS

  : Test
      full page
      20 0 DO
            craps
           LOOP ;










\ *** Block No. 26, Hexblock 1a

\\ Für Datenobjekte sind Prozeduren notwendig
















\ *** Block No. 27, Hexblock 1b

\\ Was sind denn die Datenobjekte ?                  jrg 07okt88

GLAS    als Datenteil enthält natürlich die in Frage kommenden
        Prozeduren.




GLAS    ist der gleiche Datenteil wie oben, nur eleganter.








\ *** Block No. 28, Hexblock 1c

\\ Zusammenfassen des Datenteils und des Zugriffsteiljrg 07okt88







RICHTIG und BEWEGEN sind die eigens für den Datenteil GLAS ent-
        worfenen Zugriffsprozeduren. Deshalb bietet es sich an,
        diese mit GLAS zusammenzufügen.






\ *** Block No. 29, Hexblock 1d


















\ *** Block No. 30, Hexblock 1e


















\ *** Block No. 31, Hexblock 1f


















\ *** Block No. 32, Hexblock 20

















