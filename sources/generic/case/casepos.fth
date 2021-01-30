
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

\ LoadScreen                                         jrg 31dez89

\needs :Does>    2 load

  8 load
  cr .( positionelles CASE geladen ) cr











\ *** Block No. 2, Hexblock 2

\ :Does>  für  Create <name> :Does> ... ;ks 25 aug 88jrg 31dez89

| : (does>   here >r [compile] Does> ;

  : :Does>    last @ 0= Abort" without reference"
     (does> current @ context !   hide 0 ] ;

clear
\\
: test   cls
     5 0 DO
          cr ." craps1 " I . ." mal" craps1
          cr ." craps2 " I . ." mal" craps2
          cr ." craps3 " I . ." mal" craps3
         LOOP
    cr ." fertig."  ;

\ *** Block No. 3, Hexblock 3

\ nehmen  trinken  links  rechts  schieben           jrg 05feb89

: nehmen   bright ." ein Glas nehmen"        normal  2 spaces ;
: trinken  bright ." alle Gläser austrinken" normal  2 spaces ;
: links    bright ." ein Glas nach LINKS"    normal  2 spaces ;
: rechts   bright ." ein Glas nach RECHTS"   normal  2 spaces ;
: schieben  ;
: schimpfen   invers ." Betrug! " normal ;

: Anfrage      cr ." Sollen Sie nehmen, trinken oder schieben? "
               cr ." Bitte Ihre Augenzahl und <cr> : "   ;

: Glückwunsch  cr ." Viel Glück beim nächsten Wurf ... " ;

cr .( Sprüche geladen )


\ *** Block No. 4, Hexblock 4

\ Zugriffs-Prozeduren für Tabellen von Prozeduren    jrg 05feb89

: bewegen  ( adr n -- cfa )
    2* +  perform ;

: richtig  ( n  --  0<= n <= 3 )
    swap
    1 max  6 min                        \ ein bißchen Sicherheit
    3 case? IF  2  1- exit  ENDIF
    5 case? IF  4  1- exit  ENDIF
    1-  ;                               \ ein bißchen Justage



\ Dieses Wort läßt zwar Werte < 1 und > 6 zu, justiert sie aber
\ auf den Bereich zwischen 1 und 6 .

\ *** Block No. 5, Hexblock 5

\  Die möglichen Tabellen mit  ] [  oder Create:     jrg 05feb89
\  traditionell:

Create  Glas
        ]  nehmen  links schieben
                   rechts schieben  trinken  [


\  oder VOLKS4TH-gemäß :

Create: Glas
         nehmen
         links schieben
         rechts schieben
         trinken ;


\ *** Block No. 6, Hexblock 6

\  Create: ; :Does>      Auswertung.8                jrg 05feb89

Create: Auswertung.8
          nehmen
           links schieben
           rechts schieben
          trinken ;
      :Does>
         richtig bewegen ;








\ *** Block No. 7, Hexblock 7

\  Das vollständige Programm                         jrg 05feb89

: CRAPS1
     cr  Anfrage cr
           input#
           Glas richtig bewegen
     cr  Glückwunsch
;

\ ausschließlich als Datenstruktur
: CRAPS2
     cr  Anfrage cr
           input#
           Auswertung
     cr  Glückwunsch
;

\ *** Block No. 8, Hexblock 8

\ #### positional CASE def.words  Case: Associative: jrg 05feb89
: Case:    ( -- )
   Create: Does>    ( pfa -- )  swap 2* +  perform ;

\ alternative Definition für CASE:
\ : Case:
\     : Does>       ( pfa -- )  swap 2* +  perform ;


: Associative:           ( n -- )
   Constant   Does>      ( n - index )
   dup @ -rot            \ out of range = maxIndex + 1
   dup @ 0
   DO  2+  2dup  @ =
       IF  2drop drop  I  0 0  LEAVE  THEN
   LOOP 2drop ;

\ *** Block No. 9, Hexblock 9

\  CASE: in der Anwendung       ( 9. Auswertung)     jrg 05feb89
         Case:  handeln                   \ besteht aus :
                   nehmen
                    links  links
                    rechts rechts
                   trinken
                schimpfen ;

6 Associative:  auswerten

                  1 ,
                2 , 3 ,
                4 , 5 ,
                  6 ,

\ Hier erzeugen MIN und MAX  out of range Fehler  maxIndex + 1

\ *** Block No. 10, Hexblock a

\  CASE:  und  Associative:                          jrg 05feb89

: CRAPS3   ( -- )

     cr  Anfrage cr
         input#
            auswerten
            handeln
     cr  Glückwunsch
;







\ *** Block No. 11, Hexblock b


















\ *** Block No. 12, Hexblock c


















\ *** Block No. 13, Hexblock d


















\ *** Block No. 14, Hexblock e

















