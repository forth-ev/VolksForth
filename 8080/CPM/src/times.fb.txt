
\ *** Block No. 0, Hexblock 0

\\ Times Often: interactive loops                        11Nov86

Dieses File enthaelt die Definitionen der beiden Utility-Worte
TIMES, OFTEN, die interaktiv benutzt werden koennen, was
normalerweise mit BEGIN WHILE ... nicht moeglich ist.

Benutzung:  nur interaktiv!

a b ... nn times \ Wiederhole die Befehlsfolge "a b ..." nn mal,
                 \ oder bis eine Taste gedrueckt wird, oder
                 \ bis ein Fehler auftritt,

a b ... often     \ Wiederhole die Befehlsfolge "a b ..."
                  \ so oft, bis eine Taste gedrueckt wird, oder
                  \ bis ein Fehler auftritt.


\ *** Block No. 1, Hexblock 1

\ Times, Often                                           02feb86

also Forth definitions

: often    stop? ?exit  >in off ;

| Variable #times   #times off

: times   ( n --)
 ?dup IF  #times @ 2+  u<  stop? or
          IF  #times off  exit  THEN  1 #times +!
 ELSE  stop? ?exit  THEN  >in off ;

toss definitions


