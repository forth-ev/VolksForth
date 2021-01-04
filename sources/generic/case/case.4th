
\ *** Block No. 0, Hexblock 0


















\ *** Block No. 1, Hexblock 1


















\ *** Block No. 2, Hexblock 2

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

\ leapyear?    nach Wil Baden VD 2/87 S.42           jrg 30mai89

| : leapyear?  ( year# -- f : true falls Jahr = Schaltjahr )
       CASE 400 mod 0= OF  true   BREAK
       CASE 100 mod 0= OF  false  BREAK
       CASE   4 mod 0= OF  true   BREAK
       drop   false ;

\\ nach Kaiser, Grundlegende Elemente ... S.160, Birkhäuser

  : leapyear?  ( year# -- f : true falls Jahr = Schaltjahr )
       dup         4 mod 0=        ( y# f)
       swap dup  100 mod 0<>       ( f1 y# f2 )
       rot       and               ( y# f3 )
       swap      400 mod 0= or  ;


\ *** Block No. 5, Hexblock 5

\ Monatsnamen                                        jrg 30mai89
|  1 Constant jan
|  2 Constant feb
|  3 Constant mär
|  4 Constant apr
|  5 Constant mai
|  6 Constant jun
|  7 Constant jul
|  8 Constant aug
|  9 Constant sep
| 10 Constant okt
| 11 Constant nov
| 12 Constant dez

\\
| Create months  ," janfebmäraprmaijunjulaugsepoktnovdez"

\ *** Block No. 6, Hexblock 6

\ Tage im Monat                                      jrg 30mai89

: #days  ( month# -- days-in-month )
    CASE  jan =  apr =or  jun =or  nov =or  OF  30  BREAK
    CASE  feb = not                         OF  31  BREAK
    drop  leapyear? IF  29  ELSE  28  THEN ;

: .all
    12 1+  1
             DO cr
              I .
              I >months type  ."  hat "
              I #days   .     ." Tage."
             LOOP ;



\ *** Block No. 7, Hexblock 7

% Schaltjahr ?                                       jrg 30mai89

Bei der Entscheidung, ob eine Jahreszahl ein Schaltjahr be-
zeichnet, werden zunächst die ohne Rest durch 4 teilbaren Jahre
durch
        JAHR MOD 4 = 0
erkannt. Die ohne Rest durch 100 teilbaren Jahre werden durch

        (JAHR MOD 4 = 0) AND (JAHR MOD 100 <> 0)

"entfernt".
Dazu werden die ohne Rest durch 400 teilbaren Jahreszahlen hin-
zugefügt:

((JAHR MOD 4 = 0) AND (JAHR MOD 100 <> 0)) OR (JAHR MOD 400 = 0)


\ *** Block No. 8, Hexblock 8


















\ *** Block No. 9, Hexblock 9

% Schaltjahr ?                                       jrg 30mai89

Bei der Entscheidung, ob eine Jahreszahl ein Schaltjahr be-
zeichnet, werden zunächst die ohne Rest durch 4 teilbaren Jahre
durch
        JAHR MOD 4 = 0
erkannt. Die ohne Rest durch 100 teilbaren Jahre werden durch

        (JAHR MOD 4 = 0) AND (JAHR MOD 100 <> 0)

"entfernt".
Dazu werden die ohne Rest durch 400 teilbaren Jahreszahlen hin-
zugefügt:

((JAHR MOD 4 = 0) AND (JAHR MOD 100 <> 0)) OR (JAHR MOD 400 = 0)


\ *** Block No. 10, Hexblock a


















\ *** Block No. 11, Hexblock b

















