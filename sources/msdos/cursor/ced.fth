
\ *** Block No. 0, Hexblock 0

\ Commandline EDitor für volksFORTH rev. 3.80         UH 05feb89
Dieses File enthaelt Definitionen, die es ermöglichen die
Kommandozeile zu editieren.
Es gibt eine Commandline History, die es ermöglicht alte
Eingaben wiederzuholen. Diese werden zyklisch auf Screen 0
im File History gesichert und bleiben so auch über ein
SAVESYSTEM erhalten.

Tasten:
  Cursor links/rechts                       
  Zeichen löschen                         <del> und <-
  Zeile löschen                           <esc>
  Einfügen an aus                         <ins>
  Zeile abschließen                       <enter>
  Anfang/Ende der Zeile                   <pos1> <end>
  alte Zeilen wiederholen                  

\ *** Block No. 1, Hexblock 1

\ Commandline EDitor LOAD-Screen        UH 20Nov87   jrg 14mär89


: curleft ( -- )  at? 1- at ;
: currite ( -- )  at? 1+ at ;

1 5 +thru  \ Erweiterte Eingabe

.( Kommandozeilen Editor geladen ) cr








\ *** Block No. 2, Hexblock 2

\ History -- Kommandogeschichte                      jrg 14mär89
makefile history  1 more

| Variable line#       line# off
| Variable lastline#   lastline# off

| : 'history ( n -- addr )  pushfile  history
    c/l *   b/blk /mod  block + ;

| : @line ( n -- addr len )  'history c/l -trailing ;
| : !history ( addr line# -- )
      'history dup c/l blank  span @ c/l min cmove update ;
| : @history ( addr line# -- )
      @line rot swap dup span ! cmove ;

| : +line ( n addr -- )  dup @  rot + l/s  mod  swap ! ;

\ *** Block No. 3, Hexblock 3

\ Ende der Eingabe                                   jrg 18dez89

| Variable maxchars               | Variable cinsert cinsert on

| : -text ( a1 a2 l -- 0=equal )  bounds
      ?DO count I c@ - ?dup IF nip ENDLOOP exit THEN LOOP 0= ;

| : done ( a p1 -- a p2 )  2dup
      at?  rot -  span @ dup maxchars !  + at space blankline
      line# @ @line  span @ = IF span @ -text 0=exit 2dup THEN
      drop  lastline# @ !history  1 lastline# +line ;






\ *** Block No. 4, Hexblock 4

\ Erweiterte Eingabe                    UH 08OCt87   jrg 19dez89
  : redisplay ( addr pos -- )
     at?  2swap  span @ swap /string type blankline at ;

| : del   ( addr pos -- )  span @ 0=exit  dup >r   + dup 1+ swap
      span @ r> - cmove   -1 span +! ;
| : ins   ( addr pos1 -- )  dup >r   + dup  dup 1+
      span @ r> - cmove>   bl swap c!  1 span +! ;

| : delete  ( a p1 -- a p2 )  2dup del  2dup redisplay ;
| : back ( a p1 -- a p2 )  1- curleft delete ;

| : recall ( a p1 -- a p2 ) at? rot - at  dup line# @ @history
     dup 0 redisplay  at? span @ + at  span @ ;

| : <start ( a1 p1 -- a2  p2 )  at?  rot - at  0 ;

\ *** Block No. 5, Hexblock 5

\ Tastenbelegung für Zeilen-Editor MS/DOS         ks jrg 19dez89

: (decode ( addr pos1 key -- addr pos2 )
  -&77  case? IF dup  span @ < 0=exit  currite 1+   exit THEN
  -&75  case? IF dup           0=exit  curleft 1-   exit THEN
  -&82  case? IF cinsert @     0=      cinsert !    exit THEN
   #bs  case? IF dup           0=exit  back         exit THEN
  -&83  case? IF span @ 2dup < and 0=exit delete    exit THEN
  -&72  case? IF -1 line# +line recall              exit THEN
  -&80  case? IF  1 line# +line recall              exit THEN
   #cr  case? IF done                               exit THEN
   #esc case? IF <start span off  2dup redisplay    exit THEN
  -&71  case? IF <start                             exit THEN
  -&79  case? IF at? rot -  span @ +  at  span @    exit THEN
   dup emit >r  cinsert @ IF 2dup ins THEN   2dup +
   r>  swap c!  1+  dup span @ max span ! 2dup redisplay ;

\ *** Block No. 6, Hexblock 6

\ Patch                                  UH 08Oct87  jrg 19dez89

: showcur  ( -- )
     curshape? swap drop    \ bot top
     cinsert @ IF    dup 2/  \ top = bot/2
               ELSE  dup 1-
               THEN swap curshape ;

: (expect ( addr len -- )  maxchars !   span off
    lastline# @ line# !  0
    BEGIN span @ maxchars @ u<
    WHILE   key decode showcur REPEAT 2drop ;

' (decode ' keyboard 6 + !
' (expect ' keyboard 8 + !


\ *** Block No. 7, Hexblock 7


















\ *** Block No. 8, Hexblock 8

















