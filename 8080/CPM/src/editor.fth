
\ *** Block No. 0, Hexblock 0

\ Full-Screen Editor                                  UH 02Nov86

Dieses File enthaelt den Full-Screen Editor fuer die CP/M -
volksFORTH-Version.

Er enthaelt Line- und Chararcter-Stacks, Find&Replace-Funktion
sowie Unterstuetzung des Shadow-Screen-Konzepts, der view-
Funktion und des sichtbaren Laden von Screens (showload).

Durch die integrierte Tastaturtabelle (keytable) laesst sich die
Kommandobelegung der Tasten auf einfache Art und Weise aendern.

Anregungen, Kritik und Verbesserungsvorschlaege bitte an:
                       U. Hoffmann
                       Harmsstrasse 71
                       2300 Kiel

\ *** Block No. 1, Hexblock 1

\ Load Screen for the Editor             UH 03Nov86   UH 27Nov87

Onlyforth    cr

 1 $1E  +thru

Onlyforth










\ *** Block No. 2, Hexblock 2

\ String primitves                                       27Nov87

: delete   ( buffer size count -- )
   over umin dup >r -  2dup  over r@ +  -rot cmove
   +  r> bl fill ;

: insert   ( string length buffer size -- )
   rot over umin dup >r -
   over dup r@ +  rot cmove>   r> cmove  ;

: replace   ( string length buffer size -- )  rot umin cmove ;






\ *** Block No. 3, Hexblock 3

\ usefull definitions and Editor vocabulary           UH 27Nov87

: blank ( addr len -- )   bl fill ;

: ?enough ( n --)  depth 1- > abort" Not enough Parameters" ;

: ?abort( ( f -- )
    IF [compile] .(  true abort" !" THEN  [compile] ( ;

Vocabulary Editor

' Forth  | Alias F: immediate
' Editor | Alias E: immediate

Editor also definitions


\ *** Block No. 4, Hexblock 4

\ move cursor with position-checking                     23Nov86

| : c  ( n --)   \ checks the cursor position
      r# @ +  dup 0 b/blk uwithin not
       Abort" There is a border!"  r# ! ;

\\

: c  ( n --)   \ goes thru the screens
   r# @ +  dup b/blk 1- >  IF  1 scr +!  THEN
   dup 0< IF  -1 scr +!  THEN  b/blk mod  r# ! ;

: c  ( n --)   \ moves cyclic thru the screen
   r# @ +  b/blk mod  r# ! ;



\ *** Block No. 5, Hexblock 5

\ calculate addresses                                 UH 31Oct86

| Code *line ( l -- adr )
    H pop   H dad   H dad   H dad
    H dad   H dad   H dad   Hpush jmp   end-code

| Code /line ( n -- c l )
   H pop   L A mov   $3F ani  A E mov  0 D mvi
   L A mov   ral   A L mov   H A mov   ral   A H mov
   L A mov   ral   A L mov   H A mov   ral   A H mov
   L A mov   ral   3 ani     H L mov   A H mov
   dpush jmp   end-code

\\
| : *line      ( l -- adr )  c/l * ;
| : /line      ( n -- c l )  c/l /mod ;

\ *** Block No. 6, Hexblock 6

\ calculate addresses                                 UH 01Nov86

| : top        ( --     )  r# off ;
| : cursor     ( -- n   )  r# @ ;
| : 'start     ( -- adr )  scr @ block ;
| : 'end       ( -- adr )  'start  b/blk + ;
| : 'cursor    ( -- adr )  'start  cursor  + ;
| : position   ( -- c l )  cursor  /line ;
| : line#      ( -- l   )  position nip ;
| : col#       ( -- c   )  position drop ;
| : 'line      ( -- adr )  'start  line#  *line + ;
| : 'line-end  ( -- adr )  'line c/l + 1- ;
| : #after     ( -- n   )  c/l col# - ;
| : #remaining ( -- n   )  b/blk cursor - ;
| : #end       ( -- n   )  b/blk line# *line - ;


\ *** Block No. 7, Hexblock 7

\ move cursor directed                                UH 01Nov86

| : curup      c/l negate c ;
| : curdown    c/l c ;
| : curleft    -1 c ;
| : curright   1 c ;

| : +tab  \ 1/4 line forth
      cursor  $10 / 1+ $10 * cursor - c ;

| : -tab  \ 1/8 line back
      cursor  8 mod  negate  dup 0=  8 * +  c ;

| : >""end   'start b/blk -trailing nip  b/blk 1- min  r# ! ;
| : <cr>   #after c ;


\ *** Block No. 8, Hexblock 8

\ show border                                         UH 27Nov87
&15 | Constant dx       1 | Constant dy

| : horizontal ( row -- row' )
     dup dx 1- at   c/l 2+  0 DO Ascii - emit LOOP 1+ ;

| : vertical ( row -- row' )
     l/s 0 DO dup dx 1-    at Ascii | emit
              row dx c/l + at Ascii | emit    1+   LOOP ;

| : border  dy 1- horizontal vertical horizontal drop ;

| : edit-at ( -- )   position swap dy dx d+ at ;

Forth definitions
: updated?  ( -- f)   scr @ block  2- @  0< ;

\ *** Block No. 9, Hexblock 9

\ display screen                         UH 02Nov86   UH 27Nouho
Editor definitions  | Variable isfile'        | Variable imode

| : .updated ( -- )  7 0 at
      updated? IF 4 spaces ELSE ." not " THEN ." updated" ;

| : redisplay   ( line# -- )
     dup dy +  dx at   *line 'start +  c/l type ;

| : .file ( 'file -- ) [ Dos ] .file  &14 col - 0 max spaces ;
| : .title   1 0 at isfile@ .file    3 0 at isfile' @ .file
      5 0 at ." Scr# " scr @ 4 .r  .updated  &10 0 at
      imode @ IF ." insert   " exit THEN ." overwrite" ;

| : .screen  l/s 0 DO I redisplay LOOP ;
| : .all   .title .screen ;

\ *** Block No. 10, Hexblock a

\ check errors                                        UH 02Nov86

| : ?bottom  ( -- )  'end  c/l -  c/l -trailing nip
                      Abort" You would lose a line" ;

| : ?fit ( n -- )  'line c/l -trailing  nip + c/l >
     IF line# redisplay
        true Abort" You would lose a char" THEN ;

| : ?end    1 ?fit ;







\ *** Block No. 11, Hexblock b

\ programmer's id                                     UH 02Nov86

$12 | Constant id-len
Create id   id-len allot   id id-len erase

| : stamp  ( -- )
     id 1+ count  'start c/l + over - swap  cmove ;

| : ?stamp   ( -- )   updated? IF stamp THEN  ;

| : get-id   ( -- )
   id c@ ?exit  id on
   cr ." Enter your ID : "  at?  $10 0 DO Ascii . emit LOOP  at
   id id-len 2 /string expect  rvsoff  span @ id 1+ c! ;



\ *** Block No. 12, Hexblock c

\ update screen-display                               UH 02Dec86

| : emptybuf    prev @  2+ dup on  4+ off ;

| : undo  emptybuf  .all ;

| : modified   updated? ?exit  update .updated ;

| : linemodified   modified  line# redisplay ;

| : screenmodified    modified
     l/s line# ?DO I redisplay LOOP ;

| : .modified ( -- )  dy l/s + 4+ 0 at  scr @ .
      updated? not IF  ." un"  THEN  ." modified"  ?stamp ;


\ *** Block No. 13, Hexblock d

\ leave editor                           UH 02Dec86   UH 23Feb88
| Variable (pad   (pad off
| : memtop  ( -- adr)   sp@ $100 - ;

| Create char  1 allot

( | Variable imode ) imode off
| : setimode   imode on  .title ;
| : clrimode   imode off .title ;
| : flipimode ( -- )  imode @ 0= imode !  .title ;

| : done ( -- )
     ['] (quit is 'quit  ['] (error errorhandler !  quit ;

| : update-exit  ( -- )  .modified  done ;
| : flushed-exit ( -- )  .modified  save-buffers  done ;

\ *** Block No. 14, Hexblock e

\ handle lines                                        UH 01Nov86

| : (clear-line   'line c/l blank ;
| : clear-line   (clear-line linemodified ;

| : clear>   'cursor #after blank  linemodified ;

| : delete-line   'line #end c/l  delete screenmodified ;

| : backline   curup delete-line ;

| : (insert-line
      ?bottom  'line c/l over #end insert (clear-line ;

| : insert-line   (insert-line  screenmodified ;


\ *** Block No. 15, Hexblock f

\ handle characters                                   UH 01Nov86

| : delete-char   'cursor #after 1 delete  linemodified ;

| : backspace   curleft  delete-char ;

| : (insert-char   ?end  'cursor 1 over #after insert ;


| : insert-char   (insert-char  bl 'cursor c! linemodified ;

| : putchar  ( --)  char c@
    imode @ IF (insert-char THEN
    'cursor c!  linemodified  curright ;



\ *** Block No. 16, Hexblock 10

\ stack lines                                         UH 31Oct86

| Create lines 4 allot    \  { 2+pointer | 2base }
| : 'lines  ( -- adr)   lines 2@ + ;

| : @line   'lines  memtop u> Abort" line buffer full"
   'line 'lines c/l cmove  c/l lines +! ;

| : copyline   @line curdown ;
| : line>buf   @line delete-line ;

| : !line   c/l negate lines +!  'lines 'line c/l cmove ;

| : buf>line   lines @ 0= Abort" line buffer empty"
     ?bottom    (insert-line  !line   screenmodified ;


\ *** Block No. 17, Hexblock 11

\ stack characters                                    UH 01Nov86

| Create chars 4 allot    \  { 2+pointer | 2base }
| : 'chars  ( -- adr)   chars 2@ + ;

| : @char   'chars 1-  lines 2+ @  u> Abort" char buffer full"
     'cursor c@ 'chars c!  1 chars +! ;

| : copychar   @char curright ;
| : char>buf   @char delete-char ;

| : !char   -1 chars +!  'chars c@  'cursor c! ;

| : buf>char   chars @ 0= Abort" char buffer empty"
     ?end  (insert-char  !char  linemodified ;


\ *** Block No. 18, Hexblock 12

\ switch screens                         UH 03Nov86   UH 27Nov87

| Variable r#'             r#'  off
| Variable scr'            scr' off
( | Variable isfile' )     isfile@  isfile' !

| : associate \ switch to alternate screen
     isfile' @ isfile@   isfile' ! isfile !
     scr' @ scr @ scr' ! scr !   r#' @ r# @ r#' ! r# ! ;

| : mark   isfile@ isfile' !  scr @ scr' !  r# @ r#' ! .title ;
| : n   ?stamp  1 scr +! .all ;
| : b   ?stamp -1 scr +! .all ;
| : a   ?stamp associate .all ;



\ *** Block No. 19, Hexblock 13

\ shadow screens                                      UH 03Nov86

Variable shadow   shadow off

| : (shadow   isfile@ IF capacity 2/ exit THEN  shadow @ ;

| : >shadow  ?stamp   \ switch to shadow screen
      (shadow dup  scr @  u> not IF negate THEN scr +! .all ;









\ *** Block No. 20, Hexblock 14

\ load and show screens                               UH 06Mar88

' name >body &10 + | Constant 'name

| : showoff  ['] exit 'name ! curoff rvsoff  ;

| : show ( -- )  blk @ 0= IF showoff exit THEN
     >in @ 1- r# !  curoff edit-at curon
     stop? IF showoff true Abort" Break! " THEN
     blk @  scr @ -
     IF  blk @  scr ! rvsoff curoff .all rvson curon THEN ;

| : showload  ( -- )    ?stamp save-buffers
     ['] show 'name !  curon rvson
     ['] .status >body push  ['] noop is .status
     scr @  scr push  scr off  r# push  r# @  (load  showoff ;

\ *** Block No. 21, Hexblock 15

\ find strings                                        UH 01Nov86

| Variable insert-buffer
| Variable find-buffer
| : 'insert ( -- addr )  insert-buffer @ ;
| : 'find   ( -- addr )  find-buffer @ ;

| : .buf ( addr -- )   count type ." |"  &80 col - spaces  ;

| : get ( addr -- )  >r at?  r@ .buf
    2dup at  r@ 1+ c/l expect  span @ ?dup IF r@ c! THEN
    at r> .buf ;

| : get-buffers    dy l/s + 2+  dx 1-  2dup  at
       ." find:    |"  'find   get     swap 1+ swap 2- at
     ." ? replace: |"  'insert get ;

\ *** Block No. 22, Hexblock 16

\ search for string                      UH 02Nov86   UH 27Nov87

| : skip ( addr -- addr' )  'find c@ + ;

| : find?  ( -- addr T | F )
     'find count 'cursor #remaining "search ;

| : "find ( -- r# scr )
    find?  IF skip 'start -  scr @ exit THEN  ?stamp
    capacity   scr @ 1+
    ?DO 'find count
         I   dup 5 5 at 4 .r   block  b/blk "search
       IF skip I block -  I endloop exit THEN
       stop? Abort" Break! "
    LOOP  true Abort" not found!" ;


\ *** Block No. 23, Hexblock 17

\ replace strings                        UH 03Nov86   UH 27Nov87
| : replace? ( -- f )  dy l/s + 3+ dx 3 - at
      key dup #cr = IF line# redisplay true Abort" Break!" THEN
      capital Ascii R = ;

| : "mark ( -- )   r# push
     'find count dup negate c  edit-at  rvson type rvsoff ;

| : (replace  'insert c@  'find c@ - ?fit
      'find c@ negate c   'cursor #after  'find c@ delete
      'insert count   'cursor #after  insert
      'insert c@ c   modified ;

| : "replace   get-buffers
      BEGIN  "find  dup scr @ - swap scr ! IF .all THEN  r# !
        "mark replace? IF (replace THEN line# redisplay REPEAT ;

\ *** Block No. 24, Hexblock 18

\ Control-Characters  'normal' CP/M                uho 08May2005

Forth definitions

: Ctrl ( -- c )
   name 1+ c@  $1F and   state @ IF [compile] Literal THEN ;
immediate

$7F Constant #del

Editor definitions

\ | : flipimode    imode @ 0= imode ! ;




\ *** Block No. 25, Hexblock 19

\ Try a Screen-Editor 'normal' CP/M                   UH 29Nov86

Create keytable
Ctrl E c,       Ctrl S c,       Ctrl X c,       Ctrl D c,
Ctrl I c,       Ctrl J c,       Ctrl O c,       Ctrl K c,
                                Ctrl P c,       Ctrl L c,
Ctrl H c,       Ctrl H c,         #del c,       Ctrl G c,
Ctrl T c,                       Ctrl Y c,       Ctrl N c,
Ctrl V c,                                       Ctrl Z c,
   #cr c,       Ctrl F c,       Ctrl A c,
                                Ctrl \ c,       Ctrl U c,
Ctrl Q c,         #esc c,                       Ctrl W c,
Ctrl C c,       Ctrl R c,       Ctrl ] c,       Ctrl B c,


here keytable -  Constant #keys

\ *** Block No. 26, Hexblock 1a

\ Try a screen Editor                                 UH 29Nov86

Create: actiontable
curup           curleft         curdown         curright
line>buf        char>buf        buf>line        buf>char
                                copyline        copychar
backspace       backspace       backspace       delete-char
insert-char                     delete-line     insert-line
flipimode                      ( clear-line )   clear>
<cr>            +tab            -tab
( top             >""end  )     "replace        undo
update-exit     flushed-exit  ( showload )      >shadow
n               b               a               mark ;


here actiontable -  2/ 1-   #keys -  ?abort( # of actions)

\ *** Block No. 27, Hexblock 1b

\ find keys                                           UH 01Nov86

| Code findkey ( key -- addr/default )
     H pop   L A mov   keytable H lxi   #keys $100 * D lxi
     [[ M cmp 0=
        ?[ actiontable H lxi  0 D mvi   D dad   D dad
           M E mov   H inx   M D mov   D push   next ]?
        H inx   E inr   D dcr   0= ?]
     ' putchar H lxi   hpush jmp
  end-code

\\
| : findkey  ( key -- adr/default )
  #keys 0 DO  dup  keytable F: I + c@ =
     IF drop   E: actiontable  F: I 2* + @ endloop exit THEN
  LOOP drop ['] putchar ;

\ *** Block No. 28, Hexblock 1c

\ allocate buffers                                    UH 01Nov86

c/l 2* | Constant cstack-size

| : nextbuf ( adr -- adr' )   cstack-size + ;

| : ?clearbuffer   pad (pad @ = ?exit
      pad     dup (pad !
      nextbuf dup find-buffer   !   'find off
      nextbuf dup insert-buffer !   'insert off
      nextbuf dup 0 chars 2!
      nextbuf     0 lines 2! ;





\ *** Block No. 29, Hexblock 1d

\ enter and exit the editor, editor's loop            UH 02Nov86
| Variable jingle  jingle on    | : bell  07 con! jingle off ;

| : clear-error
     jingle @ ?exit  dy l/s + 1+ dx  at c/l spaces jingle on ;

| : fullquit    BEGIN  ?clearbuffer edit-at  key dup char c!
      findkey execute  clear-error REPEAT ;

| : fullerror  ( string --) jingle @ IF bell THEN
      dy l/s + 1+ dx $16 + at  rvson count type rvsoff
      &80 col - spaces  scr @ capacity 1- min 0 max scr !
      .title quit ;

| : install   ( -- )
     ['] fullquit Is 'quit  ['] fullerror errorhandler !  ;

\ *** Block No. 30, Hexblock 1e

\ enter and exit the Editor                           UH 02Nov86

Forth definitions

: v   ( -- )   E: 'start drop  get-id  install  ?clearbuffer
               page curoff  border .all  quit ;

: l   ( scr -- )   1 ?enough scr !  E: top  F: v ;









\ *** Block No. 31, Hexblock 1f

\ savesystem                                       uho 09May2uho

: savesystem    \ save image
    E: id off  (pad off   savesystem ;

| : >find  ?clearbuffer  >in push
      bl word count  'find 1+ place
      bl   'find 1+   dup >r   count   dup >r   + c!
      r> 2+ 'find c!   bl r> c! ;
| : %view  ( -- )  >find ' >name 4- @  (view
   ?dup 0= Abort" hand made"  scr !
   E: top curdown find? 0=
   IF ." From Scr # " scr @ u. true Abort" wrong file" THEN
   skip 'start - 1- r# ! ;
: view ( -- ) %view scr @ list ;
: fix ( -- ) %view v ;
