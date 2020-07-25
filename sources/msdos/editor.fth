\ *** Block No. 0 Hexblock 0 
     volksFORTH Full-Screen-Editor HELP Screen       cas 11nov05
                                                                
Quit Editor         : flushed: ESC    updated: ^E               
discard changes     : ^U  (UNDO)                                
move cursor         : Cursorkeys     (delete with DEL or <- )   
insert              : INS (toggle),  ^ENTER (insert Screen)     
Tabs                : TAB (to right), SHIFT TAB (to left)       
paging              : Pg Dn (next screen),  Pg Up (previous scr)
                    : F9    (alternate), SHIFT F9 (shadow scr)  
mark alternate Scr. : F10                                       
delete/insert line  : ^Y (delete), ^N (insert)                  
split line          : ^PgDn (split), ^PgUp (join)               
search and replace  : F2 (stop with ESC, replace with 'R' )     
linebuffer          : F3 (push&delete), F5 (push), F7 (pop)     
charbuffer          : F4 (push&delete), F6 (push), F8 (pop)     
misc                : ^F (Fix), ^L (Showload), ^S (Screen #)    
\ *** Block No. 1 Hexblock 1 
-->    \ Full-Screen Editor                          cas 10nov05
This is the Full-Screen Editor for MS-DOS volksFORTH            
                                                                
Features: Line- and Char-Buffer, Find- and Replace, Support for 
"Shadow-Screens", View Function and loading of screens with     
visual feedback (showload)                                      
                                                                
The Keybinding can be easily changed by using the integrated    
Keytable.                                                       
                                                                
                                                                
Ported to the MS-DOS volksFORTH by K.Schleisiek on 22 dez 87    
Original design by Ullrich Hoffmann                             
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ Load Screen for the Editor                         cas 10nov05
                                                                
  Onlyforth     \needs Assembler   2 loadfrom asm.scr           
                                                                
      3 load    \ PC adaption                                   
  4   9 thru    \ Editor                                        
                                                                
\   &10 load    \ ANSI display interface                        
\   &11 load    \ BIOS display interface                        
    &12 load    \ MULTItasking display interface                
                                                                
&13 &39 thru    \ Editor                                        
                                                                
Onlyforth  .( Screen Editor loaded ) cr                         
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\ BIM adaption                                        UH 11dez88
                                                                
| : ?range ( n -- n ) isfile@ 0=exit dup 0< 9 and ?diskerror    
      dup capacity - 1+  0 max  ?dup 0=exit  more  ;            
| : block    ( n -- adr )  ?range  block ;                      
                                                                
  $1B Constant #esc                                             
                                                                
  : curon    &11 &12 curshape ;                                 
                                                                
  : curoff   &14 dup curshape ;                                 
                                                                
  Variable caps   caps off                                      
                                                                
  Label ?capital   1 # caps #) byte test                        
     0= ?[  (capital # jmp  ]?   ret   end-code                 
\ *** Block No. 4 Hexblock 4 
\ search delete insert replace                    ks 20 dez 87  
                                                                
| : delete   ( buffer size count -- )                           
     over min >r  r@ - ( left over )  dup 0>                    
     IF  2dup swap dup  r@ + -rot swap cmove THEN               
     + r> bl fill ;                                             
                                                                
| : insert   ( string length buffer size -- )                   
     rot over min >r  r@ - ( left over )                        
     over dup r@ +  rot cmove>   r> cmove  ;                    
                                                                
| : replace   ( string length buffer size -- )                  
     rot min cmove ;                                            
                                                                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
\ usefull definitions and Editor vocabulary           UH 11mai88
                                                                
Vocabulary Editor                                               
                                                                
' Forth  | Alias [F] immediate                                  
' Editor | Alias [E] immediate                                  
                                                                
Editor also definitions                                         
                                                                
| : c  ( n --)   \ moves cyclic thru the screen                 
     r# @ +  b/blk mod  r# ! ;                                  
                                                                
| Variable r#'      r#'     off                                 
| Variable scr'     scr'    off                                 
' fromfile | Alias isfile'                                      
| Variable lastfile  | Variable lastscr   | Variable lastr#     
\ *** Block No. 6 Hexblock 6 
\\ move cursor with position-checking             ks 18 dez 87  
\ different versions of cursor positioning error reporting      
                                                                
| : c  ( n --)   \ checks the cursor position                   
      r# @ +  dup 0 b/blk uwithin not                           
       Abort" There is a border!"  r# ! ;                       
                                                                
| : c  ( n --)   \ goes thru the screens                        
   r# @ +  dup b/blk 1- >  IF  1 scr +!  THEN                   
   dup 0< IF  -1 scr +!  THEN  b/blk mod  r# ! ;                
                                                                
| : c  ( n --)   \ moves cyclic thru the screen                 
   r# @ +  b/blk mod  r# ! ;                                    
                                                                
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
\ calculate addresses                             ks 20 dez 87  
| : *line      ( l -- adr )  c/l * ;                            
| : /line      ( n -- c l )  c/l /mod ;                         
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
\ *** Block No. 8 Hexblock 8 
\ move cursor directed                                UH 11dez88
| Create >at 0 , 0 ,                                            
| : curup      c/l negate c ;                                   
| : curdown    c/l c ;                                          
| : curleft    -1 c ;                                           
| : curright   1 c ;                                            
                                                                
| : +tab  ( 1/4 -> ) cursor  $10 / 1+ $10 * cursor - c ;        
| : -tab ( 1/8 <- ) cursor  8 mod  negate  dup 0=  8 * +  c ;   
                                                                
| : >last  ( adr len -- ) -trailing nip  b/blk min r# ! ;       
| : <cr>   #after c ;                                           
| : <line ( -- )   col# negate c  'line c/l -trailing nip 0=exit
      BEGIN 'cursor c@ bl = WHILE curright REPEAT ;             
| : line> ( -- )   'start  line# 1+ *line 1- >last ;            
| : >""end ( -- )  'start  b/blk  >last ;                       
\ *** Block No. 9 Hexblock 9 
\ show border                                         UH 29Sep87
                                                                
&14 | Constant dx       1 | Constant dy                         
                                                                
| : horizontal ( row eck1 eck2 -- row' )                        
     rot dup >r dx 1- at  swap emit                             
     c/l 0 DO Ascii - emit LOOP emit  r> 1+ ;                   
                                                                
| : vertical ( row -- row' )                                    
     l/s 0 DO dup dx 1-    at Ascii | emit                      
              row dx c/l + at Ascii | emit    1+   LOOP ;       
                                                                
| : border  dy 1- Ascii / Ascii \ horizontal                    
     vertical     Ascii \ Ascii / horizontal drop ;             
                                                                
| : edit-at ( -- )   position swap dy dx d+ at ;                
\ *** Block No. 10 Hexblock A 
\  ANSI  display interface                        ks 03 feb 88  
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
| : redisplay ( line# -- )                                      
     dup dy +  dx at   *line 'start +  c/l type ;               
                                                                
| : (done ( -- )  ; immediate                                   
                                                                
                                                                
| : install-screen ( -- )  l/s 6 + 0 >at 2! page ;              
                                                                
\ *** Block No. 11 Hexblock B 
\  BIOS-display interface                         ks 03 feb 88  
| Code (.line  ( line addr videoseg -- )                        
     A pop   W pop   I push   E: push   D E: mov                
     $0E # W add   W W add   A I xchg   c/l # C mov             
     attribut #) A+ mov   [[  byte lods   stos   C0= ?]         
     E: pop   I pop   D pop   Next   end-code                   
                                                                
                                                                
| : redisplay ( line# -- )                                      
     dup 1+ c/row * swap c/l * 'start + video@ (.line ;         
                                                                
| : (done   ( -- )  ; immediate                                 
                                                                
                                                                
| : install-screen ( -- )  l/s 6 + 0 >at 2! page ;              
                                                                
\ *** Block No. 12 Hexblock C 
\  MULTI-display interface                      ks    UH 10Sep87
| Code (.line  ( line addr videoseg -- )                        
     C pop   W pop   I push   E: push   D E: mov                
     $0E # W add   W W add   u' area U D) I mov                 
     u' catt I D) A+ mov   C I mov                              
     c/l # C mov   [[  byte lods   stos   C0= ?]                
     E: pop   I pop   D pop   Next   end-code                   
                                                                
| : redisplay ( line# -- )                                      
     dup 1+ c/row * swap c/l * 'start + video@ (.line ;         
                                                                
| : (done ( -- )  line# 2+ c/col 2- window  ;                   
                                                                
| : cleartop ( -- )   0 l/s 5 + window (page ;                  
| : install-screen ( -- )    row l/s 6 + u<                     
    IF l/s 6 + 0 full page ELSE at? cleartop THEN >at 2! ;      
\ *** Block No. 13 Hexblock D 
\ display screen                                      UH 11mai88
Forth definitions                                               
: updated?  ( -- f)   'start 2- @  0< ;                         
Editor definitions                                              
| : .updated ( -- )  9 0 at                                     
      updated? IF 4 spaces ELSE ." not " THEN ." updated" ;     
                                                                
| : .screen  l/s 0 DO I redisplay LOOP ;                        
\ | : .file   ( fcb -- )                                        
\      ?dup IF  body> >name .name exit  THEN  ." direct" ;      
| : .title [ DOS ]  1 0 at isfile@  .file dx 1- tab             
      2 0 at drv (.drv scr @ 6 .r                               
      4 0 at fromfile @  .file  dx 1- tab                       
      5 0 at fswap  drv (.drv scr' @ 6 .r  fswap  .updated ;    
                                                                
| : .all   .title .screen ;                                     
\ *** Block No. 14 Hexblock E 
\ check errors                                        UH 02Nov86
                                                                
| : ?bottom  ( -- )  'end  c/l -  c/l -trailing nip             
                      Abort" You would lose a line" ;           
                                                                
| : ?fit ( n -- )  'line c/l -trailing  nip + c/l >             
     IF line# redisplay                                         
        true Abort" You would lose a char" THEN ;               
                                                                
| : ?end    1 ?fit ;                                            
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 15 Hexblock F 
\ programmer's id                                 ks 18 dez 87  
                                                                
$12 | Constant id-len                                           
Create id   id-len allot   id id-len erase                      
                                                                
| : stamp ( -- )  id 1+ count  'start c/l + over - swap  cmove ;
                                                                
| : ?stamp   ( -- )   updated? IF stamp THEN  ;                 
                                                                
| : ## ( n -- )  base push decimal 0 <# # # #> id 1+ attach ;   
                                                                
| : get-id   ( -- )  id c@ ?exit ID on                          
   cr ." Enter your ID : "  at?  3 0 DO Ascii . emit LOOP  at   
   id 2+ 3 expect normal  span @ dup id 1+ c!  0=exit           
   bl id 1+ append  date@ rot ## swap >months id 1+ attach ## ; 
                                                                
\ *** Block No. 16 Hexblock 10 
\ update screen-display                               UH 28Aug87
                                                                
| : emptybuf    prev @  2+ dup on  4+ off ;                     
                                                                
| : undo  emptybuf  .all ;                                      
                                                                
| : modified   updated? ?exit  update .updated ;                
                                                                
| : linemodified   modified  line# redisplay ;                  
                                                                
| : screenmodified    modified                                  
     l/s line# ?DO I redisplay LOOP ;                           
                                                                
| : .modified ( -- )  >at 2@ at  space  scr @ .                 
      updated? not IF  ." un"  THEN  ." modified"  ?stamp ;     
                                                                
\ *** Block No. 17 Hexblock 11 
\ leave editor                                        UH 10Sep87
| Variable (pad   (pad off                                      
| : memtop  ( -- adr)   sp@ $100 - ;                            
                                                                
| Create char  1 allot                                          
| Variable imode  imode off                                     
| : .imode   at?  7 0 at                                        
      imode @ IF ." insert   "  ELSE ." overwrite" THEN at ;    
| : setimode   imode on  .imode ;                               
| : clrimode   imode off .imode ;                               
                                                                
| : done ( -- )  (done                                          
     ['] (quit is 'quit  ['] (error errorhandler !  quit ;      
                                                                
| : update-exit  ( -- )  .modified  done ;                      
| : flushed-exit ( -- )  .modified  save-buffers  done ;        
\ *** Block No. 18 Hexblock 12 
\ handle screens                                      UH 21jan89
                                                                
| : insert-screen  ( scr -- )  \ before scr                     
     1 more   fromfile push  isfile@ fromfile !                 
     capacity 2- over 1+  convey ;                              
                                                                
| : wipe-screen ( -- )  'start b/blk blank ;                    
                                                                
| : new-screen ( -- )                                           
     scr @  insert-screen wipe-screen  top screenmodified ;     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 19 Hexblock 13 
\ handle lines                                        UH 01Nov86
                                                                
| : (clear-line   'line c/l blank ;                             
| : clear-line   (clear-line linemodified ;                     
                                                                
| : clear>   'cursor #after blank  linemodified ;               
                                                                
| : delete-line   'line #end c/l  delete screenmodified ;       
                                                                
| : backline   curup delete-line ;                              
                                                                
| : (insert-line                                                
      ?bottom  'line c/l over #end insert (clear-line ;         
                                                                
| : insert-line   (insert-line  screenmodified ;                
                                                                
\ *** Block No. 20 Hexblock 14 
\ join and split lines                                UH 11dez88
                                                                
| : insert-spaces ( n -- )  'cursor swap                        
     2dup over #remaining insert  blank ;                       
                                                                
| : split ( -- )  ?bottom cursor  col#  <cr> insert-spaces  r# !
     #after insert-spaces  screenmodified ;                     
                                                                
| : delete-characters ( n -- ) 'cursor #remaining rot delete ;  
                                                                
| : join ( -- ) cursor <cr>  line> col#  <line col# under -     
     rot r# ! #after > Abort" next line will not fit!"          
     #after +  dup delete-characters                            
     cursor  <cr> c/l rot - dup 0<                              
     IF negate insert-spaces ELSE delete-characters THEN r# !   
     screenmodified ;                                           
\ *** Block No. 21 Hexblock 15 
\ handle characters                                   UH 01Nov86
                                                                
| : delete-char   'cursor #after 1 delete  linemodified ;       
                                                                
| : backspace   curleft  delete-char ;                          
                                                                
| : (insert-char   ?end  'cursor 1 over #after insert ;         
                                                                
                                                                
| : insert-char   (insert-char  bl 'cursor c! linemodified ;    
                                                                
| : putchar  ( --)  char c@                                     
    imode @ IF (insert-char THEN                                
    'cursor c!  linemodified  curright ;                        
                                                                
                                                                
\ *** Block No. 22 Hexblock 16 
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
                                                                
\ *** Block No. 23 Hexblock 17 
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
                                                                
\ *** Block No. 24 Hexblock 18 
\ switch screens                                      UH 11mai88
                                                                
| : imprint ( -- ) \ remember valid file                        
      isfile@ lastfile !  scr @ lastscr !   r# @ lastr# ! ;     
                                                                
| : remember ( -- )                                             
      lastfile @ isfile !  lastscr @ scr !  lastr# @ r# ! ;     
                                                                
| : associate \ switch to alternate screen                      
     isfile' @ isfile@   isfile' ! isfile !                     
     scr' @ scr @ scr' ! scr !   r#' @ r# @ r#' ! r# ! ;        
                                                                
| : mark   isfile@ isfile' !  scr @ scr' !  r# @ r#' ! .all ;   
| : n   ?stamp   1 scr +! .all ;                                
| : b   ?stamp  -1 scr +! .all ;                                
| : a   ?stamp associate  .all ;                                
\ *** Block No. 25 Hexblock 19 
\ shadow screens                                      UH 03Nov86
                                                                
Variable shadow   shadow off                                    
                                                                
| : (shadow   isfile@ IF capacity 2/ exit THEN  shadow @ ;      
                                                                
| : >shadow  ?stamp   \ switch to shadow screen                 
      (shadow dup  scr @  u> not IF negate THEN scr +! .all ;   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 26 Hexblock 1A 
\ load and show screens                           ks 02 mar 88  
                                                                
| : showoff  ['] exit 'name !  normal  ;                        
                                                                
| : show ( -- )  blk @ 0= IF  showoff exit  THEN                
     >in @ 1- r# !  edit-at  imprint   blk @  scr @ - 0=exit    
     blk @  scr ! normal curoff .all invers curon ;             
                                                                
| : showload  ( -- )  ?stamp  save-buffers                      
     ['] show 'name !  curon invers                             
     adr .status push  ['] noop is .status                      
     scr @  scr push  scr off  r# push  r# @  (load  showoff ;  
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 27 Hexblock 1B 
\ find strings                                    ks 20 dez 87  
| Variable insert-buffer                                        
| Variable find-buffer                                          
                                                                
| : 'insert ( -- addr )  insert-buffer @ ;                      
| : 'find   ( -- addr )  find-buffer @ ;                        
                                                                
| : .buf ( addr -- )   count type ." |"  &80 col - spaces  ;    
                                                                
| : get ( addr -- )  >r at?  r@ .buf                            
    2dup at  r@ 1+ c/l expect  span @ ?dup IF  r@ c!  THEN      
    at r> .buf ;                                                
                                                                
| : get-buffers    dy l/s + 2+  dx 1-  2dup  at                 
       ." find:    |"  'find   get     swap 1+ swap 2- at       
     ." ? replace: |"  'insert get ;                            
\ *** Block No. 28 Hexblock 1C 
\                                                 ks 20 dez 87  
  Code match    ( addr1 len1 string -- addr2 len2 )             
     D W mov   W ) D- mov   $FF # D and  0= ?[  D pop   Next  ]?
     W inc   D dec   C pop   I A mov   I pop   A push           
     W ) A- mov   W inc   ?capital # call   A- A+ mov   D C sub 
     >= ?[  I inc   Label done   I dec                          
               A pop   I push   A I mov   C D add   Next  ]?    
     [[  byte lods   ?capital # call   A+ A- cmp  0=            
        ?[  D D or  done 0= not ?]                              
            I push   W push   C push   A push   D C mov         
            [[  byte lods   ?capital # call   A+ A- xchg        
                W ) A- mov   W inc   ?capital # call   A+ A- cmp
                0= ?[[  C0= ?]   A pop   C pop                  
                                 W pop   I pop   done ]]        
                    ]?  A pop   C pop   W pop   I pop           
     ]?  C0= ?]  I inc   done ]]   end-code                     
\ *** Block No. 29 Hexblock 1D 
\ search for string                                   UH 11mai88
                                                                
| : skip ( addr -- addr' )  'find c@ + ;                        
                                                                
| : search  ( buf len string -- offset flag )                   
     >r stash r@ match  r> c@ <                                 
     IF  drop 0= false exit  THEN  swap - true ;                
                                                                
| : find?  ( -- r# f )  'cursor #remaining 'find search ;       
                                                                
| : searchthru ( -- r# scr )                                    
    find? IF  skip cursor +  scr @ exit THEN  drop              
    capacity   scr @ 1+                                         
    ?DO  I 2 3 at 6 .r   I block b/blk 'find search             
      IF  skip I endloop exit  THEN  stop? Abort" Break!"       
    LOOP  true Abort" not found!" ;                             
\ *** Block No. 30 Hexblock 1E 
\ replace strings                                     UH 14mai88
| : replace? ( -- f )  dy l/s + 3+ dx 3 - at                    
      key dup #cr = IF line# redisplay true Abort" Break!" THEN 
      capital Ascii R = ;                                       
                                                                
| : "mark ( -- )   r# push                                      
     'find count dup negate c  edit-at  invers type normal ;    
                                                                
| : (replace  'insert c@  'find c@ - ?fit                       
      r# push  'find c@ negate c                                
      'cursor #after  'find c@ delete                           
      'insert count   'cursor #after  insert  modified ;        
                                                                
| : "replace   get-buffers  BEGIN searchthru                    
      scr @ - ?dup IF ?stamp  scr +! .all THEN  r# !  imprint   
      "mark  replace? IF (replace THEN  line# redisplay REPEAT ;
\ *** Block No. 31 Hexblock 1F 
\ Display Help-Screen, misc commands                 cas 11nov05
                                                                
| : helpfile ( -- ) fromfile push  editor.fb ;                  
| : .help ( --)                                                 
      isfile push   scr push   helpfile   scr off .screen ;     
| : help ( -- )  .help  key drop  .screen ;                     
                                                                
| : screen# ( -- scr )  scr @ ;                                 
                                                                
| Defer (fix-word                                               
                                                                
| : fix-word ( -- )  isfile@ loadfile !                         
     scr @ blk !  cursor >in !  (fix-word ;                     
                                                                
                                                                
                                                                
\ *** Block No. 32 Hexblock 20 
\ Control-Characters  IBM-PC Functionkeys             UH 10Sep87
                                                                
Forth definitions                                               
                                                                
: Ctrl ( -- c )                                                 
   name 1+ c@  $1F and   state @ IF [compile] Literal THEN ;    
immediate                                                       
                                                                
\needs #del $7F Constant #del                                   
                                                                
Editor definitions                                              
                                                                
| : flipimode    imode @ 0= imode ! .imode ;                    
                                                                
| : F ( # -- 16b )  $FFC6 swap - ;                              
| : shift ( n -- n' )  dup 0< + &24 - ;                         
\ *** Block No. 33 Hexblock 21 
\ Control-Characters  IBM-PC Functionkeys             UH 11dez88
                                                                
Create keytable                                                 
-&72   ,       -&75   ,       -&80   ,       -&77   ,           
 3 F   ,       4 F    ,       7 F    ,       8 F    ,           
Ctrl F ,       Ctrl S ,       5 F    ,       6 F    ,           
 1 F   ,       Ctrl H ,         #del ,       -&83   ,           
                              Ctrl Y ,       Ctrl N ,           
-&82   ,                                                        
   #cr ,         #tab ,   #tab shift ,                          
 -&119 ,       -&117  ,        2 F   ,       Ctrl U ,           
Ctrl E ,         #esc ,       Ctrl L ,    9 F shift ,           
-&81   ,       -&73   ,       9 F    ,     &10 F    ,           
-&71   ,       -&79   ,      -&118   ,     -&132    ,           
#lf    ,                                                        
here keytable - 2/ Constant #keys                               
\ *** Block No. 34 Hexblock 22 
\ Try a screen Editor                                 UH 11dez88
                                                                
Create: actiontable                                             
curup           curleft         curdown         curright        
line>buf        char>buf        buf>line        buf>char        
fix-word        screen#         copyline        copychar        
help            backspace       backspace       delete-char     
( insert-char )                 delete-line     insert-line     
flipimode                      ( clear-line     clear>  )       
<cr>            +tab            -tab                            
top             >""end          "replace        undo            
update-exit     flushed-exit    showload        >shadow         
n               b               a               mark            
<line           line>           split           join            
new-screen ;                                                    
here actiontable -  2/ 1-   #keys -  abort( # of actions)       
\ *** Block No. 35 Hexblock 23 
\ find keys                                       ks 20 dez 87  
                                                                
| : findkey  ( key -- adr/default )                             
  #keys 0 DO  dup  keytable [F] I 2* + @ =                      
     IF drop   [E] actiontable  [F] I 2* + @ endloop exit THEN  
  LOOP drop ['] putchar ;                                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 36 Hexblock 24 
\ allocate buffers                                    UH 01Nov86
                                                                
c/l 2* | Constant cstack-size                                   
                                                                
| : nextbuf ( adr -- adr' )   cstack-size + ;                   
                                                                
| : ?clearbuffer   pad (pad @ = ?exit                           
      pad     dup (pad !                                        
      nextbuf dup find-buffer   !   'find off                   
      nextbuf dup insert-buffer !   'insert off                 
      nextbuf dup 0 chars 2!                                    
      nextbuf     0 lines 2! ;                                  
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 37 Hexblock 25 
\ enter and exit the editor, editor's loop            UH 11mai88
                                                                
| Variable jingle  jingle on  | : bell  07 charout jingle off ; 
                                                                
| : clear-error ( -- )                                          
     jingle @ ?exit  dy l/s + 1+ dx  at c/l spaces jingle on ;  
                                                                
| : fullquit ( -- ) BEGIN ?clearbuffer edit-at  key dup char c! 
      findkey imprint execute ( .status )  clear-error REPEAT ; 
                                                                
| : fullerror  ( string -- )  jingle @ IF bell THEN  count      
     dy l/s + 1+ over 2/ dx $20 + swap - at invers type normal  
     &80 col - spaces  remember  .all quit ;                    
                                                                
| : install   ( -- )                                            
     ['] fullquit Is 'quit  ['] fullerror errorhandler !  ;     
\ *** Block No. 38 Hexblock 26 
\ enter and exit the Editor                           UH 11mai88
                                                                
Forth definitions                                               
                                                                
: v   ( -- )                                                    
    [E] 'start drop  get-id   install-screen                    
    install  ?clearbuffer                                       
    border .all .imode .status quit ;                           
                                                                
  ' v Alias ed                                                  
                                                                
: l   ( scr -- )   1 arguments scr !  [E] top  [F] v ;          
                                                                
  ' l Alias edit                                                
                                                                
                                                                
\ *** Block No. 39 Hexblock 27 
\ savesystem    enhanced view                         UH 24jun88
                                                                
: savesystem   [E] id off  (pad off   savesystem ;              
                                                                
Editor definitions                                              
| : >find  ?clearbuffer  >in push                               
      name  dup c@ 2+ >r  bl over c! r>  'find place ;          
                                                                
Forth definitions                                               
: fix    [ Dos ]  >find ' @view >file                           
   isfile !  scr !   [E] top curdown                            
   find? IF  skip 1-  THEN  c v ;                               
                                                                
' fix Is (fix-word                                              
                                                                
                                                                
\ *** Block No. 40 Hexblock 28 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 41 Hexblock 29 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
