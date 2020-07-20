\ *** Block No. 0 Hexblock 0 
\\                   *** Screen-Editor ***             10aug86we
                                                                
Dieses File enthÑlt den volksFORTH - Editor.                    
Er basiert auf dem Editor im F83 von Laxen/Perry, besitzt aber  
erheblich erweiterte Funktionen (Zeichen- und Zeilenstack) und  
ist ein vollstÑndig in GEM integrierter Fullscreen-Editor.      
                                                                
Obwohl die Steuerung mit Maus und Menuzeile erfolgt, kînnen     
ihn die 'Profis' auch vollstÑndig Åber Controltasten bedienen,  
                                                                
Die Dauerhilfe-Funktion macht eine Funktionsbeschreibung Åber-  
flÅssig. Solange im HILFE-Menu Dauerhilfe gewÑhlt ist, erscheint
vor der AusfÅhrumg jeder Editor-Funktion ein erlÑuternder Text  
mit der Mîglichkeit zum Abbruch. Dies gilt jedoch nicht, wenn   
die Funktion per Tastendruck aufgerufen wurde.                  
                                                                
\ *** Block No. 1 Hexblock 1 
\ Load Screen for the Editor                         cas20130105
                                                                
Onlyforth  GEM also                                             
include ediicon.fb                                              
                                                                
| Variable (dx  2 (dx !           | Variable (dy    4 (dy !     
| : dx     (dx @ ;                | : dy     (dy @ ;            
                                                                
\needs -text         .( strings needed !!)    abort             
\needs file?         .( Filesystem needed !!) abort             
include gem\supergem.fb                                         
include gem\gemdefs.fb                                          
include edwindow.fb                                             
                                                                
Forth definitions                                               
 1 $2C  +thru                                                   
\ *** Block No. 2 Hexblock 2 
\ Editor Variable                                      10sep86we
                                                                
Variable 'scr  1 'scr !         Variable 'r#  'r# off           
Variable 'edifile                                               
                                                                
?head @      1 ?head !                                          
                                                                
Variable changed                Variable edistate               
Variable edifile                                                
Variable ycur                                                   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\ Edi move cursor with position-checking or cyclic     30aug86we
                                                                
: c  ( n -- )   \ checks the cursor position                    
    r# @ +  dup 0 b/blk uwithin 0= abort" Border!"  r# ! ;      
                                                                
\  : c  ( n -- )   \ moves cyclic thru the screen               
\   r# @ +  b/blk mod  r# ! ;                                   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
\ Move the Editor's cursor around                      08aug86we
                                                                
: top          ( -- )       r# off ;                            
: cursor       ( -- n )     r# @ ;                              
: t            ( n -- )     c/l * cursor - c ;                  
: line#        ( -- n )     cursor  c/l  /  ;                   
: col#         ( -- n )     cursor  c/l  mod  ;                 
: +t           ( n -- )     line# + t   ;                       
: 'start       ( -- addr )  scr @ block ;                       
: 'cursor      ( -- addr )  'start  cursor  + ;                 
: 'line        ( -- addr )  'cursor  col# -  ;                  
: #after       ( -- n )     c/l col# -  ;                       
: #remaining   ( -- n )     b/blk cursor - ;                    
: #end         ( -- n )     #remaining col# +  ;                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
\ Move the Editors cursor                              08aug86we
                                                                
: curup      c/l negate c ;                                     
: curdown    c/l c ;                                            
: curleft     -1 c ;                                            
: curright     1 c ;                                            
: +tab       cursor  $10 / 1+ $10 * cursor - c ;                
: -tab       cursor  8 mod  negate  dup 0=  8 * +  c ;          
: >""end     'start b/blk -trailing nip  b/blk 1- min  r# ! ;   
: <cr>       line# t curdown ;                                  
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 6 Hexblock 6 
\ buffers                                              14sep86we
                                                                
: modified   ( -- )             scr @ block drop  update        
   changed @ ?exit   edistate off  changed on  ;                
                                                                
&84 Constant c/pad                                              
&42 Constant c/buf                                              
                                                                
: 'work     ( -- work-buf )     pad     c/pad + ;               
: 'insert   ( -- ins-buf )      'work   c/pad + ;               
: 'find     ( -- find-buf )     'insert c/buf + ;               
                                                                
: 'find+    ( n1 -- n2 )        'find c@ +  ;                   
                                                                
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
\ Errorchecking                                        09sep86we
                                                                
: ?bottom  ( -- )     'start b/blk + c/l -  c/l  -trailing nip  
                          abort" You would lose a line" ;       
                                                                
: ?end  ( -- )        'line c/l + 1- c@  bl -                   
                          abort" You would lose a char" ;       
                                                                
: ?range  ( n -- n )   dup  0 capacity uwithin not              
                          abort" Out of range!" ;               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
\ Graphics for display                                 23aug86we
                                                                
: lineclr      ( line# -- )                                     
   wi_x   swap cheight *  wi_y +                                
   over  wi_width +   over cheight +  fbox  ;                   
                                                                
: lineinsert   ( line# -- )                                     
   wi_x   over  cheight *  wi_y +                               
   wi_width   over  l/s 1- cheight *  wi_y +  swap -            
   2over cheight +   scr>scr    lineclr   ;                     
                                                                
: linedelete   ( line# -- )                                     
   wi_x    swap 1+ cheight *  wi_y +                            
   wi_width   over  l/s cheight *  wi_y +  swap -               
   2over cheight -   scr>scr    l/s 1- lineclr   ;              
                                                                
\ *** Block No. 9 Hexblock 9 
\ Editor-Window Title and Status-Line                cas20130105
                                                                
: 'workblank                                                    
   'work dup $sum !   dup off   dup 1+ c/l blank   c/l + off ;  
                                                                
                                                                
: update$   ( -- string )                                       
   scr @ updated? not IF  " not updated" exit THEN  " updated" ;
                                                                
: .edistate      edistate @ ?exit  edistate on  'workblank      
   "  Scr # " count $add   scr @ extend  <# # # # #> $add       
   'work c@ 2+ 'work c!    update$ count $add                   
   'work 1+ wi_status ;                                         
                                                                
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
\ screen display                                       30aug86we
                                                                
: .edifile       'workblank   1 'work c!                        
    isfile@  ?dup 0=  IF  " DIRECT"  ELSE  2- >name  THEN       
    count $add  'work count + 1+  c/l  min off                  
    'work 1+  wi_title ;                                        
                                                                
: 'line#   ( line# -- addr count )                              
   dup dy +  dx at   c/l * 'start +  c/l ;                      
                                                                
: .line      ( line# -- )  dup lineclr  'line# -trailing type ; 
: redisplay  ( line# -- )  'line# type ;                        
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 11 Hexblock B 
\ screen display                                       14sep86we
                                                                
&18 Constant id-len                                             
Create id   id-len allot   id id-len erase                      
                                                                
: stamp       id  1+ count  'start c/l +  over -  swap  cmove ; 
: ?stamp      changed @ IF  stamp  THEN ;                       
                                                                
                                                                
: edilist     edistate off  changed off                         
              vslide_size   scr @ vslide                        
              .edifile  .edistate   l/s 0 DO  I  .line  LOOP ;  
                                                                
: undo        scr @ block drop    prev @ emptybuf   edilist ;   
                                                                
: do_redraw   hide_c  wi_clear redraw_screen  edilist ;         
\ *** Block No. 12 Hexblock C 
\ Edi Variables,                                       23aug86we
                                                                
Variable (pad   (pad off                                        
: memtop  ( -- addr )   sp@ $100 - ;                            
                                                                
Variable chars                  Variable #chars                 
: 'chars  ( -- addr )   chars @  #chars @ + ;                   
                                                                
Variable lines                  Variable #lines                 
: 'lines  ( -- addr )   lines @  #lines @ + ;                   
                                                                
Variable (key                                                   
                                                                
Variable imode  imode off                                       
                                                                
                                                                
\ *** Block No. 13 Hexblock D 
\ Edi line handling                                    09aug86we
                                                                
: linemodified   modified  line# redisplay ;                    
                                                                
: clrline        'line c/l blank        linemodified ;          
: clrright       'cursor #after blank   linemodified ;          
                                                                
: delline        'line #end c/l delete                          
                 line# linedelete   modified ;                  
: backline       curup  delline ;                               
                                                                
: instline       ?bottom   'line c/l over #end insert           
                 line# lineinsert  clrline ;                    
                                                                
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
\ Edi line handling                                    09aug86we
                                                                
: @line         'lines  memtop u> abort" line buffer full"      
                'line 'lines c/l cmove  c/l #lines +! ;         
                                                                
: copyline      @line curdown ;                                 
: line>buf      @line delline ;                                 
                                                                
: !line         c/l negate #lines +!  'lines 'line c/l cmove    
                linemodified ;                                  
                                                                
: buf>line      #lines @ 0= abort" line buffer empty"           
                ?bottom  instline  !line ;                      
                                                                
                                                                
                                                                
\ *** Block No. 15 Hexblock F 
\ Edi char handling                                    09aug86we
                                                                
: delchar      'cursor #after 1 delete  linemodified ;          
: backspace    curleft  delchar ;                               
                                                                
: inst1        ?end   'cursor 1 over #after insert ;            
: instchar     inst1  bl 'cursor c!  linemodified ;             
                                                                
: @char        'chars 1-  lines @  u> abort" char buffer full"  
               'cursor c@  'chars c!   1 #chars +! ;            
: copychar     @char curright ;                                 
: char>buf     @char delchar ;                                  
                                                                
: !char        -1 #chars +!  'chars c@ 'cursor c! ;             
: buf>char     #chars @ 0= abort" char buffer empty"            
               inst1  !char  linemodified ;                     
\ *** Block No. 16 Hexblock 10 
\ from Screen to Screen ...                            22oct86we
                                                                
: setscreen  ( n -- )       ?stamp  ?range  scr !  edilist ;    
: n    scr @  1+  setscreen ;                                   
: b    scr @  1-  setscreen ;                                   
                                                                
: >shadow   ( n1 -- n2 ) capacity 2/  2dup < IF + ELSE - THEN ; 
: w                      scr @  >shadow  setscreen ;            
                                                                
: (mark       scr @ 'scr !   r# @ 'r# !   isfile@ 'edifile ! ;  
: mark        (mark  true abort" marked !" ;                    
                                                                
: a           ?stamp   'edifile @   [ Dos ]  dup searchfile drop
                        isfile@ 'edifile !   !files             
              'r# @  r# @ 'r# !  r# !                           
              'scr @   scr @ 'scr !   ?range scr !  edilist ;   
\ *** Block No. 17 Hexblock 11 
\ splitting a line, replace                            17aug86we
                                                                
: split          ?bottom  pad c/l 2dup blank                    
   'cursor #remaining insert   linemodified                     
   col#   <cr>  line# lineinsert                                
   'start cursor +  c/l  rot  delete  linemodified ;            
                                                                
: ins      'insert count  under 'cursor #after  insert  c ;     
                                                                
: r                                                             
   c/l   'line over -trailing  nip  -                           
   'insert c@  'find c@ -  <  abort" not enough room"           
   'find c@  dup negate c  'cursor #after rot  delete  ins      
   linemodified ;                                               
                                                                
                                                                
\ *** Block No. 18 Hexblock 12 
\ find und search                                      30aug86we
                                                                
: >last?   ( -- f )    :dfright state_gaddr l@  1 and ;         
: >last                :dfright select   :dfleft   deselect ;   
: >1st                 :dfleft  select   :dfright  deselect ;   
                                                                
Variable fscreen                                                
                                                                
: find?  ( - n f )   'find count  'cursor #remaining  search ;  
                                                                
: s   BEGIN   find?   IF  'find+ c  edilist  exit  THEN   drop  
              fscreen @  scr @   -  ?dup   stop? 0= and         
      WHILE   0< IF  -1  ELSE  1  THEN  scr +! top scr @ vslide 
      REPEAT  :sfind tree!                                      
   >last? IF  >1st :df1st  ELSE  >last :dflast THEN             
   getnumber drop fscreen !  edilist  true abort" not found" ;  
\ *** Block No. 19 Hexblock 13 
\ Search-Findbox auswerten                             24aug86we
                                                                
: initfind   ( -- )                                             
   :dfmatch select   :dfignore deselect   >last                 
   1 extend :df1st putnumber                                    
   capacity 1- extend :dflast putnumber ;                       
                                                                
: getfind    ( -- n )                                           
   :dfignore state_gaddr l@  1 and  caps !                      
   >last?  IF  :dflast  ELSE  :df1st  THEN  getnumber drop      
   capacity 1- min                                              
   :dffstrin 'find getstring   :dfrstrin 'insert getstring ;    
                                                                
: do_fbox   ( -- button )    :sfind tree!                       
   edifile @ isfile@ - IF  isfile@ edifile ! initfind  THEN     
   show_object  :dffstrin form_do  dup deselect  hide_object ;  
\ *** Block No. 20 Hexblock 14 
\ Replacing ...                                        24aug86we
                                                                
Variable ?replace                                               
                                                                
: show_replace   ( -- )                                         
   &320 &200 &10 &10 little 4!                                  
   col# dx + 2-  cwidth *   line# dy + 1+ cheight *             
     2dup 0 objc_setpos   0 objc_getwh big 4!                   
   big 4@  scr>mem1    1  little 4@  big 4@  form_dial          
   0 ( install) 3 ( depth)  big 4@ objc_draw  show_c ;          
                                                                
: replace  ( -- )                                               
   :fbox tree!      BEGIN                                       
   show_replace   0 form_do  dup deselect  hide_object          
   dup :fboxcanc - WHILE  :fboxyes = IF  r  THEN  s             
   REPEAT drop ;                                                
\ *** Block No. 21 Hexblock 15 
\ Editor's find and replace                            24aug86we
                                                                
Variable (findbox   (findbox off                                
                                                                
: repfind  ( -- )                                               
  (findbox @  'find c@  and   0= abort" use find first"         
  ?stamp   fscreen @  capacity 1-  min  fscreen !               
  s ?replace @  IF replace  THEN  ;                             
                                                                
: edifind  ( -- )                                               
   do_fbox     :dfcancel case? ?exit                            
     :dfreplac =  ?replace  swap IF  on  ELSE  off  THEN        
   :edimenu tree!  :repeat 1 menu_ienable  (findbox on          
     :sfind tree!  getfind fscreen !  repfind ;                 
                                                                
                                                                
\ *** Block No. 22 Hexblock 16 
\ exiting the Editor                                   30aug86we
                                                                
Defer resetmouse                                                
                                                                
: done   ( ff addr  -- tf )                                     
   :edimenu tree! 0 menu_bar   resetmouse  hide_c               
   wi_close   ycur @ 0 at cr ." Scr #"  scr @ 3 .r  2 spaces    
   count type  true ;                                           
                                                                
: cdone   ( ff -- tf )   prev @ emptybuf   " canceled" done ;   
: sdone   ( ff -- tf )   ?stamp save-buffers  " saved" done ;   
: xdone   ( ff -- tf )   ?stamp   update$              done ;   
: ldone   ( ff -- tf )   drop true                              
                         ?stamp save-buffers " loading" done ;  
                                                                
                                                                
\ *** Block No. 23 Hexblock 17 
\ get User's ID, jump to screen                        24aug86we
                                                                
: do_getid                                                      
   :sgetid tree!     id 1+ :idtext putstring                    
   show_object  :idtext form_do  dup deselect  hide_object      
   :idcancel case? ?exit                                        
   :noid = IF  id off  exit THEN                                
   :idtext id 1+ getstring ;                                    
                                                                
: get-id                                                        
   id c@  ?exit  1 id c!  do_getid ;                            
                                                                
: jumpscreen         :sgetscr tree!                             
   pad  dup off  :scrnr putstring                               
   show_object  :scrnr form_do  dup deselect  hide_object       
   :sgcancel = ?exit   :scrnr getnumber drop   setscreen ;      
\ *** Block No. 24 Hexblock 18 
\ insert- and overwrite-mode                           24aug86we
                                                                
: mark_item   ( item# -- )     1 menu_icheck ;                  
: clr_item    ( item# -- )     0 menu_icheck ;                  
                                                                
: setimode    imode on   :edimenu tree!                         
              :imode mark_item  :omode clr_item ;               
: clrimode    imode off  :edimenu tree!                         
              :omode mark_item  :imode clr_item ;               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 25 Hexblock 19 
\ viewing words                                        24aug86we
                                                                
: >view   ( -- )                                                
    'find count pad place  pad capitalize  bl pad count + c!    
     find 0= abort" Haeh?"                                      
    >name ?dup 0= abort" no view-field"                         
    4- @  ?dup 0= abort" hand made"                             
    (view scr !  top curdown  find? 0= IF drop exit THEN        
    'find+ c ;                                                  
                                                                
: do_view  ( -- )                                               
   :sview tree!   pad  dup off  :svword putstring               
   show_object  :svword form_do  dup deselect  hide_object      
   :idcancel case? ?exit                                        
   :svword 'find getstring   :svmark = IF  (mark  THEN          
   >view  edilist ;                                             
\ *** Block No. 26 Hexblock 1A 
\ Table of keystrokes                                  10aug86we
                                                                
Create keytable                                                 
$4800 0 , ,     $4B00 0 , ,     $5000 0 , ,     $4D00 0 , ,     
$4838 1 , ,     $4B34 1 , ,     $5032 1 , ,     $4D36 1 , ,     
                                $5000 2 , ,     $7400 2 , ,     
$0E08 0 , ,     $537F 0 , ,     $5200 0 , ,     $240A 2 , ,     
$0E08 1 , ,     $537F 1 , ,     $5230 1 , ,     $6100 0 , ,     
$1709 2 , ,     $180F 2 , ,     $1205 2 , ,     $531F 2 , ,     
$1C0D 0 , ,     $1C0D 1 , ,     $0F09 0 , ,     $0F09 1 , ,     
$4700 0 , ,     $4737 1 , ,     $2207 2 , ,     $2F16 2 , ,     
$2106 2 , ,     $1312 2 , ,     $320D 2 , ,                     
$011B 0 , ,     $1F13 2 , ,     $2D18 2 , ,     $260C 2 , ,     
$310E 2 , ,     $3002 2 , ,     $1E01 2 , ,     $1117 2 , ,     
                                                                
here keytable - 2/ 2/  Constant #keys                           
\ *** Block No. 27 Hexblock 1B 
\ Table of actions                                     11aug86we
                                                                
Create actiontable  ]                                           
curup           curleft         curdown         curright        
line>buf        char>buf        buf>line        buf>char        
                                copyline        copychar        
backspace       delchar         instchar        jumpscreen      
backline        delline         instline        undo            
setimode        clrimode        clrline         clrright        
<cr>            split           +tab            -tab            
top             >""end          do_getid        do_view         
edifind         repfind         mark                            
cdone           sdone           xdone           ldone           
n               b               a               w               
                                                                
[  here actiontable -  2/  #keys -  abort( # of actions)        
\ *** Block No. 28 Hexblock 1C 
\ Table of Menuevents                                  24aug86we
                                                                
Create menutable                                                
$FF c,          $FF c,          $FF c,          $FF c,          
:cutline c,     :cutchar c,     :pastelin c,    :pastecha c,    
                                :copyline c,    :copychar c,    
$FF c,          $FF c,          $FF c,          :jump c,        
:backline c,    :delline c,     :insline c,     :undo c,        
:imode c,       :omode c,       :eraselin c,    :erasrest c,    
$FF c,          :split c,       :tab c,         :backtab c,     
:home c,        :toend c,       :getid c,       :view c,        
:search c,      :repeat c,      :mark c,                        
:canceled c,    :flushed c,     :updated c,     :loading c,     
:next c,        :back c,        :alternat c,    :shadow c,      
                                                                
here menutable -   #keys -  abort( # of menuitems)              
\ *** Block No. 29 Hexblock 1D 
\ Table of Help-Boxes                                  24aug86we
                                                                
Create helptable                                                
$FF c,          $FF c,          $FF c,          $FF c,          
:hlicut c,      :hchcut  c,     :hlipaste c,    :hchpaste c,    
                                :hlicopy c,     :hchcopy c,     
$FF c,          $FF c,          $FF c,          :hjump c,       
:hliback c,     :hlidel c,      :hliins c,      :hexundo c,     
:hspins c,      :hspover c,     :hlierase c,    :hlirest c,     
$FF c,          :hlisplit c,    :hcutabr c,     :hcutabl c,     
:hcuhome c,     :hcuend c,      :hspgetid c,    :hview c,       
:hspfind c,     :hsprepea c,    :hscmark c,                     
:hexcancl c,    :hexsave c,     :hexupdat c,    :hexload c,     
:hscnext c,     :hscback c,     :hscalter c,    :hscshado c,    
                                                                
here helptable -   #keys -  abort( # of menuitems)              
\ *** Block No. 30 Hexblock 1E 
\ Prepare multi-event                                  09sep86we
                                                                
Variable mflag   mflag off                                      
                                                                
: ediprepare                                                    
   %00110111                                                    
   1  1  1                                                      
   mflag @                                                      
      dx cwidth *  dy cheight *  c/l cwidth *  l/s cheight *    
   0 0 0 0 0                                                    
   0 0                                                          
   intin $10 array!  message >absaddr  addrin 2! ;              
                                                                
' pause  | Alias ev-timer                                       
: ev-r1    1 mflag 1+ ctoggle ;                                 
                                                                
\ *** Block No. 31 Hexblock 1F 
\ Button Event                                         24aug86we
                                                                
Variable ?cursor  ?cursor off                                   
                                                                
: curon    ?cursor @ ?exit  ?cursor on                          
   3 swr_mode   1 sf_color  1 sf_interior  0 sf_perimeter       
   at?  cwidth *  swap cheight *                                
   over cwidth 1- +   over cheight + 1-    bar ;                
                                                                
: curoff  ?cursor off  curon  ?cursor off ;                     
                                                                
: ev-button       mflag @  0= ?exit                             
   intout 4+ @  cheight /  dy -  c/l *                          
   intout 2+ @  cwidth  /  dx -  +  r# !    hide_c curoff ;     
                                                                
                                                                
\ *** Block No. 32 Hexblock 20 
\ Key event                                            17aug86we
                                                                
: visible?  ( key -- f )   $FF and  ;                           
                                                                
: putchar  ( -- )                                               
   (key @  dup visible? 0=  abort" What?"                       
   imode @  IF  inst1  THEN  'cursor c! linemodified curright ; 
                                                                
: findkey  ( d_key -- addr )                                    
   ['] putchar -rot                                             
     #keys 0 DO  2dup  keytable  I 2* 2* +  2@ d=               
       IF  rot drop  actiontable  I 2* + @  -rot  LEAVE  THEN   
             LOOP  2drop ;                                      
                                                                
                                                                
                                                                
\ *** Block No. 33 Hexblock 21 
\ Key event                                            23aug86we
                                                                
Variable jingle   jingle on                                     
Variable ?mouse                                                 
                                                                
: edit-at     cursor c/l /mod  dy + swap dx + at ;              
                                                                
: ev-key      ?mouse off                                        
   intout &10 + dup  @ dup (key !  hide_c  edit-at curoff       
   swap 2- @  dup 1 and +  2/  findkey execute                  
   jingle on .edistate  BEGIN  getkey 0=  UNTIL ;               
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 34 Hexblock 22 
\ Message events for window                            30aug86we
                                                                
: getmessage    ( n -- n' )      2* message + @ ;               
                                                                
: wm_arrowed                                                    
   4 getmessage  1 and IF  n  exit THEN  b ;                    
                                                                
: wm_vslide                                                     
   4 getmessage  capacity 1- &1000 */  setscreen ;              
                                                                
: wm_moved                                                      
   4 getmessage   cwidth  /  1 max  &14 min  (dx !              
   5 getmessage   cheight /  1 max    5 min  3 + (dy !          
   wi_handle @  5  wi_size  wind_set   redraw_screen ;          
                                                                
                                                                
\ *** Block No. 35 Hexblock 23 
\ Message events (the menuline)                        02sep86we
                                                                
Variable ?help    ?help on                                      
                                                                
: do_help   ( n -- )                                            
   helptable + c@ alert  1 = ?exit                              
   true abort" Dann eben nicht !!" ;                            
                                                                
: do_copyr      :copyr tree!                                    
   show_object  0 form_do  deselect  hide_object ;              
                                                                
: do_menuhelp    show_c  :hhemenu alert  hide_c                 
   :edimenu tree!  1 and  :menuhelp over menu_icheck            
   ?help ! ;                                                    
                                                                
                                                                
\ *** Block No. 36 Hexblock 24 
\ Message events from menuline                         02sep86we
                                                                
: do_other  ( -- )      4 getmessage                            
    :menuhelp  case?  IF  do_menuhelp exit THEN                 
    :hmouse    case?  IF  :hhemouse alert drop exit THEN        
    :hfuncts   case?  IF  :hhef1f10 alert drop exit THEN        
    drop  do_copyr ;                                            
                                                                
: menu-message  ( -- )    message @ :mn_selected - ?exit        
   :edimenu tree!    3 getmessage  1 menu_tnormal               
   ['] do_other  4 getmessage                                   
   #keys 0 DO  dup  menutable I +  c@ =                         
      IF ?help @  IF  I do_help  THEN                           
         nip  actiontable  I 2* + @ swap  LEAVE  THEN           
            LOOP drop  execute  jingle on .edistate ;           
                                                                
\ *** Block No. 37 Hexblock 25 
\ Handle message-event                                 24aug86we
                                                                
: ev-message         hide_c  edit-at curoff                     
   message @  :mn_selected case? IF  menu-message exit  THEN    
              :wm_arrowed  case? IF  wm_arrowed   exit  THEN    
              :wm_vslid    case? IF  wm_vslide    exit  THEN    
              :wm_moved    case? IF  wm_moved     exit  THEN    
              :wm_redraw   case? IF  do_redraw    exit  THEN    
   drop ;                                                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 38 Hexblock 26 
\ Handle all events                                    30aug86we
                                                                
Create ev-flag                                                  
   :mu_mesag c,  :mu_m1 c,  :mu_button c,                       
   :mu_keybd c,  :mu_timer c,                                   
                                                                
Create: event-actions                                           
   ev-message  ev-r1  ev-button  ev-key  ev-timer  ;            
                                                                
: handle-events  ( which -- )                                   
   5 0 DO  ev-flag I + c@  over and  IF  drop I LEAVE THEN  LOOP
   2* event-actions + perform ;                                 
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 39 Hexblock 27 
\ Change mouse-movement Vector                         10sep86we
                                                                
2Variable savevec                                               
                                                                
Create newvector   Assembler                                    
   ?mouse pcrel) A0 lea    true # A0 ) move                     
   .l savevec pcrel) A0 move   A0 ) jmp   end-code              
                                                                
Code ?show_c   ?mouse R#) tst   0= IF  Next  THEN  ;c: show_c ; 
                                                                
: ex_motv    ( pusrcode -- )                                    
   contrl &14 + 2!  &126 0 0 VDI  contrl &18 + 2@  savevec 2! ; 
                                                                
: setmousevec       newvector >absaddr  ex_motv ;               
: resetmousevec     savevec 2@ ex_motv ;                        
' resetmousevec Is resetmouse                                   
\ *** Block No. 40 Hexblock 28 
\ The Editor's LOOP                                    02sep86we
                                                                
: ediloop  r0 @ rp!                                             
   BEGIN   edit-at curon   ?show_c   false                      
           ediprepare evnt_multi  handle-events   UNTIL ;       
                                                                
: alarm    bell jingle off ;                                    
                                                                
: edierror  ( string -- )                                       
   jingle @ 0= IF  drop ediloop  THEN     alarm                 
   'workblank   c/l 2/ 'work c!   count c/l 2/ min  $add        
   'work 1+ wi_status   edistate off   ediloop ;                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 41 Hexblock 29 
\ Installing the Editor                                20nov86we
                                                                
Create   ediresource   &12 allot                                
Variable edihandle                                              
                                                                
: setediresource       ediresource ap_ptree &12 cmove ;         
                                                                
: ?clearbuffer                                                  
   pad (pad @ = ?exit  pad (pad !                               
   'find b/blk + dup chars !  c/l 2* +  lines !                 
   #chars off  #lines off  'find off  'insert off (findbox off ;
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 42 Hexblock 2A 
\ Installing the Editor                                20nov86we
                                                                
: finstall   ( -- )                                             
   pad memtop u> abort" No room for buffers!"                   
   get-id    changed off   row ycur !   setmousevec             
   ?clearbuffer   ?cursor off                                   
   ap_ptree &12 cpush  setediresource                           
   grhandle push  edihandle @ grhandle !                        
   wi_open   :edimenu tree! 1 menu_bar                          
   errorhandler push  ['] edierror errorhandler !               
   r0 push  rp@ r0 !   ediloop ;                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 43 Hexblock 2B 
\ Entering the Editor                                  11sep86we
                                                                
Forth definitions   ?head !                                     
                                                                
| : ?load    0= ?exit  scr @ r# @ (load ;                       
                                                                
: v   ( -- )     scr @ ?range drop   finstall  ?load ;          
                                                                
: l   ( scr -- ) 1 arguments ?range  scr !   top  v ;           
                                                                
| : >find        bl word count 'find place ;                    
                                                                
: view  ( -- )   >find  >view v ;                               
                                                                
                                                                
                                                                
\ *** Block No. 44 Hexblock 2C 
\ Init the Editor for different resolutions            18sep86we
                                                                
| : q_extnd   ( info_flag -- )    intin !  &102 0 1 VDI ;       
                                                                
| : setMFDB   ( addr_of_MFDB -- )     >r                        
     0 q_extnd  intout 2@  r@ 4+  2!    intout @ $10 /  r@ 6 + !
     1 q_extnd  intout 8 + @  r> &12 +  ! ;                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 45 Hexblock 2D 
\ save-system for Editor                             cas20130105
                                                                
| : edistart     grinit  rsrc_load" ediicon.rsc"   0 graf_mouse 
     grhandle @ edihandle !   ap_ptree ediresource &12 cmove    
     memMFDB1 setMFDB   memMFDB2 setMFDB                        
    ['] noop  [ ' drvinit >body ] Literal ! ;                   
                                                                
: bye    grexit bye ;           grinit                          
                                                                
: save-system     id off  r# off  1 scr !  'r# off  1 'scr !    
   (findbox off  (pad off                                       
   ['] edistart  [ ' drvinit >body ] Literal !                  
   [ ' forth83.fb >body ] Literal 'edifile !                    
   flush  save-system   bye ;                                   
                                                                
                                                                
\ *** Block No. 46 Hexblock 2E 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 47 Hexblock 2F 
\\                   *** Screen-Editor ***             17aug86we
                                                                
In den Editor gelangt man mit  l ( Screen-Nr. -- ), mit v oder  
view. view verlangt als weitere Eingabe ein FORTH-Wort und      
sucht dann den Screen, auf dem das Wort definiert wurde.        
                                                                
Alle Eingaben werden unmittelbar in den Blockbuffer geschrieben,
der den aktuellen Screen enthÑlt.                               
                                                                
Die Position des Cursors hÑngt von 2 Variablen ab:              
scr enthÑlt die Nummer des aktuellen Screens;                   
r#  bestimmt die Position des Cursors.                          
Beides sind Systemvariable, die auch beim Compilieren benutzt   
werden. Bei Abbruch wegen eines Fehlers ruft man den Editor mit 
v  auf. Der Cursor steht hinter dem Wort, das den Abbruch       
ausgelîst hat.                                                  
\ *** Block No. 48 Hexblock 30 
\ Load Screen for the Editor                           24aug86we
                                                                
bindet Vocabulary GEM mit in die Suchreihenfolge ein.           
Labels fÅr Editor-Resource                                      
                                                                
(dx und (dy sind Variable, die die Lage des Editorfensters      
relativ zur linken oberen Ecke des Bildschirms angeben.         
Der Editor benîtigt einige Definitionen aus anderen Files.      
- fÅr die Suchfunktionen.                                       
- falls kein File-Interface vorhanden ist.                      
- fÅr das Fenster                                               
Labels fÅr Gem-Aufrufe                                          
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 49 Hexblock 31 
\ Editor Variable                                      26oct86we
                                                                
Screen-Nr. und Cursorposition vom markierten Screen             
File fÅr markierten Screen                                      
                                                                
Alle folgenden Definitionen werden headerless compiliert.       
                                                                
Flag fÅr énderungen am Screen;  Flag, ob Statuszeile neu ge-    
File, das editiert wird         schrieben werden muû            
ycur ist die Cursorposition beim Aufruf des Editors             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 50 Hexblock 32 
\ Edi move cursor with position-checking or cyclic     30aug86we
                                                                
bewegt den Cursor um n Stellen vor- bzw. rÅckwÑrts.             
 Wird der Cursor Åber Anfang oder Ende des Screens hinausbewegt,
 stehen zwei Mîglichkeiten zur Wahl:                            
 - Kommando wird nicht ausgefÅhrt.                              
 - Der Screen wird zyklisch durchlaufen.                        
                                                                
WÑhlen Sie durch 'Wegkommentieren' und Neucompilieren des       
 Editors.                                                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 51 Hexblock 33 
\ Move the Editor's cursor around                      05aug86we
                                                                
setzt Cursor in die obere linke Ecke (Home).                    
n ist die aktuelle Position des Cursors (Offset von Home)       
setzt Cursor auf Beginn der Zeile n.                            
n ist die Zeile, in der der Cursor steht.                       
n ist die Spalte, in der der Cursor steht.                      
bewegt Cursor um n Zeilen vor- bzw. rÅckwÑrts auf Zeilenanfang. 
addr ist die Anfangsadresse des aktuellen Blocks im Speicher.   
addr ist die der Cursorposition entsprechende Speicheradresse.  
addr ist die Speicheradresse des Beginns der Cursorzeile.       
n ist die Stellenanzahl zwischen Cursorposition und Zeilenende. 
n ist die Stellenanzahl zwischen Cursorposition und Blockende.  
n ist die Stellenanzahl zwischen Cursorzeile und Blockende.     
                                                                
                                                                
\ *** Block No. 52 Hexblock 34 
\ Move the Editors cursor                              07aug86we
                                                                
setzt Cursor um eine Zeile nach oben.                           
setzt Cursor um eine Zeile nach unten.                          
setzt Cursor um ein Zeichen nach links.                         
setzt Cursor um ein Zeichen nach rechts.                        
setzt Cursor um eine Tabulatorposition nach vorn (s.unten).     
setzt Cursor um eine Tabulatorposition zurÅck (s.unten).        
setzt Cursor auf das letzte Zeichen des Screens.                
setzt Cursor auf Beginn der nÑchsten Zeile.                     
                                                                
                                                                
VorwÑrtstabs:                                                   
+               +               +               +               
RÅckwÑrtstabs:                                                  
-       -       -       -       -       -       -       -       
\ *** Block No. 53 Hexblock 35 
\ buffers                                              24aug86we
                                                                
markiert einen geÑnderten Block zum ZurÅckschreiben auf Disk    
 setzt Flag fÅr ?stamp und lîscht Flag fÅr .edistate            
                                                                
Byteanzahl in PAD (min. &84 nach 83-Standard!).                 
Byteanzahl in einem Buffer (&40 durch Resource vorgegeben).     
                                                                
'work, 'insert und 'find sind Buffer, die beim Aufruf des       
 Editors oberhalb von PAD eingerichtet werden.                  
 'work dient zur Aufbreitung von Strings fÅr die Statuszeile    
 'find enthÑlt den Suchstring und 'insert den Replacestring.    
n2 ist n1 zuzÅglich der LÑnge des Findbuffers.                  
                                                                
                                                                
                                                                
\ *** Block No. 54 Hexblock 36 
\ Errorchecking                                        17aug86we
                                                                
bricht ab, wenn beim EinfÅgen einer Zeile kein Platz mehr ist.  
                                                                
                                                                
bricht ab, wenn beim EinfÅgen eines Zeichens kein Platz mehr ist
                                                                
                                                                
bricht ab, wenn ein Screen auûerhalb des aktuellen Files edi-   
 tiert werden soll.                                             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 55 Hexblock 37 
\ Graphics for display                                 23aug86we
                                                                
lîscht Zeile n durch öberschreiben mit einem weiûen Rechteck    
 x - und y - Koordinate der linken oberen Ecke                  
 x - und y - Koordinate der rechten unteren Ecke                
                                                                
fÅgt auf dem Bildschirm an der Cursorposition eine Leerzeile ein
 x - und y - Koordinate des zu verschiebenden Rechtecks         
 Breite setzen und Hîhe berechnen                               
 x - und y - Koordinate des Zielrechtecks ( 1 Zeile tiefer )    
 das ganze mit Pixelmove (schnell) verschieben und Zeile lîschen
lîscht auf dem Bildschirm die Cursorzeile                       
 x - und y - Koordinate des zu verschiebenden Rechtecks         
 Breite setzen und Hîhe berechnen                               
 x - und y - Koordinate des Zielrechtecks ( 1 Zeile hîher )     
 das ganze mit Pixelmove verschieben und unterste Zeile lîschen 
\ *** Block No. 56 Hexblock 38 
\ Editor-Window Title and Status-Line                  30aug86we
                                                                
setzt 'work als Arbeitsspeicher und lîscht ihn; 0 als Abschluû  
                                                                
                                                                
f ist true, wenn der aktuelle Screen als updated markiert ist.  
                                                                
Åbergibt in AbhÑngigkeit vom Updatezustand den richtigen String.
                                                                
                                                                
Statuszeile wird nur beschrieben, wenn sich etwas verÑndert hat.
 Screennummer wird in 'work zusammengestellt,                   
 2 Leerzeichen und dann die Updatemeldung.                      
 das Ganze wird an .wi_state als 0-terminated String Åbergeben. 
                                                                
                                                                
\ *** Block No. 57 Hexblock 39 
\ screen display                                       30aug86we
                                                                
gibt den Filenamen in der Titelzeile aus;  'work lîschen        
 Adresse des Strings, der den Filenamen enthÑlt, ermitteln      
 und nach 'work bringen, maximal eine Zeile, Leerzeichen am Ende
 als 0-terminated String an wi_title Åbergeben.                 
                                                                
berechnet die Speicheradresse von Zeile line#,                  
 setzt Cursor und bereitet die Parameter fÅr type auf.          
                                                                
lîscht Zeile line# und gibt sie dann aus (schnell!!).           
gibt Zeile line# neu aus (langsam, aber ohne Flackern).         
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 58 Hexblock 3A 
\ screen display                                       14sep86we
                                                                
maximale LÑnge der User-ID, die automatisch in die obere rechte 
Ecke des Screens gesetzt wird, wenn dieser geÑndert wurde.      
                                                                
setzt ID rechtsbÅndig (!) in die erste Zeile.                   
setzt ID, wenn der aktuelle Screen verÑndert wurde.             
                                                                
                                                                
gibt einen Screen im Editorfenster aus. Flags fÅr ?stamp und    
 vertikaler Slider wird auf richtige Grîûe und Position gesetzt 
 .edistate werden zurÅckgesetzt.                                
                                                                
lîscht den aktuellen Buffer und erzwingt so Neueinlesen von Disk
 Der Blockzugriff ist fÅr Multitasking nîtig.                   
zeichnet den gesamten Bildschirm neu (nach Accessory-Aufruf).   
\ *** Block No. 59 Hexblock 3B 
\ Edi Variables, putchar                               17aug86we
                                                                
Adresse von PAD beim Editieren fÅr ?clearbuffer.                
Obergrenze fÅr Zeichen- (128 Zeichen)  und Zeilenbuffer, der    
 oberhalb von PAD bis zur Speichergrenze reicht                 
Adresse des Zeichenbuffers      Anzahl der Zeichen im Buffer    
liefert die nÑchste freie Adresse im Zeichenbuffer.             
                                                                
Adresse des Zeilenbuffers       Anzahl der Zeilen im Buffer     
liefert die nÑchste freie Adresse im Zeilenbuffer.              
                                                                
speichert das zuletzt eingegebene Zeichen.                      
                                                                
Insertmodus, voreingestellt aus                                 
                                                                
                                                                
\ *** Block No. 60 Hexblock 3C 
\ Edi line handling                                    17aug86we
                                                                
erneuert gerade bearbeitete Zeile auf dem Bildschirm; setzt Flag
 fÅr ?stamp.                                                    
lîscht die Cursorzeile.                                         
lîscht vom Cursor bis zum Zeilenende.                           
                                                                
lîscht Cursorzeile und zieht Rest des Bildschirms nach oben.    
                                                                
lîscht Zeile Åber dem Cursor und zieht Rest des Bildschirms nach
 oben.                                                          
fÅgt an der Cursorposition eine Leerzeile ein; Rest des Bild-   
 schirms wird nach unten geschoben.                             
                                                                
                                                                
                                                                
\ *** Block No. 61 Hexblock 3D 
\ Edi line handling                                    17aug86we
                                                                
prÅft, ob Platz im Zeilenbuffer vorhanden ist, und kopiert      
 eine Zeile in den Zeilenbuffer.                                
                                                                
kopiert eine Zeile in den Buffer, setzt Cursor auf die nÑchste. 
kopiert eine Zeile in den Buffer und lîscht sie.                
                                                                
setzt aus dem Zeilenbuffer eine Zeile in der Cursorzeile ein.   
                                                                
                                                                
benutzt !line, prÅft vorher, ob Zeilen im Buffer sind.          
 FÅr die neue Zeile wird zuerst eine Leerzeile eingefÅgt.       
                                                                
                                                                
                                                                
\ *** Block No. 62 Hexblock 3E 
\ Edi char handling                                    17aug86we
                                                                
lîscht Zeichen unter dem Cursor.                                
lîscht Zeichen links neben dem Cursor.                          
                                                                
fÅgt an der Cursorposition ein Zeichen im Buffer ein.           
benutzt inst1, um ein Leerzeichen einzufÅgen.                   
                                                                
analog zu @line, kopiert ein Zeichen in den Zeichenbuffer.      
                                                                
kopiert ein Zeichen in den Buffer, setzt Cursor auf das nÑchste.
kopiert ein Zeichen in den Buffer und lîscht es.                
                                                                
analog zu !line, setzt ein Zeichen aus dem Buffer bei Cursor ein
benutzt !char, prÅft vorher, ob Zeichen im Buffer sind.         
 FÅr das neue Zeichen wird zuerst ein Leerzeichen eingefÅgt.    
\ *** Block No. 63 Hexblock 3F 
\ from Screen to Screen ...                            24aug86we
                                                                
prÅft, ob der angeforderte Screen vorhanden ist und gibt ihn aus
geht auf den nÑchsten Screen.                                   
geht auf den vorherigen Screen.                                 
                                                                
berechnet zu Screen n1 den Shadow-Screen n2 oder umgekehrt.     
schaltet zwischen Original und Shadow hin und her.              
                                                                
markiert den aktuellen Screen mit File und Cursorposition.      
s.o., jedoch mit Meldung.                                       
                                                                
vertauscht aktuellen und markierten Screen. Dabei wird auch das 
 File mitberÅcksichtigt. Dies erlaubt es, nach VIEW einen mar-  
 kierten Screen wieder zu benutzen.                             
                                                                
\ *** Block No. 64 Hexblock 40 
\ splitting a line, replace                            17aug86we
                                                                
setzt den Rest der Zeile ab Cursor auf den Anfang einer neu     
 eingefÅgten Zeile. Dazu wird erst eine komplette leere Zeile   
 eingefÅgt und dann von Cursorspalte bis Anfang der neuen       
 Zeile gelîscht.                                                
                                                                
fÅgt den Insert-Buffer an der Cursorposition ein.               
                                                                
ersetzt den gefundenen String durch den Insert-Buffer.          
 berechnet Anzahl der Leerzeichen am Ende der Zeile.            
 Abbruch, wenn weniger als Differenz zwischen Find und Insert,  
 sonst Findstring lîschen und Insert-Buffer einfÅgen            
                                                                
                                                                
                                                                
\ *** Block No. 65 Hexblock 41 
\ find und search                                      30aug86we
                                                                
f ist 1, wenn in Richtung last Screen gesucht wird, sonst 1.    
schaltet Button in der Findbox auf Suche Richtung last screen.  
schaltet Button in der Findbox auf Suche Richtung 1st screen.   
                                                                
Der Screen, bis zu dem gesucht werden soll                      
                                                                
sucht von Cursor bis Screenende; n ist Offset zu Cursorposition.
                                                                
sucht von Cursor bis Screen fscreen vorwÑrts oder rÅckwÑrts.    
 solange bis fscreen erreicht ist oder Esc oder CTRL-C gedrÅckt,
 wird der nÑchste Screen aufgerufen.                            
 Abbruch, falls nicht gefunden und Umschalten der Suchrichtung  
 in der Box und in fscreen.                                     
 Screen auflisten und Abbruchmeldung ausgeben.                  
\ *** Block No. 66 Hexblock 42 
\ Search-Findbox auswerten                             17aug86we
                                                                
Vorbelegung der Buttons und Screennummern in der Find-box:      
 Groû-Kleinschreibung unterscheiden.                            
 Aufsteigend suchen bis Fileende.                               
 1 fÅr 1st Screen, letzten Screen im File als Last Screen       
                                                                
Filebox auswerten:                                              
 Variable caps entsprechend setzen.                             
 Suchrichtung bestimmt, ob der erste oder letzte Screen         
 als Endscreen benutzt wird.                                    
 Strings in die entsprechenden Buffer Åbernehmen.               
                                                                
Falls das File gewechselt wurde, neu initialisieren, geschieht  
 auch automatisch, wenn sich PAD und damit Find- und Insert-    
 buffer verÑndert haben.                                        
\ *** Block No. 67 Hexblock 43 
\ Replacing ...                                        17aug86we
                                                                
Flag fÅr Ersetzen des Find-Strings durch den Insert-String      
                                                                
O Schreck und Graus !!!                                         
 Die Replace-Box soll natÅrlich nicht den gefundenen String     
 verdecken; die von form_center gelieferten Werte sind also     
 unbrauchbar. X- und Y-Position mÅssen von Hand berechnet werden
 und zwar so, daû die linke obere Ecke der Box auf den Such-    
 string zeigt;  zeichnen des Objects wie in show_object.        
                                                                
ersetzt solange den Suchstring durch den Insertstring, bis      
 CANCEL gedrÅckt oder der Suchstring nicht gefunden wird.       
 Abbruch auch, wenn der Insertstring sich nicht einsetzen lÑût. 
 Sonst wie bei Find Abbruch mit Esc. oder CTRL-C mîglich.       
                                                                
\ *** Block No. 68 Hexblock 44 
\ Editor's find and replace                            17aug86we
                                                                
Flag fÅr repfind, ob bereits eine Suche stattgefunden hat.      
                                                                
fÅhrt erneute Suche (und Ersetzen) durch ohne Find-Box.         
 Abbruch, wenn noch kein Aufruf der Find-Box oder Findbuffer    
 leer; sonst sicherstellen, daû fscreen innerhalb des Files     
 liegt und s bzw replace ausfÅhren.                             
                                                                
Das ist das aufrufende Wort; im CANCEL-Fall abbrechen,          
 sonst Flag fÅr replace setzen, wenn :dfreplac gewÑhlt wurde    
 Im Menubalken Repeatfind enable'n                              
 Screennummer merken; suchen und ggf. ersetzen mit repfind.     
                                                                
                                                                
                                                                
\ *** Block No. 69 Hexblock 45 
\ exiting the Editor                                   30aug86we
                                                                
Setzt Mausvector zurÅck, wird erst spÑter definiert.            
                                                                
gemeinsame Routine fÅr alle Exits                               
lîscht (und restauriert) das Fenster, setzt Mausvector zurÅck   
 gibt an der alten Cursorpositione eine Meldung aus             
 und setzt  Flag zum Verlassen von ediloop.                     
                                                                
wirft alle énderungen weg, falls man sich 'vereditiert' hat.    
speichert den Screen auf Disk, falls er verÑndert wurde.        
markiert den Screen, ohne ihn direkt zurÅckzuschreiben.         
speichert den Screen auf Disk, falls er verÑndert wurde         
 und compiliert ab Cursorposition.                              
                                                                
                                                                
\ *** Block No. 70 Hexblock 46 
\ get User's ID, jump to screen                        17aug86we
                                                                
User-ID holen                                                   
 bisherige ID im Fenster ausgeben                               
 das Åbliche form-handling                                      
 bei Cancel nichts wie raus!                                    
 bei NO-ID wird sie gelîscht; die Box erscheint dann bei nÑch-  
 ster Gelegenheit wieder; sonst ID Åbernehmen (auch Leerstring) 
                                                                
User-ID nur holen, wenn noch keine vorhanden ist.               
 Wird beim Eintritt in den Editor benutzt.                      
                                                                
springt auf beliebigen Screen im File.                          
 Leerstring in die Box setzen.                                  
 das Åbliche form-handling                                      
 Screen-Nr. fÅr setscreen Åbernehmen und Screen ausgeben        
\ *** Block No. 71 Hexblock 47 
\ insert- and overwrite-mode                           11aug86we
                                                                
setzt im Pulldownmenu ein HÑkchen.                              
wie oben, nur umgekehrt.                                        
                                                                
Insert-Modus setzen und Pulldownmenu entsprechend Ñndern.       
                                                                
Overwrite-Modus setzen und Pulldownmenu entsprechend Ñndern.    
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 72 Hexblock 48 
\ viewing words                                        17aug86we
                                                                
Hilfswort fÅr do_view                                           
 Findbuffer wird nach PAD gebracht und fÅr find aufbereitet.    
 sucht CFA des Wortes im Findbuffer, um                         
 das zugehîrige Name- und damit das View-Feld zu finden.        
 setzt File und Screen-Nr. und sucht auf dem Screen nach dem    
 Wort; falls gefunden, wird der Cursor dahinter positioniert.   
                                                                
                                                                
lîscht den String in der Box;  das Åbliche form-handling        
 String in Findbuffer Åbernehmen, falls nicht Cancel gewÑhlt;   
 aktuellen Screen markieren, wenn MARK                          
 angeklickt wurde, und gesuchten Screen aufrufen                
 Danach kann mit CTRL-A wieder auf den anderen Screen gewechselt
 werden. Sehr nÅtzlich, um Zeilen aus anderen Files zu 'klauen'.
\ *** Block No. 73 Hexblock 49 
\ Table of keystrokes                                  17aug86we
                                                                
Diese Tabelle enthÑlt alle Tasten, die irgendwelche Sonder-     
 funktionen haben. Das jeweils erste Wort ist der Scancode der  
 Taste, das zweite die zusÑtzlich gedrÅckten Tasten:            
                1 = linke oder rechte SHIFT-Taste               
                2 = CONTROL-Taste                               
                4 = ALTERNATE-Taste ( wird nicht benutzt )      
 Auf die Funktionstasten wurde bewuût verzichtet, weil man damit
 nicht vernÅnftig umgehen kann.                                 
                                                                
                                                                
Zusatzvorschlag:                                                
 Alternate-Shift-Control bei gleichzeitig gedrÅckter Enter- und 
 F10-Taste ---> lîscht den Bildschirm.                          
                                                                
\ *** Block No. 74 Hexblock 4A 
\ Table of actions                                     17aug86we
                                                                
Tabelle aller Editorfunktionen                                  
 Die Position eines Tabelleneintrags stimmt mit der des         
 zugehîrigen Tastendrucks Åberein, um die öbersicht zu behalten.
 Dies gilt auch fÅr die folgenden Screens.                      
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
prÅft, ob Anzahl der Funktionen mit Anzahl der Tasten Åberein-  
 stimmt. Wird nur wÑhrend der Compilation gebraucht.            
\ *** Block No. 75 Hexblock 4B 
\ Table of Menuevents                                  17aug86we
                                                                
Tabelle der MenueintrÑge.                                       
 Alle Editorfunktionen sind sowohl Åber die MenÅleiste als auch 
 Åber Tastendruck zu erreichen.                                 
 Bei allen Worten mit : am Anfang handelt es sich um 'kopflose' 
 Konstanten aus dem Resource-Definitionen-File (EDIICON.SCR),   
 das mit dem Programm CONVH.SCR aus EDIICON.H erzeugt wurde.    
 EDIICON.H wird vom 'Resource Construction Set' ausgegeben.     
 An dieser Stelle unser herzlicher Dank an Digital Research fÅr 
 dieses hervorragende Produkt. Nur ca. 80 SystemabstÅrze gab es 
 bei der Entwicklung, weil Icons bisweilen auf ungeraden Spei-  
 cheradressen abgelegt werden. Auûerdem war bei knapp 10 kByte  
 LÑnge der Resource mein Speicher (1024 kByte!!!) grundsÑtzlich 
 voll bis absturzvoll. Dann bleibt das Programm stehen, nicht   
 ohne vorher die letzte lauffÑhige Resource zu lîschen....      
\ *** Block No. 76 Hexblock 4C 
\ Table of Help-Boxes                                  17aug86we
                                                                
Tabelle der Help-Boxen.                                         
 Zu jeder Editorfunktion gibt es eine Box, die die Funktion     
 beschreibt. WÑhlt man Dauerhilfe, erscheinen solche Boxen      
 immer, wenn ein Befehl aus der Menuleiste abgerufen wird.      
 Soll beim Einarbeiten in den Editor Hilfe leisten. Die Idee    
 dazu stammt aus 1st Word.                                      
 Gibt es zu einer Funktion keine Box (z.B. Cursortasten), ist   
 der entsprechende Eintrag mit $FF gekennzeichnet.              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 77 Hexblock 4D 
\ Prepare multi-event                                  24aug86we
                                                                
Flag, ob Maus innerhalb oder auûerhalb von Rechteck1            
                                                                
FÅr den Multi-Event mÅssen 17 (!) Parameter Åbergeben werden.   
 timer, message, mouse, button + keyboard events zulassen.      
 1 Tastendruck auf linke Maustaste, event bei gedrÅckter Taste  
 1, wenn Maus im Fensterbereich                                 
 Rechteck 1 (énderung der Mausfunktion) umfaût Editor-Fenster   
 Rechteck 2 gibts nicht                                         
 Timer auf 0 Millisekunden (sonst kommt der Multi-Event nicht   
 zurÅck)                                                        
                                                                
Wenn nichts anderes zu tun ist, kann eine andere Task ran.      
schaltet Flag um.                                               
                                                                
\ *** Block No. 78 Hexblock 4E 
\ Button Event                                         17aug86we
                                                                
Flag, das anzeigt, ob der Cursor sichtbar ist (1 = sichtbar)    
                                                                
schaltet Cursor ein, wenn er noch nicht eingeschaltet ist;      
 die Funktion arbeitet im EXOR-Modus, daher dieser Aufwand.     
 baut an der aktuellen Cursorposition ein schwarzes Rechteck    
 in der Grîûe eines Zeichens.                                   
                                                                
kann curon benutzen wegen EXOR-Modus, muû aber das Flag setzen. 
                                                                
Mausknopfereignis dann, wenn die Maus im Editorfenster steht.   
 die Position der Maus (in Pixel) wird in Zeile und Spalte umge-
 rechnet und nach r# gespeichert. Maus abschalten und alten     
 Cursor lîschen (in dieser Reihenfolge!)                        
                                                                
\ *** Block No. 79 Hexblock 4F 
\ Key event                                            17aug86we
                                                                
Steuertasten erzeugen keinen ASCII-Code, sondern eine Null.     
                                                                
gibt ein Zeichen auf dem Bildschirm aus und schreibt es in den  
 Blockbuffer. Abbruch, wenn kein druckbares Zeichen vorliegt.   
 Auf Insert-Modus prÅfen und Zeichen ausgeben.                  
                                                                
ermittelt die Adresse der zu einer Taste gehîrenden Funktion.   
 d_key enthÑlt im oberen Wort den Status von Shift, Control usw.
 putchar ist voreingestellt,  keytable wird auf d_key abgesucht 
 wenn gefunden, wird die Adresse von putchar entfernt und statt-
 dessen die zugehîrige Adresse aus actiontable hinterlegt.      
                                                                
                                                                
                                                                
\ *** Block No. 80 Hexblock 50 
\ Key event                                            17aug86we
                                                                
Flag fÅr Fehlerpiep                                             
Flag, ob die Maus sichtbar ist                                  
                                                                
positioniert den Cursor auf die Position in r#.                 
                                                                
Tasten-Event    schaltet Mausflag ab                            
 Tastencode holen und Maus und Cursor abschalten.               
 Status der Sondertasten aufbereiten und Tastenfunktion ausfÅh- 
 ren, Fehlerpiep ermîglichen, Status ausgeben                   
 und - darauf bin ich ganz stolz - alle weiteren TastendrÅcke   
 lîschen!! Dadurch lÑuft auch bei schnellem Tastenrepeat keine  
 Funktion 'nach', wird aber trotzdem schnellstmîlich ausgefÅhrt.
 Funktioniert allerdings dann nicht, wenn das lahme GEM was zu  
 tun hat, also beim Screenwechsel (CTRL-B und CTRL-N)           
\ *** Block No. 81 Hexblock 51 
\ Message events for window                            30aug86we
                                                                
holt Wort n aus dem AES-message Buffer.                         
                                                                
bei Anklicken des Sliders oder der Pfeile                       
 wird der nÑchste oder vorherige Screen aufgerufen.             
                                                                
beim Verschieben des Sliders                                    
 wird aus der Position die Screennummer berechnet.              
                                                                
beim Verschieben des ganzen Fensters                            
 wird die vom User gewÅnschte Position berechnet                
 und in ganze Zeile bzw. Spalten umgewandelt; auûerhalb des     
 Screens kann nicht positioniert werden, sonst kînnte man       
 ohne Sichtkontrolle weiter editieren. öber den Sinn dieser     
 Funktion kann man streiten, aber ich wollte zeigen, daû es geht
\ *** Block No. 82 Hexblock 52 
\ Message events (the menuline)                        17aug86we
                                                                
Flag fÅr Dauerhilfe bei jeder MenÅfunktion                      
                                                                
Hilfsbox Nr. n ausgeben                                         
 passende Hilfsbox aus Tabelle suchen und anzeigen, bei OK Ende.
 sonst Funktion abbrechen.                                      
Es folgen die Funktionen, die nicht in der helptable auftauchen.
Info-, Werbe- und Prunk-Box                                     
 braucht nur angezeigt zu werden, spricht fÅr sich selbst.      
                                                                
Dauerhilfe-Box anzeigen; je nach gewÑhltem Knopf                
 HÑkchen bei Menu Help setzen oder lîschen                      
 dito fÅr Flag                                                  
                                                                
                                                                
\ *** Block No. 83 Hexblock 53 
\ Message events from menuline                         24aug86we
                                                                
Funktion, die nicht in actiontable steht, ausfÅhren             
 mit case? die passende Funktion auswÑhlen                      
 Tabelle lohnt hier nicht.                                      
                                                                
                                                                
                                                                
MenÅauswahl verarbeiten                                         
 MenÅtitel von revers auf normal schalten                       
 voreingestellt ist do_other, Nummer des angeklickten Items     
 holen, menutable wird auf Item-Nr. abgesucht                   
 wenn gefunden, wird die Adresse von do_other entfernt und      
 stattdessen die zugehîrige Adresse aus actiontable hinterlegt. 
 Funktion ausfÅhren, Fehlerpiep ermîglichen und Status ausgeben.
                                                                
\ *** Block No. 84 Hexblock 54 
\ Handle message-event                                 24aug86we
                                                                
hier werden die Messages ausgewertet, die AES zurÅckgibt.       
 wenn ein MenÅpunkt angeklickt wird, menu-message ausfÅhren.    
 alle anderen Messages betreffen die Window-Attribute und       
 werden entsprechend ausgefÅhrt.                                
                                                                
 Wenn ein Desk-Accessory ausgefÅhrt wurde, erhÑlt man lediglich 
 die Meldung, daû neu gezeichnet werden muû, und dies auch nur  
 dann, wenn ein Fenster aktiv ist.                              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 85 Hexblock 55 
\ Handle all events                                    24aug86we
                                                                
Tabelle der mîglichen Events (werden als gesetztes Bit gemeldet)
 in der Reihenfolge ihrer PrioritÑt, sonst kommt z.B. der Timer 
 immer                                                          
                                                                
und der zugehîrigen Funktionen                                  
                                                                
                                                                
Das ist der Event-Handler                                       
 gemeldeter Event wird mit Liste verglichen (PrioritÑt !!)      
 und die entsprechende Event-Aktion ausgefÅhrt.                 
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 86 Hexblock 56 
\ Change mouse-movement Vector                         17aug86we
                                                                
Variable, um den alten Mausvektor zu speichern.                 
                                                                
Die neue Mausroutine soll zusÑtzlich das Flag ?mouse setzen,    
 wenn die Maus bewegt wurde. So wird die Maus bei jedem Tasten- 
 druck ausgeschaltet und erst wieder eingeschaltet bei Bewegung.
 Schick, gell?!                                                 
Aus GeschwindigkeitsgrÅnden in Assembler                        
                                                                
Ñndert den Mausvektor.                                          
                                                                
Mausvektor auf neuen Wert, alter Wert nach savevec.             
Mausvektor auf alten Wert (muû unbedingt ausgefÅhrt werden, das 
 Betriebssystem erledigt das beim Verlassen von FORTH nicht !!  
resetmousevec lîst das deffered word in done auf.               
\ *** Block No. 87 Hexblock 57 
\ The Editor's LOOP                                    30aug86we
                                                                
ediloop rÑumt den Returnstack auf, falls mit abort" abgebrochen.
 Das ist die Endlos-Schleife, die erst verlassen wird, wenn     
 das Flag fÅr UNTIL durch done gesetzt wird.                    
                                                                
Fehlerpiep, nur einmal ausfÅhren, sonst klingelts dauernd.      
                                                                
Errorhandler fÅr Editor                                         
 falls Fehlermeldung bereits erfolgt, sofort nach ediloop       
 piepen, 'work vorbereiten                                      
 in der Statuszeile rechts Fehlertext ausgeben, soweit Platz ist
 und RÅcksprung in ediloop ;                                    
                                                                
                                                                
                                                                
\ *** Block No. 88 Hexblock 58 
\ Installing the Editor                                26oct86we
                                                                
Alle Routinen in der GEM-Library sind so geschrieben, daû sie   
 implizit auf eine Variable grhandle zurÅckgreifen. Dies        
 vereinfacht die ParameterÅbergabe erheblich.                   
 Sollen verschiedene Grafik-Applikationen aktiviert werden, darf
 trotzdem nur eine Appliktion angemeldet werden. Dies geschieht 
 bereits beim Laden des FORTH-Systems.                          
Beim Laden eines Resource-Files mit rsrc_load wird die Adresse  
 der zugehîrigen Baumstruktur im Global-Array unter ap_ptree    
 abgelegt. Diese Adresse kann man zum Umschalten auf verschie-  
 dene Resources benutzen.                                       
Wenn PAD sich verÑndert hat (durch neue Worte oder forget)      
 sind Find- und Insert-Buffer verschoben und mÅssen neu initia- 
 lisiert werden. Ebenso Zeichen und Zeilenbuffer.               
 (findbox wird gelîscht, damit die Findbox initialisiert wird.  
\ *** Block No. 89 Hexblock 59 
\ Installing the Editor                                26oct86we
                                                                
initialisiert den Editor beim Aufruf.                           
 Abbruch, wenn kein Platz fÅr die Editor-Buffer ist (s.u...)    
 aktuelle Cursorposition merken, Mausvector initialisieren      
 Buffer bei Bedarf initialisieren                               
 Editor-Resource und Grafik-Handle installieren.                
 Fenster îffnen und MenÅzeile ausgeben                          
 Errorhandler auf Editor umschalten, alten merken.              
                                                                
                                                                
...das Dictionary ist zu voll. Entweder man 'vergiût' einige    
 Worte oder schafft mit z.B.  'save 4 buffers'  mehr Raum. Mit  
 BUFFERS lÑût sich die Anzahl der Diskbuffer festlegen. Dabei   
 steht mehr Platz im Dictionary gegen Arbeitskomfort beim Edi-  
 tieren. Beachten Sie auch, daû BUFFERS ein COLD ausfÅhrt.      
\ *** Block No. 90 Hexblock 5A 
\ Entering the Editor                                  17aug86we
                                                                
Es folgen die Forth-Worte zum Aufruf des Editors.               
                                                                
Flag entscheidet, ob compiliert werden soll (ldone).            
                                                                
Screen mit Nummer in scr und Cursor in r# wird aufgerufen.      
 Diese Systemvariablen werden auch bei Fehlern gesetzt, also    
 kann man bei einem Compilationsfehler auf den richtigen Screen 
 gelangen; Cursor steht dann hinter dem Wort, das den Fehler    
 ausgelîst hat.                                                 
l editiert Screen-Nr. n                                         
view erwartet ein Wort und editiert den Screen, auf dem das     
 Wort definiert wurde (s.a. >view)                              
                                                                
                                                                
\ *** Block No. 91 Hexblock 5B 
\ savesystem for Editor                                17aug86we
                                                                
Damit der Editor auf Schwarz-Weiû und Farbmonitoren lÑuft,      
 mÅssen die entsprechenden Parameter ermittelt und in die       
 beiden Arrays, die fÅr die Zwischenspeicherung des Bildschirms 
 verantwortlich sind, gepatched werden.                         
 FÅr die Zwischenspeicherung werden 2 Buffer benutzt, die ober- 
 halb des Systems liegen. Nur dadurch kann der Bildschirminhalt 
 so schnell restauriert werden, wenn Alertboxen oder andere     
 aufgerufen wurden.                                             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 92 Hexblock 5C 
\ savesystem for Editor                                30aug86we
                                                                
Diese Routine muû beim Start des Systems (!) ausgefÅhrt werden, 
 setzt die Variablen fÅr die GEM-Routinen des Editors           
 und fÅr die beiden Speicherdefinitions-Arrays                  
 wird daher nach  drvinit  gepatched, klinkt sich selbst aus.   
                                                                
savesystem muû eine Reihe von Variablen zurÅcksetzen, damit     
 das System mit 'vernÅnftigen' Werten hochkommt.                
 drvinit wird mit edistart gepatched.                           
 FORTH-83.SCR als File fÅr markierten Screen.                   
 geÑnderte Blîcke auf Diskette zurÅckschreiben                  
 und altes savesystem ausfÅhren.                                
Neues bye muû zusÑtzlich ein  GREXIT  ausfÅhren.  GRINIT  bei   
 Neukompilation nîtig wegen  GREXIT in  BYE .                   
                                                                
\ *** Block No. 93 Hexblock 5D 
\ savesystem for Editor                                17aug86we
                                                                
Damit der Editor auf Schwarz-Weiû und Farbmonitoren lÑuft,      
 mÅssen die entsprechenden Parameter ermittelt und in die       
 beiden Arrays, die fÅr die Zwischenspeicherung des Bildschirms 
 verantwortlich sind, gepatched werden.                         
 FÅr die Zwischenspeicherung werden 2 Buffer benutzt, die ober- 
 halb des Systems liegen. Nur dadurch kann der Bildschirminhalt 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
