\ *** Block No. 0 Hexblock 0 
\\ Printer Interface                                     08Nov86
                                                                
Dieses File enthaelt das Printer Interface zwischen volksFORTH  
und dem Drucker.                                                
                                                                
Damit ist es moeglich Source-Texte auf bequeme Art und Weise    
in uebersichtlicher Form auszudrucken (6 auf eine Seite).       
                                                                
In Verbindung mit dem Multitasker ist es moeglich, auch Texte im
Hintergrund drucken zu lassen und trotztdem weiterzuarbeiten.   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ Printer Interface Epson RX80                           18Aug86
\ angepasst auf M 130i                                 07dec85we
                                                                
Onlyforth                                                       
                                                                
Variable shadow         capacity 2/ shadow !   \ s. Editor      
                                                                
Vocabulary Printer   Printer definitions also                   
| Variable printsem   printsem off                              
                                                                
     01 +load  04 0C +thru        \ M 130i - Printer            
\ 01 03 +thru  06 0C +thru        \ Fujitsu - Printer           
                                                                
Onlyforth                                                       
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ Printer  p! and controls                            UH 02Nov87
                                                                
| : ready? ( -- f ) [ Dos ]  0 &15 biosa 0= not ;               
                                                                
: p!  ( n --)   BEGIN  pause                                    
   stop? IF printsem unlock true abort" stopped! " THEN         
   ready?  UNTIL  [ Dos ]  5 bios ;                             
                                                                
| : ctrl:  ( 8b --)   Create c,   Does>  ( --)   c@ p! ;        
                                                                
07   ctrl: BEL       7F | ctrl: DEL        0D | ctrl: RET       
1B | ctrl: ESC       0A   ctrl: LF         0C   ctrl: FF        
0F | ctrl: (+17cpi   12 | ctrl: (-17cpi                         
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\ Printer Escapes                                        24dec85
                                                                
| : esc:  ( 8b --)   Create c,   does>  ( --)   ESC c@ p! ;     
                                                                
Ascii 0   esc: 1/8"       Ascii 1   esc: 1/10"                  
Ascii 2   esc: 1/6"       Ascii T   esc: suoff                  
Ascii N   esc: +jump      Ascii O   esc: -jump                  
Ascii G   esc: +dark      Ascii H   esc: -dark                  
\ Ascii 4   esc: +cursive   Ascii 5   esc: -cursive             
                                                                
                                                                
| : ESC2  ( 8b0 8b1 --)   ESC p! p! ;                           
                                                                
| : on:  ( 8b --)  Create c,  does>  ( --)  ESC c@ p!  1 p! ;   
| : off: ( 8b --)  Create c,  does>  ( --)  ESC c@ p!  0 p! ;   
                                                                
\ *** Block No. 4 Hexblock 4 
\ Printer Escapes                                        29jan86
                                                                
Ascii W   on:  +wide    Ascii W   off: -wide                    
Ascii -   on:  +under   Ascii -   off: -under                   
Ascii S   on:  sub      Ascii S   off: super                    
Ascii P   on:  (10cpi   Ascii P   off: (12cpi                   
                                                                
: 10cpi     (-17cpi  (10cpi  ;                                  
: 12cpi     (-17cpi  (12cpi  ;                                  
: 17cpi     (10cpi   (+17cpi ;                                  
                                                                
: lines  ( #.of.lines --)   Ascii C ESC2 ;                      
: "long  ( inches --)   0 lines p! ;                            
: american   0 Ascii R ESC2 ;                                   
: german   2 Ascii R ESC2 ;                                     
: normal   12cpi  american  suoff 1/6" 0C "long RET ;           
\ *** Block No. 5 Hexblock 5 
\ Printer Escapes                                        16Jul86
                                                                
| : esc:  ( 8b --)   Create c,   does>  ( --)   ESC c@ p! ;     
                                                                
Ascii 0   esc: 1/8"       Ascii 1   esc: 1/10"                  
Ascii 2   esc: 1/6"       Ascii T   esc: suoff                  
Ascii N   esc: +jump      Ascii O   esc: -jump                  
Ascii G   esc: +dark      Ascii H   esc: -dark                  
Ascii 4   esc: +cursive   Ascii 5   esc: -cursive               
Ascii M   esc: 12cpi      Ascii P | esc: (-12cpi                
                                                                
: 10cpi   (-12cpi (-17cpi ;                                     
: 17cpi   (-12cpi (+17cpi ;                                     
                                                                
' 10cpi   Alias pica      ' 12cpi   Alias elite                 
                                                                
\ *** Block No. 6 Hexblock 6 
\ Printer Escapes                                        16Jul86
                                                                
| : ESC2  ( 8b0 8b1 --)   ESC p! p! ;                           
                                                                
| : on:  ( 8b --)  Create c,  does>  ( --)  ESC c@ p!  1 p! ;   
| : off: ( 8b --)  Create c,  does>  ( --)  ESC c@ p!  0 p! ;   
                                                                
Ascii W   on:  +wide    Ascii W   off: -wide                    
Ascii -   on:  +under   Ascii -   off: -under                   
Ascii S   on:  sub      Ascii S   off: super                    
Ascii p   on:  +prop    Ascii p   off: -prop                    
: lines  ( #.of.lines --)   Ascii C ESC2 ;                      
: "long  ( inches --)   0 lines p! ;                            
: american   0 Ascii R ESC2 ;                                   
: german     2 Ascii R ESC2 ;                                   
: normal   12cpi  american  suoff 1/6" 0C "long RET ;           
\ *** Block No. 7 Hexblock 7 
\ Printer Output                                         04Jul86
                                                                
: prinit ;  \ initializing Printer                              
                                                                
| Variable pcol   pcol off      | Variable prow   prow off      
| : pemit  ( 8b --)   p!  1 pcol +! ;                           
| : pcr  ( --)   RET LF  1 prow +!  pcol off ;                  
| : pdel  ( --)   DEL  pcol @ 1- 0 max pcol ! ;                 
| : ppage  ( --)   FF  prow off  pcol off ;                     
| : pat  ( row col --)   over  prow @ <  IF  ppage  THEN        
   swap  prow @ -  0 ?DO  pcr  LOOP                             
   dup  pcol @ <  IF  RET  pcol off  THEN  pcol @ - spaces ;    
| : pat?  ( -- row col)   prow @  pcol @ ;                      
| : ptype  ( adr len --)                                        
 dup pcol +!  bounds ?DO  I c@ p!  LOOP ;                       
                                                                
\ *** Block No. 8 Hexblock 8 
\ Printer output                                         28Jun86
                                                                
| Output: >printer   pemit pcr ptype pdel ppage pat pat? ;      
                                                                
Forth definitions                                               
                                                                
: print   >printer  normal ;                                    
                                                                
: printable?  ( char -- f)   bl  Ascii ~  uwithin ;             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
\ Variables and Setup                                    23Oct86
                                                                
Printer definitions                                             
                                                                
$00 | Constant logo   | Variable pageno                         
| Create scr#s   $0E allot  \ enough room for 6 screens         
                                                                
| : header  ( -- )                                              
 12cpi  4 spaces   ." Page No " +dark pageno @ 2 .r             
 $0D spaces  ."  volksFORTH83 der FORTH-Gesellschaft eV "       
 5 spaces  file?  -dark 1 pageno +! 17cpi ;                     
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
\ Print 2 screens across on a page                       03dec85
                                                                
| : text?  ( scr# -- f)   block  dup c@  printable?             
  IF  b/blk -trailing  nip  0=  THEN  0=  ;                     
                                                                
| : pr  ( scr# --)    dup  capacity 1- u>  IF  drop logo  THEN  
 1 scr#s +!  scr#s dup @ 2* + ! ;                               
                                                                
| : 2pr  ( scr#1 scr#2 line# --)  cr  dup 2 .r space  c/l * >r  
 pad $101 bl fill  swap block  r@ +  pad c/l cmove              
 block r> + pad c/l + 1+ c/l cmove  pad $101 -trailing type ;   
                                                                
| : 2scr  ( scr#1 scr#2 --)    cr cr  $1E spaces                
 +wide +dark over 4 .r  $1C spaces  dup 4 .r  -wide -dark       
 cr  l/s 0 DO  2dup  I 2pr  LOOP  2drop ;                       
                                                                
\ *** Block No. 11 Hexblock B 
\ Printer 6 screens on a page                            03dec85
                                                                
| : pr-start  ( --)   scr#s off  1 pageno ! ;                   
                                                                
| : pagepr  ( --)   header  scr#s off  scr#s 2+                 
 3 0 DO  dup @  over 6 + @  2scr  2+  LOOP  drop  page ;        
                                                                
| : shadowpr  ( --)   header  scr#s off  scr#s 2+               
 3 0 DO  dup @  over 2+ @  2scr  4 +  LOOP  drop  page ;        
                                                                
| : pr-flush  ( -- f)   scr#s @  dup   \ any screens left over? 
 IF  BEGIN  scr#s @ 5 <  WHILE  -1 pr  REPEAT  logo pr  THEN    
 0<> ;                                                          
                                                                
                                                                
                                                                
\ *** Block No. 12 Hexblock C 
\ Printer 6 screens on a page                            23Nov86
Forth definitions                                               
                                                                
: pthru  ( first last --)                                       
 printsem lock  output push  print  pr-start  1+ swap           
 ?DO  I text?  IF  I pr  THEN  scr#s @ 6 = IF  pagepr  THEN     
 LOOP  pr-flush IF  pagepr  THEN  printsem unlock ;             
                                                                
: document  ( first last --)                                    
   isfile@ IF capacity 2/ shadow ! THEN                         
   printsem lock  output push  print  pr-start  1+ swap         
   ?DO  I text?  IF  I pr  I shadow @ + pr  THEN                
        scr#s @ 6 = IF  shadowpr  THEN  LOOP                    
   pr-flush  IF  shadowpr  THEN  printsem unlock ;              
                                                                
: listing  ( --)  0  capacity 2/ 1- document ;                  
\ *** Block No. 13 Hexblock D 
\ Printerspool                                           03Nov86
                                                                
\needs Task \\                                                  
                                                                
| Input: noinput    0  false  drop  2drop  ;                    
                                                                
                                                                
$100 $200  noinput Task spooler                                 
                                                                
keyboard                                                        
                                                                
: spool   ( from to -- )                                        
   isfile@  spooler 3 pass  isfile !  pthru  stop ;             
                                                                
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 15 Hexblock F 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
