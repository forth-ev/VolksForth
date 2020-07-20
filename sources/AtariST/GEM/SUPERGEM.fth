\ *** Block No. 0 Hexblock 0 
\\                     *** SUPERGEM.SCR ***            16sep86we
                                                                
In diesem File soll eine GEM-Library aufgebaut werden, die      
komfortablere Routinen als die Standardbefehle mit Ihren un-    
Åbersehbaren Parametern zur VerfÅgung stellt.                   
                                                                
Bei der Entwicklung des Editors sind bereits einige solche      
Routinen entstanden.                                            
                                                                
FÅr Anregungen gerade in diesem Bereich sind wir dankbar....    
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ GEM-Library Loadscreen                             cas20130105
                                                                
Onlyforth  GEM also                                             
                                                                
\needs scr>mem  $10 loadfrom gem\vdi.fb                         
                                                                
Onlyforth  GEM also definitions                                 
                                                                
1 4 +thru                                                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ Resource Trees and objects                           02sep86we
                                                                
: tree!   ( tree -- )                                           
   0 swap  rsrc_gaddr  objc_tree 2! ;                           
                                                                
: objc_gaddr   ( object# -- laddr )                             
   &24 *  extend  objc_tree 2@  d+  ;                           
                                                                
: text_gaddr   ( object# -- laddr )                             
   objc_gaddr   &12 extend  d+  l2@   l2@ ;                     
                                                                
: alert   ( n -- button )                                       
   show_c                                                       
   5 swap rsrc_gaddr addrin 2!  1 intin !  &52 1 1 1 AES        
   hide_c ;                                                     
                                                                
\ *** Block No. 3 Hexblock 3 
\ Move text to Objects and back                        02nov86we
                                                                
: putstring   ( addr object# -- )      >r                       
   count  under >r   >absaddr   r>  r@ text_gaddr  rot  lcmove  
   0 swap  extend  r> text_gaddr  d+ lc! ;                      
                                                                
: getstring   ( object# addr -- )      >r  text_gaddr           
   0  BEGIN  >r 2dup r@ extend d+ lc@  WHILE  r> 1+  REPEAT  r> 
   r>  2dup c!  1+ >absaddr  rot  lcmove ;                      
                                                                
: getnumber   ( object# -- d )                                  
   pad getstring   pad count bl skip  swap 1-   dup >r  c!      
   r@ capitalize  c@  IF  r> number  ELSE  rdrop 0 0  THEN ;    
                                                                
: putnumber   ( d object# -- )     >r                           
   <# #s #>  over 1- c!  1-   r> putstring ;                    
\ *** Block No. 4 Hexblock 4 
\ init_object select deselect                          02nov86we
                                                                
Create little  &320 , &200 , &10 , &10 ,                        
Create big     8 allot                                          
                                                                
: init_object   ( -- )                                          
    &320 &200 &10 &10 little 4!  form_center big 4! ;           
                                                                
: state_gaddr  ( object -- laddr )    objc_gaddr  &10. d+ ;     
                                                                
: select       ( object -- )          1 swap state_gaddr l! ;   
: deselect     ( object -- )          0 swap state_gaddr l! ;   
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
\ show_object hide_object objc_setpos objc_getwh       12aug86we
                                                                
: show_object   ( -- )   init_object                            
   big 4@  scr>mem1   1  little 4@  big 4@  form_dial           
   0 ( install) 3 ( depth)  big 4@ objc_draw show_c ;           
                                                                
: hide_object   ( -- )   hide_c                                 
   2  little 4@  big 4@  form_dial   big 4@  mem1>scr ;         
                                                                
: objc_setpos   ( x y object# -- )                              
   dup >r  objc_gaddr  $0.12 d+ l!   r> objc_gaddr $0.10 d+ l! ;
                                                                
: objc_getwh    ( object# -- width height )                     
   dup  objc_gaddr  $0.14 d+ l@  swap objc_gaddr $0.16 d+ l@ ;  
                                                                
                                                                
\ *** Block No. 6 Hexblock 6 
\                                                               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
\                                                               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
\ GEM-Library Loadscreen                               16sep86we
                                                                
nimmt GEM in die Suchordnung auf (Fehlermeldung, falls nicht    
 vorhanden)                                                     
wird fÅr die Rasteroperationen gebraucht, die den Bildschirm-   
 inhalt schnell restaurieren.                                   
Alle folgenden Definitionen werden Bestandteil des Vokabulars   
 GEM                                                            
falls die Mausroutinen noch nicht vorhanden sind.               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
\ Resource Trees and objects                           16sep86we
                                                                
speichert die Kennummer eines Trees in der FORTH-internen       
 Variablen objc_tree ab. Muû immer vor der weiteren Arbeit mit  
 Objekten geschehen.                                            
liefert die 32-Bit-Adresse des Objekts mit der Nummer object#.  
 tree! muû vorher aufgerufen worden sein.                       
                                                                
laddr ist die 32-Bit-Adresse des 0-terminated Strings mit der   
 Objektnummer object#.                                          
                                                                
n ist die Objektnummer der Alertbox, button ist der vom Benutzer
 betÑtigte Knopf. Die Maus wird vorher eingeschaltet und hinter-
 her glîscht.                                                   
                                                                
                                                                
\ *** Block No. 11 Hexblock B 
\ Move text to Objects and back                        16sep86we
                                                                
addr ist die Adresse eines 0-terminated Strings innerhalb des   
 FORTH-Systems. Dieser wird in das Objekt object# transportiert.
                                                                
                                                                
Der Text im Objekt object# wird nach addr transportiert.        
                                                                
                                                                
                                                                
wie oben, jedoch wir der String in eine doppelt genaue Zahl     
 gewandelt. Ist der String leer wird 0.0 zurÅckgegeben. Ein     
 Abbruch erfolgt, wenn der String nicht gewandelt werden kann.  
                                                                
wandelt die doppelt genaue Zahl d in einen 0-terminated String  
 und transportiert ihn in das Objekt object#.                   
\ *** Block No. 12 Hexblock C 
\ init_object select deselect                          16sep86we
                                                                
little beschreibt ein kleines Rechteck in Bildschirmmitte.      
big    beschreibt ein Rechteck in der Grîûe des Objekts.        
                                                                
initialisiert little und big auf die Grîûen des darzustellenden 
 Objekts. Die Koordinaten des Objekts werden in der Resource (!)
 so geÑndert, daû es auf dem Bildschirm zentriert erscheint.    
laddr ist die Langadresse des Statuswortes des Objekts object#. 
setzt den Status des Objekts object# auf selected (revers).     
setzt den Status des Objekts object# auf deselected (normal).   
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 13 Hexblock D 
\ show_object hide_object objc_setpos objc_getwh       16sep86we
                                                                
zeichnet das Objekt auf dem Bildschirm und rettet den Hinter-   
 grund. Die Treenummer des Objekts muû mit tree! gesetzt sein.  
 Das Objekt wird mit (bis zu) drei Unterebenen gezeichnet.      
 Die Maus wird eingeschaltet.                                   
entfernt das Objekt vom Bildschirm und restauriert den Hinter-  
 grund.                                                         
                                                                
x und y sind die Koordinaten der oberen rechten Ecke, an der    
 das Objekt object# auf dem Bildschirm erscheinen soll.         
                                                                
width und height sind Breite und Hîhe des Objekts object#.      
                                                                
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 15 Hexblock F 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
