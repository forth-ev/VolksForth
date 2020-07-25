\ *** Block No. 0 Hexblock 0 
\\ Terminal-Anpassung                                 UH 08OCt87
                                                                
In diesem File wird volksFORTH an das benutzte Terminal         
angepasst. Ueber folgende Faehigkeiten muss das Terminal        
verfuegen, damit alle Moeglichkeiten von volksFORTH ausgenutzt  
werden koennen:                                                 
                                                                
curon,   curoff   \ Ein- bzw. Ausschalten des Cursors           
rvson,   rvsoff   \ Ein- bzw. Ausschalten der Inversedarstellung
dark              \ Loeschen des Bildschirms                    
locate            \ Positionieren des Cursors auf eine          
                  \ bestimmte Position auf dem Bildschirm       
                                                                
In der Version 3.80a nicht mehr in der Terminal-Anpassung:      
                                                                
curleft, currite  \ Cursor nach links bzw. rechts bewegen       
\ *** Block No. 1 Hexblock 1 
\ Anpassung fuer ANSI-Terminal                     uho 09May2005
| : ccon!! ( addr len -- )  bounds ?DO I C@ con! LOOP ;         
| : con!! ( addr -- ) count ccon!! ;                            
| : ## ( n -- ) base push decimal 0 <# #S #> ccon!! ;           
| : csi ( -- ) #esc con!  Ascii [ con! ;                        
| : ANSIcuron ( -- )  csi " ?25h" con!! ;                       
| : ANSIcuroff ( -- ) csi " ?25l" con!! ;                       
| : ANSIrvson ( -- )   csi " 7m" con!! ;                        
| : ANSIrvsoff ( -- )  csi " 0m" con!! ;                        
| : ANSIdark ( -- ) csi " 2J" con!!  csi " ;H" con!! ;          
| : ANSIlocate ( row col -- )                                   
     csi  swap 1+ ##  ascii ; con! 1+ ## ascii H con! ;         
                                                                
Terminal: ANSI                                                  
noop noop           ANSIrvson ANSIrvsoff ANSIdark ANSIlocate ;  
ANSI page rvson .(  ANSI Terminal installiert. ) rvsoff cr cr   
