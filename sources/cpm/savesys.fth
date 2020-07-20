\ *** Block No. 0 Hexblock 0 
\\ savesystem                                            11Nov86
                                                                
Dieses File enthaelt das Utility-Wort SAVESYSTEM.               
                                                                
Mit ihm kann man das gesamte System als File auf Disk schreiben.
                                                                
Achtung:                                                        
   Es wird SAVE ausgefuehrt, daher ist nach SAVESYSTEM          
   der Heap geloescht!                                          
                                                                
Benutzung:     SAVESYSTEM <filename>                            
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ savsystem                                              05Nov86
                                                                
: savesystem \ filename                                         
   save  $100 here over -  savefile ;                           
                                                                
                                                                
\\ Einfaches savesystem                                  18Aug86
                                                                
| : message ( -- )                                              
   base push decimal                                            
   cr ." ready for SAVE " here 1-  $100  / u.                   
   ." VOLKS4TH.COM" cr ;                                        
                                                                
: savesystem ( -- )    save message bye ;                       
                                                                
                                                                
