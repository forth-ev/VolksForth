\ *** Block No. 0 Hexblock 0 
\\ Startup: Load Standard System                     cas 11nov05
                                                                
This file can be used to create a minimal volksFORTH from       
a plain KERNEL.COM.                                             
                                                                
The System will be saved as "MINIMAL.COM".                      
                                                                
The minimal volksFORTH contains a simple line editor from       
the book "Starting Forth". The minimal system can be used to    
adapt EDITOR.FB and VOLKS4TH.SYS for special hardware.          
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ System LOAD-Screen for MS-DOS volksFORTH           cas 11nov05
  Onlyforth   warning off                                       
                                                                
  include extend.fb                                             
  include tools.fb                                              
  include rfe.fb  \ retro forth editor                          
                                                                
  : initial  rfe.fb 0 list restart ;  ' initial Is 'cold        
                                                                
  warning on   clear                                            
  savesystem MINIMAL.COM bell                                   
                                                                
  .( New System saved as "MINIMAL.COM". ) cr                    
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
