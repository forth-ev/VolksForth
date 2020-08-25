\ *** Block No. 0 Hexblock 0 
\ Additional definitions for 32bit values            cas 26jan06
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ 2Words Loadscreen                                  cas 26jan06
                                                                
hex                                                             
 &2  &3 thru                                                    
decimal                                                         
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ 2! 2@ 2VARIABLE 2CONSTANT   08JUL85RE)                        
                                                                
CODE 2!  ( D ADR --)                                            
 TYA  SETUP JSR  3 # LDY                                        
 [[  SP )Y LDA  N )Y STA  DEY  0< ?]                            
 1 # LDY  POPTWO JMP  END-CODE                                  
                                                                
CODE 2@  ( ADR -- D)                                            
 SP X) LDA  N STA  SP )Y LDA  N 1+ STA                          
 SP 2DEC  3 # LDY                                               
 [[  N )Y LDA  SP )Y STA  DEY  0< ?]                            
 XYNEXT JMP  END-CODE                                           
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\                                                               
                                                                
: 2VARIABLE  ( --)   CREATE 4 ALLOT ;                           
             ( -- ADR)                                          
                                                                
: 2CONSTANT  ( D --)   CREATE , ,   DOES> ( -- D)   2@ ;        
                                                                
\ 2DUP  EXISTS                                                  
\ 2SWAP EXISTS                                                  
\ 2DROP EXISTS                                                  
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
