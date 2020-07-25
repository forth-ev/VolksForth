\ *** Block No. 0 Hexblock 0 
\\ Double words                                      cas 10nov05
                                                                
This File contains definitions for 32Bit Math                   
                                                                
This definitions are already included in the volksFORTH Kernel: 
                                                                
   2! 2@ 2drop 2dup 2over 2swap d+ d. d.r                       
   d0= d< d= dabs dnegate                                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ 2constant  2rot 2variable d- d2/                ks 22 dez 87  
                                                                
  : 2constant   Create , ,  does>  2@ ;                         
                                                                
  : 2rot   ( d1 d2 d3 -- d2 d3 d1 )  5 roll  5 roll ;           
                                                                
  : 2variable   Variable  2 allot ;                             
                                                                
  : d-  ( d1 d2 -- d3 ) dnegate d+ ;                            
                                                                
  Code d2/  ( d1 -- d2 )                                        
     A pop   D sar   A rcr   A push   Next   end-code           
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ dmax  dmin  du<                                 ks 22 dez 87  
                                                                
  : dmax   ( d1 d2 -- d3 )                                      
     2over 2over d< IF  2swap  THEN  2drop ;                    
                                                                
  : dmin   ( d1 d2 -- d3 )                                      
     2over 2over d< IF  2drop exit  THEN  2swap 2drop ;         
                                                                
  : du<    ( 32b1 32b2 -- f )                                   
     rot  2dup = IF  2drop u< exit  THEN  u> -rot 2drop ;       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
