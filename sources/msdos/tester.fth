\ *** Block No. 0 Hexblock 0 
\ ANS Forth Compatibility Tester                     cas 25jun20
                                                                
\ From: John Hayes S1I                                          
\ Subject: tester.fr                                            
\ Date: Mon, 27 Nov 95 13:10:09 PST                             
                                                                
\ (C)1995 JOHNS HOPKINS UNIVERSITY / APPLIED PHYSICS LABORATORY 
\ MAY BE DISTRIBUTED FREELY AS LONG AS THIS COPYRIGHT NOTICE    
\ REMAINS.                                                      
\ VERSION 1.2                                                   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ ANS Forth Compatibility Tester                     cas 25jun20
                                                                
: \vf  [compile] \ ; immediate                                  
                                                                
1 5 +thru .( ANS Forth Tester Loaded ... )                      
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ Test Unit Tools                                    cas 25jun20
                                                                
HEX                                                             
                                                                
\ SET THE FOLLOWING FLAG TO TRUE FOR MORE VERBOSE OUTPUT;       
\ THIS MAY                                                      
\ ALLOW YOU TO TELL WHICH TEST CAUSED YOUR SYSTEM TO HANG.      
VARIABLE VERBOSE                                                
   FALSE VERBOSE !                                              
                                                                
\ ( ... -- ) EMPTY STACK: HANDLES UNDERFLOWED STACK TOO.        
: EMPTY-STACK                                                   
  DEPTH ?DUP IF DUP 0< IF                                       
    NEGATE 0 DO 0 LOOP ELSE 0 DO DROP LOOP THEN                 
  THEN ;                                                        
                                                                
\ *** Block No. 3 Hexblock 3 
\ Unit Test Tools                                    cas 25jun20
                                                                
VARIABLE #ERRORS 0 #ERRORS !                                    
                                                                
\ ( C-ADDR U -- ) DISPLAY AN ERROR MESSAGE FOLLOWED BY          
\ THE LINE THAT HAD THE ERROR.                                  
                                                                
: ERROR                                                         
  CR TYPE SOURCE TYPE  \ DISPLAY LINE CORRESPONDING TO ERROR    
  EMPTY-STACK     \ THROW AWAY EVERY THING ELSE                 
  #ERRORS @ 1 + #ERRORS !                                       
  \   QUIT  \ *** Uncomment this line to QUIT on an error       
;                                                               
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
\ Unit Test Tools                                    cas 25jun20
                                                                
VARIABLE ACTUAL-DEPTH         \ STACK RECORD                    
CREATE ACTUAL-RESULTS 20 CELLS ALLOT                            
                                                                
: T{      \ ( -- ) SYNTACTIC SUGAR.                             
   ;                                                            
                                                                
: ->      \ ( ... -- ) RECORD DEPTH AND CONTENT OF STACK.       
   DEPTH DUP ACTUAL-DEPTH !      \ RECORD DEPTH                 
   ?DUP IF            \ IF THERE IS SOMETHING ON STACK          
      0 DO ACTUAL-RESULTS I CELLS + ! LOOP \ SAVE THEM          
   THEN ;                                                       
                                                                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
\ Unit Test Tools                                    cas 25jun20
                                                                
\ ( ... -- ) COMPARE STACK (EXPECTED) CONTENTS WITH SAVED       
\ (ACTUAL) CONTENTS.                                            
: }T                                                            
  DEPTH ACTUAL-DEPTH @ = IF     \ IF DEPTHS MATCH               
    DEPTH ?DUP IF               \ IF THERE IS SOMETHING ON THE  
                                \ STACK                         
    0  DO            \ FOR EACH STACK ITEM                      
     ACTUAL-RESULTS I CELLS + @  \ COMPARE ACTUAL WITH EXPECTED 
     = 0= IF S" INCORRECT RESULT: " ERROR LEAVE THEN            
    LOOP                                                        
  THEN                                                          
ELSE                            \ DEPTH MISMATCH                
  S" WRONG NUMBER OF RESULTS: " ERROR                           
THEN ;                                                          
\ *** Block No. 6 Hexblock 6 
\ Unit Test Tools                                    cas 25jun20
                                                                
: TESTING   \ ( -- ) TALKING COMMENT.                           
  SOURCE VERBOSE @                                              
   IF DUP >R TYPE CR R> >IN !                                   
   ELSE >IN ! DROP [CHAR] * EMIT                                
   THEN ;                                                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
