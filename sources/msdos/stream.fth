\ *** Block No. 0 Hexblock 0 
\                                                    cas 11nov05
The word STREAM>BLK convert a sequiential file with CR lineend  
into a screenfile with 64 Chars per line.                       
                                                                
Example:                                                        
FORTH.TXT is a Forth-Sourceode in a sequiential file            
                                                                
MAKEFILE FORTH.FB  will create an empty screenfile              
FROM FORTH.TXT     will define the inputfile                    
STREAM>BLK         will convert FORTH.TXT into FORTH.FB         
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\                                                 ks 06 jul 88  
  Onlyforth Dos also                                            
                                                                
| : in   ( -- fcb )  fromfile @ ;                               
| : out  ( -- fcb )    isfile @ ;                               
                                                                
| : padd ( cnt -- )  dup IF  c/l mod ?dup 0=exit  THEN          
     c/l swap ?DO  BL out fputc  LOOP ;                         
                                                                
| : skipctrl  ( -- char )                                       
     BEGIN  in fgetc dup #cr = ?exit                            
            dup 0 BL uwithin 0=exit  drop  REPEAT ;             
                                                                
  2 3 thru                                                      
                                                                
  Onlyforth                                                     
\ *** Block No. 2 Hexblock 2 
\                                                 ks 06 jul 88  
                                                                
| : lastline?  ( -- f )   false  0  skipctrl                    
     BEGIN  -1 case? IF  ?dup IF  padd   THEN  0= exit  THEN    
            #cr case? 0= WHILE  out fputc  1+  in fgetc  REPEAT 
     padd ;                                                     
                                                                
  : stream>blk   open  out freset                               
     out f.size 2@ out fseek \ append to end of file            
     BEGIN  lastline? stop? or UNTIL  close  out fclose ;       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\ absolute blocks in file eintragen               ks 11 aug 87  
                                                                
| : >stream   ( blk -- )                                        
     fromfile @ (block b/blk bounds                             
     DO  ds@ I C/L -trailing out lfputs                         
         #cr out fputc  #lf out fputc   C/L +LOOP ;             
                                                                
  : blk>stream  ( from.blk to.blk -- )  emptyfile               
     1+ swap DO  I >stream  LOOP  close ;                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 6 Hexblock 6 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
