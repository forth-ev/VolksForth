\ *** Block No. 0 Hexblock 0 
\ Serial interface for IBM-PC using 8250 chip        cas 11nov05
                                                                
 INCLUDE SERIAL.FB  will load code for COM1,                    
 2 LOADFROM SERIAL.FB  for COM2                                 
                                                                
Bytes recieved will be buffered in a 128 Byte deep Queue        
by an interrupt Routine.                                        
                                                                
The DTR Line will be used to signal that new bytes can be       
recieved.                                                       
The Sender will recognize CTS, a full Handshake is implemented  
                                                                
Xon/Xoff Protocoll using ^S/^Q is _not_ implemented.            
                                                                
Sender:     TX? ( -- f )      TX ( -- char )                    
Empf„nger:  RX? ( -- f )      RX ( char -- )                    
\ *** Block No. 1 Hexblock 1 
\ Driver for IBM-PC Serial card using 8250           cas 11nov05
  Onlyforth   \needs Assembler  2 loadfrom asm.fb               
                                                                
cr .( COM1: )                                                   
                                                                
| $C 4 * Constant SINT@  \ absolute loc. of serial interrupt    
                                                                
  $3F8 >label Portadr                                           
                                                                
| $10 Constant I_level   \ 8259 priority                        
                                                                
  2 7 +thru                                                     
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ Driver for IBM-PC Serial card using 8250           cas 11nov05
  Onlyforth   \needs Assembler  2 loadfrom asm.fb               
                                                                
cr .( COM2: )                                                   
                                                                
| $B 4 * Constant SINT@  \ absolute loc. of serial interrupt    
                                                                
  $2F8 >label Portadr                                           
                                                                
|   8 Constant I_level   \ 8259 priority                        
                                                                
  1 6 +thru                                                     
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\ Driver for IBM-PC Serial card using 8250        ks 11 mai 88  
\   3 .( 38.4 kbaud )                                           
\  &6 .( 19.2 kbaud )                                           
  &12 .( 9.6 kbaud )                                            
\ &24 .( 4.8 kbaud )                                            
\ &96 .( 1200 baud )                                            
  >label baud                                                   
                                                                
  $20 >label I_ctrl      $21 >label I_mask   \ 8259 addresses   
                                                                
  Create Queue  0 , $80 allot                                   
\  0     1     2                      130  byte address         
\ | len | out |<-- 128 byte Queue -->|                          
\ len ::= number of characters queued                           
\ out ::= relativ address of next output character              
\ (len+out)mod(128) ::= relative address of first empty byte    
\ *** Block No. 4 Hexblock 4 
\ transmit to 8250                                ks 11 dez 87  
                                                                
  Code tx?  ( -- f )   D push   Portadr 5 + # D mov             
     D in   D D xor   $1020 # A and   $1020 # A cmp             
     0= ?[  D dec  ]?   Next   end-code                         
                                                                
  Code tx   ( c -- )   D- A- xchg   Portadr # D mov             
     D byte out   D pop   Next   end-code                       
                                                                
  Code -dtr    D push   Portadr 4 + # D mov                     
     D byte in   $1E # A- and   D byte out   D pop   Next       
  end-code                                                      
                                                                
  Code +dtr    D push   Portadr 4 + # D mov                     
     D byte in   1 # A- or   D byte out   D pop   Next          
  end-code                                                      
\ *** Block No. 5 Hexblock 5 
\ receive queue and interrupt service routine     ks 11 dez 87  
                                                                
  Label S_INT     D push   I push   A push                      
     Portadr # D mov   D byte in   A- D+ mov                    
     Queue # I mov   C: seg I ) A mov   A- D- mov   D- inc      
     C: seg D- I ) mov   A+ A- add   $7F # A and   A I add      
     C: seg D+ 2 I D) mov   $68 # D- cmp  CS not                
     ?[  Portadr 4 + # D mov                                    
         D byte in   $1E # A- and   D byte out  ]?  \ -DTR      
     $20 # A- mov   I_ctrl #) byte out          \ EOI for 8259  
     A pop   I pop   D pop   iret                               
  end-code                                                      
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 6 Hexblock 6 
\ rx?  rx                                         ks 30 dez 87  
                                                                
  Code rx?  ( -- f )   D push   D D xor                         
     Queue #) D- mov   D- D- or  0=                             
     ?[  [[  D push   Portadr 4 + # D mov   \ +DTR              
             D byte in   9 # A- or   D byte out   D pop         
swap ]?  Next   end-code                                        
                                                                
  Code rx   ( -- 8b )  I W mov   Queue # I mov                  
     D push   D D xor   cli   lods   A- A- or  0= not           
     ?[  A+ C- mov   A- dec   A+ inc   $7F # A+ and             
         A -2 I D) mov   D- C+ mov   C I add   I ) D- mov       
     ]?  sti   W I mov   $18 # A- cmp  CS not ?]   Next         
  end-code                                                      
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
\ Serial initialization                           ks 25 apr 86  
                                                                
| Code S_init  D push   D: push   A A xor   A D: mov   C: A mov 
     SINT@ # W mov   S_INT # W ) mov   A 2 W D) mov   D: pop    
     Portadr 3 + # D mov   $80 # A- mov   D byte out \ DLAB = 1 
     2 # D sub   baud # A mov   A- A+ xchg   D byte out         
     D dec   A- A+ xchg   D byte out                 \ baudrate 
     3 # D add   $A07 # A mov   D out     \ 8bit, noP, +RTS +OUT
     2 # D sub   1 # A- mov   D byte out             \ +rxINT   
     I_mask #) byte in   I_level Forth not Assembler # A- and   
     I_mask #) byte out   D pop   Next                          
  end-code                                                      
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
\ init  bye                                       ks 11 dez 87  
  \needs init   : init ;                                        
                                                                
  : init   init   Queue off  S_init ;  init                     
                                                                
  : bye    0 [ Portadr 1+ ] Literal  pc!  \ -rxINT              
           0 [ Portadr 4 + ] Literal pc!  \ -dtr/-rts/-out2     
           I_mask pc@ I_level or I_mask pc! bye ;               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
\ dumb terminal via 8250                          ks 11 dez 87  
                                                                
  Variable Fkeys   Fkeys on                                     
                                                                
| : ?rx   ( -- )   pause rx? 0=exit rx                          
     Fkeys @ 0= IF  emit ?cr exit  THEN                         
     #LF case? IF  cr       exit    THEN                        
     #CR case? IF  Row 0 at exit    THEN                        
     #BS case? IF  del exit  THEN   emit ;                      
                                                                
| : ?tx   ( c -- )   BEGIN ?rx tx? UNTIL tx ;                   
                                                                
  : dumb   BEGIN BEGIN  ?rx key?  UNTIL  key                    
                 $1B case? IF -dtr exit THEN ?tx REPEAT ;       
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 11 Hexblock B 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 12 Hexblock C 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 13 Hexblock D 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 15 Hexblock F 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 16 Hexblock 10 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 17 Hexblock 11 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 18 Hexblock 12 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 19 Hexblock 13 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 20 Hexblock 14 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 21 Hexblock 15 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
