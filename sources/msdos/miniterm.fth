\ *** Block No. 0 Hexblock 0 
\\ Terminalprogramm mit Blockinterface          ( 08.03.91/KK ) 
                                                                
 Autor: Klaus Kohl, 30.01.89 aus FG-FORTH des RTX entnommen     
                                                                
                     Beschreibung:                              
                                                                
 Kleines Beispiel  zur Implementation eines Fileinterfaces ber 
 die serielle Schnittstelle (Achtung: immer 8 Datenbits)        
                                                                
 Die Schnittstellenbefehle  stammen aus dem  PC-volksFORTH 3.81 
 von Klaus Schleisiek.  Sie wurden weitgehend unver„ndert ber- 
 nommen, sind aber auf 4KByte-Puffer erweitert.                 
 File: SERIAL.SCR                                               
                                                                
 Umstellung des  Ports durch  Ausmaskierung der  entsprechenden 
 Zeilen in Screen 2 (momentan COM1 aktiviert).                  
\ *** Block No. 1 Hexblock 1 
\ LOADSCREEN                                         cas 28jun20
                                                                
Onlyforth               \ Suchreihenfolge: FORTH  FORTH ONLY    
\needs Assembler  2 loadfrom asm.fb     \ Assembler nachladen   
                                                                
  FROM source.img  ( File for SAVESYSTEM )                      
                                                                
  $20 >label I_ctrl             \ 8259-Register                 
  $21 >label I_mask             \ 8259-Mask                     
                                                                
  &02 &11 THRU  ( SIO-Terminalroutines )                        
  &12 &17 THRU  ( extended command words )                      
  &18 LOAD      ( Terminalprogram )                             
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ Addresses and Constants                            cas 28jun20
                                                                
| $C 4 * Constant SINT@         \ SIO-Interuptvector COM 1/3    
\ $B 4 * Constant SINT@         \ SIO-Interuptvector COM 2/4    
| $10 Constant I_level          \ 8259-Interuptlevel COM 1/3    
\ $08 Constant I_level          \ 8259-Interuptlevel COM 2/4    
( Port address)                                                 
| $3F8 >label Portadr           \ Portaddress COM1:             
\ $2F8 >label Portadr           \ Portaddress COM2:             
\ $3E8 >label Portadr           \ Portaddress COM3:             
\ $2E8 >label Portadr           \ Portaddress COM4:             
( Selection of Baud rate )                                      
\ &96 >label baud               .(  1200 Baud )                 
\ &48 >label baud               .(  2400 Baud )                 
| &12 >label baud               .(  9600 Baud )                 
\ &02 >label baud               .( 57600 Baud )                 
\ *** Block No. 3 Hexblock 3 
\ Queue and required commands                        cas 28jun20
                                                                
( Dataqueue with 128 bytes and two pointer for IRQ service )    
( Queue+0: Number of saved characters )                         
( Queue+1: offset to next char to be send )                     
 Create Queue  0 , 0 ,  $1000 allot                             
                                                                
\ send byte to port address ( b adr -- )                        
\needs pc!   Code pc!  A pop   D byte out   D pop   Next        
                                                                
\ Read Byte from port address ( adr -- b )                      
\needs pc@   Code pc@  D byte in   A- D- mov   D+ D+ xor   Next 
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
\ tx? = Request status for sending char              cas 28jun20
                                                                
( test if a char cn be send )                                   
  Code tx?      ( -- f )        \ f=-1, ready to send           
   D push               \ TOS to datastack (TOS=Top Of Stack)   
   Portadr 5 + # D mov  \ move status address into D reg        
   D in                 \ get port into register A              
   D D xor              \ set D register to 0                   
   $1020 # A and        \ mask  % 0001 0000 0010 0000           
   $1020 # A cmp        \ tes if these bits are set             
   0= ?[  D dec  ]?     \ char output permitted ?               
   Next                 \ compiling "Next" wurg macro           
  end-code                                                      
                                                                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
\ (tx  tx  = transmit                                cas 28jun20
                                                                
( unconditional send byte directly to 8250-Port )               
  Code (tx      ( char -- )                                     
   D- A- xchg           \ load char into AL-register            
   Portadr # D mov      \ load port address in D-register       
   D byte out           \ transmit AL                           
   D pop                \ load next stack value into D-register 
   Next                 \ compiling "Next"                      
  end-code                                                      
                                                                
( wait until last char has been send )                          
  : tx          ( char -- )                                     
   BEGIN tx? UNTIL      \ wait until SIO ready                  
   (tx ;                \ now write to port                     
                                                                
\ *** Block No. 6 Hexblock 6 
\ -DTR  +DTR  = Data Terminal Ready on/off           cas 28jun20
( DTR-Line to +12 V = logical zero )                            
  Code -DTR     ( -- )                                          
   D push               \ save TOS                              
   Portadr 4 + # D mov  \ get Address of Port Controllregister  
   D byte in            \ move content to AL register           
   $1C # A- and         \ DTR and RTS to 0 = +12 V              
   D byte out           \ write AL back into port register      
   D pop                \ restore TOS                           
   Next                 \ next FORTH words                      
  end-code                                                      
( set DTR and RTS back to 1 = -12 V )                           
  Code +DTR     ( -- )                                          
   D push   Portadr 4 + # D mov                                 
   D byte in   3 # A- or   D byte out                           
   D pop   Next    end-code                                     
\ *** Block No. 7 Hexblock 7 
\ receive queue and interrupt service routine   ( 21.02.89/KK ) 
                                                                
| Label S_INT                                                   
     D push   I push   A push                                   
     Queue # I mov   C: seg I ) A mov                           
     A D mov   A inc   $FFF # A and   C: seg A I ) mov   D I ADD
     Portadr # D mov   D byte in   C: seg A- 4 I D) mov         
     $20 # A- mov   I_ctrl #) byte out          \ EOI for 8259  
     A pop   I pop   D pop   iret                               
  end-code                                                      
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
\ rx?  = request status for reading from Queue       cas 28jun20
| Code rx?  ( -- f )   D push                                   
     Queue #) D mov   Queue 2+ #) D XOR                         
     Next   end-code                                            
                                                                
\\ Query if a char can be read from the queue                   
  Code rx?      ( -- f ) ( f<>0, if char ready )                
   D push               \ TOS to datastack                      
   D D xor              \ D-register to 0                       
   Queue #) D- mov      \ get number if DL and                  
   D- D- or             \ test for 0                            
   0= ?[  [[  D push                       \ if queue empty     
       Portadr 4 + # D mov                 \ activate S8 again  
       D byte in  $B # A- or   D byte out  \ without changing   
       D pop                               \ D register         
swap ]?  Next   end-code                                        
\ *** Block No. 9 Hexblock 9 
\ (rx  rx  = receive char from queue                 cas 28jun20
                                                                
( get char from queue, adjust pointer )                         
  Code (rx      ( -- char )                                     
     D push   I push                                            
     Queue 2+ # I mov   C: seg I ) A mov                        
     A D mov   A inc   $FFF # A and   C: seg A I ) mov   D I ADD
     C: seg 2 I D) A- mov   0 # A+ mov  A D mov                 
     I pop   Next   end-code                                    
                                                                
( get char, wait for char available )                           
  : rx          ( -- char )                                     
   BEGIN rx? UNTIL (rx ;                                        
                                                                
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
\ S_init  = initialize serial interface              cas 28jun20
| Code S_init   ( -- )                                          
   D push   D: push               \ save TOS and DS register    
   A A xor   A D: mov   C: A mov            \ 0 -> DS ; CS -> A 
   SINT@ # W mov   S_INT # W ) mov      \ set IRQ vector        
   A 2 W D) mov   D: pop            \ and restore DS register   
   Portadr 3 + # D mov                                          
   $80 # A- mov   D byte out   \ enable Baud-rate register      
   2 # D sub  baud # A mov  A- A+ xchg  D byte out   \ set the  
       D dec                A- A+ xchg  D byte out   \ BAUD rate
   3 # D add   $A07 # A mov   D out      \ 8bit, noP, +RTS +OUT 
   2 # D sub   1 # A- mov   D byte out   \ enable RX IRQ        
   I_mask #) byte in                                            
   I_level Forth not Assembler # A- and       \ activate 8259   
   I_mask #) byte out                                           
   D pop   Next  end-code                                       
\ *** Block No. 11 Hexblock B 
\ init  -init   = Initialization / Reset             cas 28jun20
                                                                
\needs init   | : init ;                                        
                                                                
( clear queue pointer and initialize port and interrupt )       
  : init        ( -- )                                          
   init   Queue off  Queue 2+ off   S_init ;                    
                                                                
( block IRQ, disable RTS and DTR )                              
  : -init       ( -- )                                          
   0 [ Portadr 1+ ] Literal  pc!        \ disable 8259 IRQ      
   0 [ Portadr 4 + ] Literal pc!        \ -RTS/-rts/-out2       
   I_mask pc@ I_level or I_mask pc! ;   \ block 8259            
                                                                
                                                                
                                                                
\ *** Block No. 12 Hexblock C 
\ rxto  rxwto  = receive char with timeout           cas 28jun20
                                                                
| &1000 Constant Timeout        \ exit after 1000 iterations    
                                                                
( get a char )                                                  
| : rxto        ( -- char 0 | f ) ( f=-1 signals error )        
   Timeout                              \ number iterations     
   BEGIN rx? IF drop (rx 0 exit THEN    \ char available?       
         1- DUP 0=                      \ Timeout ?             
   UNTIL DROP -1 ;                                              
                                                                
( get a word, Highbyte first )                                  
| : rxwto       ( -- n 0 | f )                                  
   rxto ?dup ?exit             \ exit when Timeout in 1st byte  
   &256 * rxto               \ move to highbyte, get lowbyte    
   if drop -1 else OR 0 then ;     \ Timeout -> error flag      
\ *** Block No. 13 Hexblock D 
\ info.  blk>sio  sio>blk  = Forth Block I/O         cas 28jun20
: info.         ." Block: " dup . cr ;                          
: blk>sio       ( b -- f )      ( Block to target machine )     
   dup capacity u<                                              
   if  cr ." HOST -> TA -" info. block 0 tx                     
       &1024 0 DO dup c@ tx 1+ LOOP drop                        
   else drop 9 tx                                               
   then 0 ;                                                     
: sio>blk       ( b -- f )      ( Block from Target )           
   dup capacity u<                                              
   if   cr ." TA -> HOST -" info. flush block 0 tx              
      &1024 0 do rxto if   drop &1234 leave                     
                      else over c! 1+       then loop &1234 =   
      if empty-buffers -1 else update flush 0 then              
   else drop 9 tx 0 then ;                                      
                                                                
\ *** Block No. 14 Hexblock E 
\ Extension for img>file                             cas 28jun20
                                                                
VARIABLE TSEG  TSEG OFF      ( Segment-Address of Target-RAM )  
                                                                
: TINIT         ( len -- )                                      
   0 B/SEG UM/MOD SWAP IF 1+ THEN   ( number of blocks )        
   LALLOCATE ABORT" No RAM"         ( reserve )                 
   TSEG ! ;                         ( save address )            
: TFREE         ( -- )              ( release memory )          
   TSEG @ LFREE ABORT" RAM allocated" ;                         
                                                                
: TC!           ( c addr -- )   ( write byte )                  
   TSEG @ SWAP LC! ;                                            
: <TMOVE        ( taddr addr n -- ) ( data from target )        
   >R >R TSEG @ SWAP DS@ R> R> LMOVE ;                          
                                                                
\ *** Block No. 15 Hexblock F 
\ Terminal part for SAVESYSTEM                       cas 28jun20
                                                                
: img>file      ( len -- f )    ( save image file )             
   DUP TINIT   DUP 0   0 tx                                     
   ?DO  rxto ABORT" Savesystem-Error"  I TC!  LOOP              
   PUSHFILE  SOURCE.IMG                                         
   CAPACITY 1-  0  DO I BLOCK &1024 -1 FILL UPDATE LOOP         
   0 $400 UM/MOD   DUP 0                                        
   ?DO  I $400 *  I BLOCK   $400 <TMOVE  UPDATE  LOOP           
   SWAP ?DUP                                                    
   IF   OVER  DUP $400 *  SWAP BLOCK  ROT <TMOVE  UPDATE  THEN  
   DROP FLUSH CLOSE                                             
   TFREE  0 ;                                                   
                                                                
                                                                
                                                                
\ *** Block No. 16 Hexblock 10 
\ tbu  = command interpreter                         cas 28jun20
                                                                
( command interpreter for escape codes )                        
| : tbu         ( -- f ) ( Terminal block transmission )        
   rxto ?dup ?exit                                 \ get code   
     1 case? if rxwto ?dup ?exit blk>sio exit then    \ Transmit
     2 case? if rxwto ?dup ?exit sio>blk exit then    \ Receive 
     3 case? if rxwto ?dup ?exit img>file exit then       \ ROM 
     4 case? if rxwto ?dup ?exit drop page 0 exit then   \ PAGE 
     5 case? if rxto ?dup ?exit rxto ?dup if nip exit then      
                swap at                0 exit then         \ AT 
   $1B case? if $1B tx                 0 exit then     \ ESCAPE 
   drop -1 ;                    \ error unknown command         
                                                                
                                                                
                                                                
\ *** Block No. 17 Hexblock 11 
\ ?rx  = char from terminal                          cas 28jun20
                                                                
( receive and interpret char )                                  
| : ?rx         ( -- )                                          
   pause rx? 0=exit (rx          \ return if no char wainting   
   dup $20 u<                          \ is control char?       
   if                                                           
     $1B case? if tbu abort" Command-Error" exit THEN  \ ESCAPE 
     #LF case? IF cr        exit THEN                    \ CRLF 
     #CR case? IF Row 0 at  exit THEN               \ only CR   
     #BS case? IF del       exit THEN               \ Backspace 
     drop                               \ better ignore these   
   else                                                         
     Col &78 u> if cr then                    \ next line?      
     emit                   \ directly emit char                
   then ;                                                       
\ *** Block No. 18 Hexblock 12 
\ T - Main Terminal command                          cas 28jun20
                                                                
( send char if possible )                                       
| : ?tx         ( c -- )                                        
   BEGIN ?rx tx? UNTIL          \ receive unil SIO is free      
   tx ;                                 \ then transmit         
( Terminal Interpreter Loop )                                   
| : (T          ( -- )                                          
   BEGIN BEGIN  ?rx key?  UNTIL   \ receive until key pressed   
         key    $1B case? IF -DTR exit THEN ?tx  \ exit on ESC  
   REPEAT ;                                                     
( Main program, en-/disables interrupt )                        
  : T           ( -- )                                          
   CR ." TA-Terminal (Exit with ESC)" CR                        
   INIT   (T   -INIT                                            
   CR ." VolksForth " ;                                         
\ *** Block No. 19 Hexblock 13 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
