\ *** Block No. 0 Hexblock 0 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ loadscreen for system IO for Apple1               cas2013apr05
                                                                
                                                                
 1  9 +thru                                                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ 65KEY? GETKEY                                     cas2013apr05
| $D010 Constant KBDDTA                                         
| $D011 Constant KBDCTL                                         
                                                                
| CODE 65KEY? ( -- FLAG) KBDCTL lda 0>= ?[ 0 # lda ][ 1 # lda ]?
                          push0a jmp end-code                   
                                                                
| CODE GETKEY  ( -- 8B)   KBDDTA lda $7F # AND                  
 push0a jmp end-code                                            
                                                                
| CODE CURON   ( --) NEXT JMP END-CODE                          
                                                                
| CODE CUROFF  ( --) NEXT JMP END-CODE                          
                                                                
: 65KEY  ( -- 8B)                                               
    CURON BEGIN PAUSE 65KEY?  UNTIL CUROFF GETKEY ;             
\ *** Block No. 3 Hexblock 3 
\ DECODE EXPECT KEYBOARD      BP28MAY85)               cs08aug05
08 CONSTANT #BS   $0D CONSTANT #CR  &27 CONSTANT #ESC           
                                                                
: 65DECODE  ( ADDR CNT1 KEY -- ADDR CNT2)                       
   #BS CASE?  IF  DUP  IF DEL 1- THEN EXIT  THEN                
   #CR CASE?  IF  DUP SPAN ! EXIT THEN                          
   >R  2DUP +  R@ SWAP C!  R> EMIT  1+ ;                        
                                                                
: 65EXPECT ( ADDR LEN1 -- )  SPAN !  0                          
   BEGIN  DUP SPAN @  U<                                        
   WHILE  KEY  DECODE                                           
   REPEAT 2DROP SPACE ;                                         
                                                                
INPUT: KEYBOARD   [ HERE INPUT ! ]                              
 65KEY 65KEY? 65DECODE 65EXPECT [                               
                                                                
\ *** Block No. 4 Hexblock 4 
\ senden? (emit 65emit        25JAN85RE)            cas2013apr05
                                                                
| $D012 Constant DSP                                            
                                                                
| Code send? ( -- flg )                                         
     DSP lda $80 # AND $80 # EOR push0a jmp end-code            
                                                                
Code (emit ( 8b -- )                                            
     SP X) LDA DSP sta (drop jmp end-code                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
\ EMIT CR DEL PAGE AT AT?     25JAN85RE)            cas2013apr05
                                                                
| Variable out    0 out !         | &40 Constant c/row          
                                                                
: 65emit   ( 8b -- ) BEGIN pause send? UNTIL 1 out +! (emit ;   
                                                                
: 65CR     #CR 65emit  out @  c/row /  1+  c/row *  out ! ;     
                                                                
: 65DEL    ASCII _ 65emit -1 out +! ;                           
                                                                
: 65PAGE   &24 0 DO CR LOOP  out off ;                          
                                                                
: 65at ( row col -- ) .( at einf. ) swap  c/row * + out ! ;     
                                                                
: 65AT?  ( -- ROW COL ) out @  c/row /mod  &24 min swap ;       
                                                                
\ *** Block No. 6 Hexblock 6 
\                                                      er14dez88
                                                                
: 65type ( adr len -- ) bounds ?DO I c@ emit LOOP ;             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
\ TYPE DISPLAY (BYE       BP  28MAY85RE)               er14dez88
                                                                
OUTPUT: DISPLAY   [ HERE OUTPUT ! ]                             
 65EMIT 65CR 65TYPE 65DEL 65PAGE 65AT 65AT? [                   
                                                                
                                                                
| : (bye ;                                                      
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
\ B/BLK DRIVE >DRIVE DRVINIT  28MAY85RE)               er14dez88
                                                                
$400 CONSTANT B/BLK                                             
                                                                
$0AA CONSTANT BLK/DRV                                           
                                                                
| VARIABLE (DRV    0 (DRV !                                     
                                                                
| : DISK ( -- DEV.NO )   (DRV @ 8 + ;                           
                                                                
: DRIVE  ( DRV# -- )      BLK/DRV *  OFFSET ! ;                 
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
\                                                      er14dez88
: >DRIVE ( BLOCK DRV# -- BLOCK' )                               
    BLK/DRV * +   OFFSET @ - ;                                  
: DRV?    ( BLOCK -- DRV# )                                     
    OFFSET @ + BLK/DRV / ;                                      
                                                                
: DRVINIT  NOOP ;                                               
.( fuer reads. u. writes. ist errorhandler erforderlich )       
| : readserial ( adr blk -- )                                   
     &27 emit .( rb ) space base push decimal . cr              
     $400 bounds DO key I c! LOOP ;                             
                                                                
| : writeserial ( adr blk -- )                                  
     &27 emit .( wb ) space base push decimal . cr              
     $400 bounds DO I c@ emit LOOP ;                            
                                                                
\ *** Block No. 10 Hexblock A 
\  (r/w                                                er14decas
                                                                
:  (R/W  ( ADR BLK FILE R/WF -- FLAG)                           
   swap abort" no file"                                         
   IF readserial ELSE writeserial THEN false ;                  
                                                                
' (R/W  IS   R/W                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
