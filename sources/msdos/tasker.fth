\ *** Block No. 0 Hexblock 0 
\                                                 ks 22 dez 87  
The multitasker is a simple yet powerful round robin scheme     
with explicit task switching. This has the major advantage      
that the system switches tasks only in known states.            
Hence the difficulties in synchronizing tasks and locking       
critical portions of code are greatly minimized or simply       
do not exist at all.                                            
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ Multitasker loadscreen                          ks 03 apr 88  
  Onlyforth   \needs Assembler   2 loadfrom asm.scr             
                                                                
  Code stop         $E990 # U ) mov  ' pause @ # jmp   end-code 
                                                                
  : singletask   [ ' noop @  ] Literal  ['] pause ! ;           
  : multitask    [ ' pause @ ] Literal  ['] pause ! ;           
                                                                
  1 3 +thru   .( Multitasker geladen) cr                        
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ pass activate                                   ks  1 jun 87  
                                                                
  : pass      ( n0 ... nr-1 Taddr r -- )                        
BEGIN [ rot ]                                                   
    swap $E9CD over !       \ awake Task                        
    r> -rot                 \ Stack: IP r addr                  
    8 + >r                  \ s0 of Task                        
    r@ 2+ @ swap            \ Stack:  IP r0 r                   
    2+ 2*                   \ bytes on Taskstack incl. r0 & IP  
    r@ @ over -             \ new SP                            
    dup r> 2- !             \ into Ssave                        
    swap bounds  ?DO  I !  2 +LOOP  ;  restrict                 
                                                                
  : activate  ( Taddr -- )  0  \ [ ' pass >body ] Literal >r ;  
[ -rot ] REPEAT ; restrict                                      
                                                                
\ *** Block No. 3 Hexblock 3 
( Building a Task                          ks  8 may 84 )       
                                                                
| : taskerror   ( string -- )   standardi/o  singletask         
     ." Task error: " count type multitask stop ;               
                                                                
  : sleep     ( addr -- )    $90 swap c! ;                      
                                                                
  : wake      ( addr -- )    $CD swap c! ;                      
                                                                
  : rendezvous   ( semaphoraddr -- )                            
     dup unlock  pause  lock ;                                  
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
\ Task                                            ks  1 jun 87  
                                                                
  : Task   ( rlen slen -- )   clear                             
     0 Constant   here 2- >r      \ addr of task constant       
     here -rot                    \ here for Task dp            
     even allot even              \ allot dictionary area       
     here r> !                    \ set task constant addr      
     up@ here $100 cmove          \ init user area              
     here $E990 ,                 \ JMP opcode                  
     up@ 2+ dup dup @ + here - ,                                
     2dup - 2-  swap !            \ link task                   
     0 , dup 2- dup , ,           \ ssave and s0                
     2dup + ,                     \ here + rlen = r0            
     rot ,                        \ dp                          
     under + dp !  0 ,            \ allot rstack                
     ['] taskerror [ ' errorhandler >body c@ ] Literal rot + ! ;
