\ *** Block No. 0 Hexblock 0 
                                                     cas 10nov05
Video display interface for an ANSI.SYS interface.              
It should work on any MS-DOS computer. Since ANSI.SYS does      
not have a delete line function, split screen can not           
be implemented as usual. Instead, the cursor "rotates"          
ie. when a CR is performed on the bottom line, the cursor       
moves up to the top line in the current window.                 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ ansi cursor control                                cas 10nov05
  Onlyforth                                                     
                                                                
| : (char"    "lit count bounds DO I c@ charout LOOP ;          
| : char"     compile (char" ," align ; immediate restrict      
                                                                
| Ascii 0 Constant #0                                           
                                                                
| : (#S)  ( u -- )  &10 /mod  #0 + charout   #0 + charout ;     
                                                                
  : (at  ( row col -- )     char" ["                           
     swap 1+ (#S) char" ;" 1+ (#S) char" H" ;                   
                                                                
| : )##(  ( -- u )   (key #0 - &10 *   (key #0 -  + ;           
                                                                
  1 4 +thru   .( ANSI display interface active) cr              
\ *** Block No. 2 Hexblock 2 
\ Ansi Standard display output                       cas 10nov05
| : keydrop    (key drop ;                                      
                                                                
  : (at?    char" [6n"    keydrop   keydrop                    
     )##( 1-  keydrop  )##( 1-  keydrop keydrop ;               
                                                                
  Variable top   top off                                        
                                                                
  : full         top off ;                                      
                                                                
  : blankline    char" [K" ;                                   
| : lineerase    0 (at blankline ;                              
                                                                
  : normal     char" [0m" ;  : invers     char" [7m" ;        
  : underline  char" [4m" ;  : bright     char" [1m" ;        
                                                                
\ *** Block No. 3 Hexblock 3 
\ Ansi Standard display output                       cas 10nov05
                                                                
  ' 2drop Alias curshape                                        
  ' drop  Alias setpage                                         
  ' (at?  Alias curat?                                          
                                                                
  : (type  ( addr len -- )   pad place                          
     pad count bounds ?DO  I c@ (emit  LOOP ;                   
                                                                
  : (cr   top @ 0=  adr .status @ ['] noop = and                
     IF  (cr exit  THEN   row c/col 2- u<                       
     IF  row 1+  ELSE  top @  THEN  lineerase ;                 
                                                                
  : (page   top @ 0= IF  char" [2J" exit  THEN                 
     top @ c/col 2- DO  I lineerase  -1 +LOOP ;                 
                                                                
\ *** Block No. 4 Hexblock 4 
\ statusline                                         cas 10nov05
                                                                
  ' (cr   ' display   4 + !  ' (type ' display   6 + !          
  ' (page ' display &10 + !                                     
  ' (at   ' display &12 + !  ' (at?  ' display &14 + !          
                                                                
                                                                
  : .sp      ( n -- )  ."  s" depth   swap 1+ - 2 .r ;          
  : .base    base @  decimal dup 2 .r   base ! ;                
  : (.drv    ( n -- )   Ascii A + emit ." : " ;                 
  : .dr      ."   " drv (.drv ;                                 
  : .scr     blk @ IF  ."   Blk" blk  ELSE  ."   Scr" scr  THEN 
             @ 5 .r ;                                           
  : .space   ."   Dic" s0 @ here $100 + - 6 u.r ;               
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
\ statusline                                         cas 10nov05
                                                                
| : fstat  ( n -- )   invers .base .sp                          
     .space .scr .dr file? 2 spaces order normal ;              
                                                                
  : .stat    output @  (at?   display  c/col 1- 0 (at           
     3 fstat blankline   (at  output ! ;                        
                                                                
  : +stat   ['] .stat Is .status  .status ;                     
                                                                
  : -stat   [']  noop Is .status ;                              
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 6 Hexblock 6 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
