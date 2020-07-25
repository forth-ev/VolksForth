\ *** Block No. 0 Hexblock 0 
                                                                
This video display interface utilizes the ROM BIOS call $10.    
The display is fairly fast and should work on most IBM          
compatible computers                                            
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ BIOS display interface                     ks  1 secas 09jun20
  Onlyforth   \needs Assembler   2 loadfrom asm.fb              
  Variable dpage        dpage off                               
  Variable top          top off                                 
                                                                
  Code (at    ( lin col -- )   A pop   R push   U push          
     dpage #) R+ mov   A- D+ mov   2 # A+ mov   $10 int         
     U pop   R pop   D pop   Next   end-code                    
                                                                
  Code (at?   ( -- lin col )   D push   R push   U push         
     dpage #) R+ mov   3 # A+ mov   $10 int   U pop   R pop     
     D+ A- mov   0 # A+ mov   A+ D+ mov   A push   Next         
  end-code                                                      
                                                                
  1 6 +thru   .( BIOS display interface active) cr              
                                                                
\ *** Block No. 2 Hexblock 2 
\ BIOS  normal invers  blankline                  ks  1 sep 86  
  : full            top off ;                                   
                                                                
  Variable attribut     7 attribut !                            
                                                                
  : normal     7 attribut ! ;   : invers   $70 attribut ! ;     
  : underline  1 attribut ! ;   : bright    $F attribut ! ;     
                                                                
  Code blankline      D push   R push   U push                  
     dpage #) R+ mov   attribut #) R- mov                       
     3 # A+ mov   $10 int   ' c/row >body #) C mov              
     D- C- sub   bl # A- mov   9 # A+ mov   $10 int             
     U pop   R pop   D pop   Next   end-code                    
                                                                
| : lineerase   0 (at blankline ;                               
                                                                
\ *** Block No. 3 Hexblock 3 
\ curshape  setpage  curat?                       ks  8 mar 88  
                                                                
  Code curshape  ( top bot -- )   D C mov   D pop               
     D- C+ mov   1 # A+ mov   $10 int   D pop   Next            
  end-code                                                      
                                                                
  Code setpage   ( n -- )                                       
     $503 # A mov   D- A- and   $10 int   D pop   Next          
  end-code                                                      
                                                                
  ' (at? Alias curat?                                           
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
\ BIOS  (type  (emit                              ks  1 sep 86  
                                                                
  Code (type   ( addr len -- )   W pop   R push   U push        
     D U mov   dpage #) R+ mov   attribut #) R- mov             
     3 # A+ mov   $10 int   U inc   C push   $E0E # C mov       
     1 # A+ mov   $10 int   1 # C mov   [[  U dec  0= not       
     ?[[  D- inc   ' c/row >body #) D- cmp  0= not              
          ?[[  W ) A- mov   W inc   9 # A+ mov                  
               $10 int   2 # A+ mov   $10 int  ]]?              
     ]?   C pop   1 # A+ mov   $10 int                          
     U pop   R pop   D pop   ' pause #) jmp                     
  end-code                                                      
                                                                
  : (emit  ( char -- )   sp@ 1 (type drop ;                     
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
\ BIOS  (del scroll (cr (page                     ks  2 sep 86  
                                                                
  : (del    (at? ?dup                                           
     IF  1- 2dup (at  bl (emit  (at exit  THEN  drop ;          
                                                                
  Code scroll   D push   R push   U push   attribut #) R+ mov   
     top #) C+ mov   0 # C- mov   ' c/row >body #) D- mov       
     D- dec   ' c/col >body #) D+ mov   D+ dec   D+ dec         
     $601 # A mov   $10 int   U pop   R pop   D pop   Next      
  end-code                                                      
                                                                
  : (cr     (at? drop 1+ dup 2+ c/col u>                        
     IF  scroll 1-  THEN  lineerase ;                           
                                                                
  : (page   top @ c/col 2- DO  I lineerase  -1 +LOOP ;          
                                                                
\ *** Block No. 6 Hexblock 6 
\ BIOS  status display                            ks  2 sep 86  
                                                                
  ' (emit ' display   2 + !  ' (cr   ' display   4 + !          
  ' (type ' display   6 + !  ' (del  ' display   8 + !          
  ' (page ' display &10 + !                                     
  ' (at   ' display &12 + !  ' (at?  ' display &14 + !          
                                                                
  : .sp      ( n -- )  ."  s" depth   swap 1+ - 2 .r ;          
  : .base    base @  decimal dup 2 .r   base ! ;                
  : (.drv    ( n -- )   Ascii A + emit ." : " ;                 
  : .dr      ."   " drv (.drv ;                                 
  : .scr     blk @ IF  ."   Blk" blk  ELSE  ."   Scr" scr  THEN 
             @ 5 .r ;                                           
  : .space   ."   Dic" s0 @ here $100 + - 6 u.r ;               
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
\ statuszeile                                 ks  1 sep 86      
                                                                
| : fstat  ( n -- )   .base .sp                                 
     .space .scr .dr file? 2 spaces order ;                     
                                                                
  : .stat    attribut @   output @  (at?                        
     display  invers  c/col 1- 0 (at  4 fstat                   
     blankline  (at   output !   attribut ! ;                   
                                                                
  : +stat   ['] .stat Is .status  .status ;                     
                                                                
  : -stat   [']  noop Is .status ;                              
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
