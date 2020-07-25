\ *** Block No. 0 Hexblock 0 
\\ Simple Files                                          11Nov86
                                                                
Wenn volksFORTH im Direktzugriff Disketten bearbeitet, ist es   
trotzdem wuenschenswert eine Art File-Struktur zu besitzen.     
Dieses File enthaelt eine einfache Implementation eines         
Filesystems. Der/die Programmierer/in muss selbst die Direktory 
auf dem laufenden halten: in ihr sind die Start-Bloecke des     
entsprechenden Diskettenteils gespeichert.                      
Sogar eine Hierarchie von Direktories laesst sich so relisieren.
                                                                
Vorgestellt wurde dieses FileSystem von Georg Rehfeld und auch  
von ihm fuer volksFORTH implementiert (ultraFORTH auf dem C64). 
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ simple files                                           12feb86
                                                                
\needs search   .( search missing) \\                           
                                                                
| Variable (dir    : dir  (dir @ ;   : root  0 (dir ! ;  root   
                                                                
| : read"  ( -- n)                                              
 Ascii " word  count  dup >r  dir block  b/blk  search          
 0= abort" not found"  r> +  >in push  >in !                    
 bl  dir block  b/blk  (word number drop ;                      
                                                                
: load"   read" dir + load ;    : dir"   read" (dir +! ;        
: list"   read" dir + list ;                                    
                                                                
\ 1 +load       \ Only if file" is needed                       
                                                                
\ *** Block No. 2 Hexblock 2 
\ simple files                                           01feb86
                                                                
| : snap   ( n0 -- n1)   $20 /  3 max  $20 * ;                  
: file"  ( n --)                                                
 Ascii " word  count  2dup  dir block b/blk  search             
 IF    + nip                                                    
 ELSE  drop  dir block  b/blk -trailing nip  snap $20 +         
       dup b/blk 1- > abort" directory full"                    
       2dup + >r  dir block + swap  cmove  r>                   
 THEN  snap $18 +  >r                                           
 dir - extend under dabs <# # # # #                             
 base @ $0A = IF  Ascii &  ELSE Ascii $  THEN hold              
 rot 0< IF  Ascii -  ELSE  bl  THEN hold #>                     
 r> dir block +  swap cmove update ;                            
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\ dir load"                                              11feb86
                                                                
\needs search   .( search missing) \\                           
                                                                
0 Constant dir                                                  
                                                                
: load"   ( -- )                                                
 Ascii " word count  dup >r  dir block b/blk search             
     0= abort" not found"   r> +                                
 >in @  blk @  rot >in !  dir blk !                             
 bl word number drop -rot  blk !  >in !  load ;                 
                                                                
                                                                
                                                                
                                                                
                                                                
