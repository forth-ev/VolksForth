: fb2fth ( u -- )  
  dup scr ! 
  ." \ *** Block No. "  base @ decimal scr @  dup . ." Hexblock " hex . base ! cr  
   l/s 0 ?do 
      scr @ block i c/l * chars + c/l type cr  
   loop ; 

: bdump 0 do i fb2fth loop ; 
 
get-block-fid file-size drop drop 1024 / 
bdump 

bye

