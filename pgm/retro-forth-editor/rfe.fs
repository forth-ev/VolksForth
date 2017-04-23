\ Retro Forth Editor for VolksForth ZCN NC100
\ http://retroforth.org/pages/?PortsOfRetroEditor

( Lines per Block ) 
8 constant l/b  

: (block) scr @ block ;                                                             
: (line) c/l * (block) + ;  
: row dup c/l type c/l + cr ;
: .rows l/b 0 do i . row loop ;  
: .block ." Block: " scr @ . space ;   
: +--- ." +---" ;  
: :--- ." :----" ;                                                     
: x--- +--- :--- +--- :--- ;                
: --- space space x--- x--- x--- x--- x--- x--- cr ;  -->     

: vb --- scr @ block .rows drop --- ;                                             
: v cr vb ;  
: v* update v ;  
: s dup scr ! block drop v ;                                          
: ia (line) + >r 10 parse r> swap move v* ;  
: i 0 swap ia ;
: d (line) c/l bl fill v* ;  
: x (block) l/b c/l * bl fill v* ;         
: p -1 scr +! v ;  
: n 1 scr +! v ;  
: e scr @ load ;                
                     
( Patch 'char per line' to 110 )
' c/l >body 110 swap !                      
                                                