\ *** Block No. 0 Hexblock 0 
\\ Install Editor                                               
                                                                
Dieses File enthaelt einen Installer fuer den Editor.           
                                                                
Es werden nacheinander die Tasten erfragt, die einen bestimmten 
Befehl ausloesen sollen.                                        
                                                                
Damit ist es moeglich, die Tastatur an die individuellen        
Beduerfnisse anzupassen.                                        
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ install Editor                                      UH 17Nov86
                                                                
Onlyforth Editor also  save   warning on                        
                                                                
: tab      &20 col &20 mod - spaces ;                           
: .key ( c -- )                                                 
    dup $7E > IF ." $" u. exit THEN                             
    dup bl  < IF ." ^" [ Ascii A 1- ] Literal +  THEN emit ;    
                                                                
: install   \ install editor's keyboard                         
   page ." Entsprechende Tasten druecken. (Blank uebernimmt.)"  
   #keys 0 ?DO  cr I 2* actiontable + @ >name .name             
     tab ." : " I keytable + dup c@ .key   tab ." -> "          
     key dup bl = IF drop dup c@ THEN  dup .key  swap c!        
   LOOP ;                                                       
-->                                                             
\ *** Block No. 2 Hexblock 2 
\ define action-names                                 UH 29Nov86
: :a ( addr -- adr' )  dup @ Alias  2+ ;                        
actiontable                                                     
:a up          :a left         :a down        :a right          
:a push-line   :a push-char    :a pull-line   :a pull-char      
                               :a copy-line   :a copy-char      
:a backspace   :a backspace    :a backspace   :a delete-char    
:a insert-char                 :a delete-line :a insert-line    
:a flipimode                 ( :a erase-line) :a clear-to-right 
:a new-line    :a +tab         :a -tab                          
( :a home        :a to-end )   :a search      :a undo           
:a update-exit :a flushed-exit ( :a showload ):a shadow-screen  
:a next-Screen :a back-Screen  :a alter-Screen :a mark-screen   
drop                                                            
                                                                
warning off  install  empty                                     
\ *** Block No. 3 Hexblock 3 
                                                      UH 17Nov86
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
