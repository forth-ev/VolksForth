\ *** Block No. 0 Hexblock 0 
\\ Install Editor                                    cas 10nov05
                                                                
This file contains the Installer for the Forth Editor           
                                                                
The Installer will query for keystrokes that should invoke      
the Editor commands.                                            
                                                                
This allows custom keybinding for the individual requirements   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ install Editor                                     cas 10nov05
                                                                
Onlyforth Editor also  save   warning on                        
                                                                
: tab      &20 col &20 mod - spaces ;                           
: .key ( c -- )                                                 
    dup $7E > IF ." $" u. exit THEN                             
    dup bl  < IF ." ^" [ Ascii A 1- ] Literal +  THEN emit ;    
                                                                
: install   \ install editor's keyboard                         
   page ." Press keys requested (Spacebar to confirm)"          
   #keys 0 ?DO  cr I 2* actiontable + @ >name .name             
     tab ." : " I 2* keytable + dup @ .key   tab ." -> "        
     key dup bl = IF drop dup @ THEN  dup .key  swap !          
   LOOP ;                                                       
-->                                                             
\ *** Block No. 2 Hexblock 2 
\ define action-names                                 UH 11mai88
: :a ( addr -- adr' )  dup @ Alias  2+ ;                        
actiontable                                                     
:a up          :a left         :a down        :a right          
:a push-line   :a push-char    :a pull-line   :a pull-char      
:a fix-word    :a screen#      :a copy-line   :a copy-char      
:a backspace   :a backspace    :a backspace   :a delete-char    
( :a insert-char )             :a delete-line :a insert-line    
:a flipimode                 ( :a erase-line  :a clear-to-right)
:a new-line    :a +tab         :a -tab                          
:a home        :a to-end       :a search      :a undo           
:a update-exit :a flushed-exit :a showload    :a shadow-screen  
:a next-Screen :a back-Screen  :a alter-Screen :a mark-screen   
drop                                                            
                                                                
warning off  install  empty                                     
\ *** Block No. 3 Hexblock 3 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 5 Hexblock 5 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 6 Hexblock 6 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 11 Hexblock B 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 12 Hexblock C 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 13 Hexblock D 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 15 Hexblock F 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 16 Hexblock 10 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 17 Hexblock 11 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
