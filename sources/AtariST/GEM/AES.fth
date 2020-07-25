\ *** Block No. 0 Hexblock 0 
\\                  *** AES -Funktionen ***            26may86we
                                                                
Dieses File enth„lt alle AES-Funktionen.                        
                                                                
Zur genauren Beschreibung verweisen wir auf die Dokumentation   
von Digital Research.                                           
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ AES Loadscreen                                     cas20130105
                                                                
\needs GEM           include gem\basics.fb                      
Onlyforth                                                       
\needs 2over         include double.fb                          
Onlyforth  GEM also definitions                                 
  1 +load            cr .( Eventwords loaded) cr                
  7 +load            cr .( Menuwords loaded) cr                 
$0C +load            cr .( Objectwords loaded) cr               
$10 +load            cr .( Formwords loaded) cr                 
$14 +load            cr .( Graphicswords loaded) cr             
$19 +load            cr .( Fileselect loaded) cr                
$1C +load            cr .( Windowwords loaded) cr               
$22 +load            cr .( RSRCwords loaded) cr                 
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ Event Loadscreen                                     01feb86we
                                                                
Onlyforth  GEM also definitions                                 
                                                                
 1  5 +thru                                                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\  event_keybd event_button                            06aug86we
                                                                
: evnt_keybd  ( -- key )   &20 0 1 0 AES ;                      
                                                                
: evnt_button  ( #clicks0 bmask bstate -- #clicks1 )            
    intin 3 array!  &21 3 5 0 AES ;                             
                                                                
\\  #clicks0 is awaitet # of clicks                             
    bmask is a button mask                                      
    bstate is the awaitet state of mouse-button(s)              
    #clicks1 is the actually entered # of clicks                
    bmask + bstate use the convention:                          
    lowest bit is leftmost button etc.                          
    bit = 0 is button up                                        
    bit = 1 is button down                                      
more return parameters are in intout-array                      
\ *** Block No. 4 Hexblock 4 
\  event_mouse event_mesag                             02nov86we
                                                                
: evnt_mouse  ( f leftX topY widht heigth -- )                  
   intin 5 array!  &22 5 5 0 AES drop ;                         
                                                                
\   f = 0 is return on entry of mouse in rectangle              
\   f = 1 is return on exit ...                                 
\   more parameters are in intout                               
                                                                
Create message $10 allot                                        
                                                                
: evnt_mesag  ( -- )                                            
   message >absaddr  addrin 2!   &23 0 1 1 AES drop ;           
                                                                
\ see description of messages in AES documentation              
                                                                
\ *** Block No. 5 Hexblock 5 
\  event_timer                                         06aug86we
                                                                
: evnt_timer  ( dtime -- )                                      
   intin 2 array!  &24 2 1 0 AES drop ;                         
                                                                
\ dtime is a double number for timer count down in milliseconds 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 6 Hexblock 6 
\  evnt_multi                                         bp 12oct86
                                                                
\ because there are too much parameters:                        
                                                                
Create events                                                   
   %00110011 ,    \ timer, message, button + keyboard events on 
   2 , 1 , 1 ,    \ 2 clicks down on left mouse-button          
   here $14 allot $14 erase       \ rectangles unspecified      
   0 , 0 ,        \ 0 millisecond timer-delay                   
                                                                
: prepare      events intin $20 cmove                           
   message >absaddr   addrin 2! ;                               
                                                                
: evnt_multi   ( -- which )   &25 &16 7 1 AES ;                 
                                                                
                                                                
\ *** Block No. 7 Hexblock 7 
\  evnt_dclick                                         06aug86we
                                                                
: evnt_dclick   ( dnew dgetset -- dspeed )                      
   intin 2 array!  &26 2 1 0 AES  ;                             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
\ Menu Loadscreen                                      12aug86we
                                                                
Onlyforth  GEM also definitions                                 
                                                                
 1  4 +thru                                                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
\  objc_tree menuAES                                  bp 12oct86
                                                                
| : ?menuerror   ( flag -- )    0= abort" Menu-Error" ;         
                                                                
| : menuAES   ( opcode #intin #intout #addrin -- intout@ )      
     objc_tree 2@ addrin 2! AES ;                               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
\  menu_bar menu_icheck                                09aug86we
                                                                
: menu_bar  ( showflag  -- )                                    
   intin !  &30 1 1 1 menuAES  ?menuerror ;                     
                                                                
\ showflag = 0 is menubar off,  = 1 is menubar on               
                                                                
                                                                
: menu_icheck  ( item showflag -- )                             
   intin 2 array!  &31 2 1 1 menuAES  ?menuerror ;              
                                                                
\ item is the menu item                                         
\ showflag = 0 is checkmark off,  = 1 is checkmark on           
                                                                
                                                                
                                                                
\ *** Block No. 11 Hexblock B 
\  menu_ienable menu_tnormal                           09aug86we
                                                                
: menu_ienable  ( item enableflag -- )                          
   intin 2 array!  &32 2 1 1 menuAES  ?menuerror ;              
                                                                
\ item is the menuitem#                                         
\ enableflag = 0 is disable item, = 1 is enable item            
                                                                
                                                                
: menu_tnormal  ( title normalflag -- )                         
   intin 2 array!  &33 2 1 1 menuAES  ?menuerror ;              
                                                                
\ title is the title#                                           
\ normalflag = 0 is title reverse, = 1 is title normal          
                                                                
                                                                
\ *** Block No. 12 Hexblock C 
\  menu_text menu_register                             02nov86we
                                                                
: menu_text  ( item laddr -- )                                  
   addrin 4+ 2!  intin !  &34 1 1 2 menuAES ?menuerror ;        
                                                                
\ item is the menuitem#                                         
\ laddr is the address of a 0-terminated replace-string         
                                                                
                                                                
: menu_register  ( apid laddr -- menuid )                       
   addrin 2!  intin !  &35 1 1 1 AES  dup 0< not  ?menuerror ;  
                                                                
\ apid is the application-ID from ACC's applinit                
\ laddr is the address of a 0-terminated string for menutext    
\ menuid is ACC's menu-identifier (0-5)                         
                                                                
\ *** Block No. 13 Hexblock D 
\ Object Loadscreen                                    01feb86we
                                                                
Onlyforth  GEM also definitions                                 
                                                                
 1  3 +thru                                                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
\  objc_tree objcAES objc_add objc_delete              06aug86we
                                                                
| : ?objcerror   ( flag -- )    0= abort" Object-Error" ;       
                                                                
| : objcAES   ( opcode #intin #intout #addrin -- intout@ )      
     objc_tree 2@ addrin 2!  1 AES ;                            
                                                                
: objc_add   ( parent child -- )                                
   intin 2 array!  &40 2 1 objcAES  ?objcerror ;                
                                                                
: objc_delete   ( object --  )                                  
   intin !   &41 1 1 objcAES  ?objcerror ;                      
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 15 Hexblock F 
\  objc_draw objc_find objc_offset                    bp 12oct86
                                                                
: objc_draw   ( startob depth x y width height -- )             
   intin 6 array!  &42 6 1 objcAES  ?objcerror ;                
                                                                
: objc_find   ( startob depth x y -- obnum )                    
   intin 4 array!  &43 4 1 objcAES  ;                           
                                                                
: objc_offset   ( object -- x y )                               
   intin !  &44 1 3 objcAES  ?objcerror                         
   intout 2+ @  intout 4+ @ ;                                   
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 16 Hexblock 10 
\  objc_order objc_edit objc_change                    02feb86we
                                                                
: objc_order    ( object newpos -- )                            
   intin 2 array!  &45 2 1 objcAES  ?objcerror ;                
                                                                
: objc_edit     ( object char index kind -- newindex )          
   intin 4 array!  &46 4 2 objcAES  ?objcerror intout 2+ @ ;    
                                                                
: objc_change   ( object x y width height newstate redraw -- )  
   intin 4+ 6 array!  intin !  intin 2+ off                     
   &47 8 1 objcAES  ?objcerror ;                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 17 Hexblock 11 
\ Object Loadscreen                                    09aug86we
                                                                
Onlyforth  GEM also definitions                                 
 1  2 +thru                                                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 18 Hexblock 12 
\  form_do form_dial                                  bp 12oct86
                                                                
: form_do     ( startobj -- objectno )                          
   intin !  objc_tree 2@  addrin 2!   &50 1 1 1 AES ;           
                                                                
: form_dial   ( diflag lix liy liw lih  bix biy biw bih )       
   intin 9 array!  &51 9 1 0 AES drop ;                         
\ li means little   bi means big                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 19 Hexblock 13 
\  form_alert form_error form_center                07a09sep86we
                                                                
: form_alert   ( defbttn 0string -- exbttn )                    
   >absaddr  addrin 2!  intin !  &52 1 1 1 AES ;                
                                                                
: form_error   ( enum -- exbttn )                               
   intin !  &53 1 1 0 AES ;                                     
                                                                
: form_center  ( -- x y width height )                          
   objc_tree 2@ addrin 2!  &54 0 5 1 AES  drop  intout 2+ 4@  ; 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 20 Hexblock 14 
\  form_alert tests                                   bp 12oct86
                                                                
: test        ( -- button )                                     
    2 0" [1][Dies ist ein Test!|2.Zeile][OK|JA|NEIN]"           
      form_alert ;                                              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 21 Hexblock 15 
\ Graphics Loadscreen                                  02feb86we
                                                                
Onlyforth  GEM also definitions                                 
                                                                
 1  4 +thru                                                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 22 Hexblock 16 
\  graf_dragbox graf_movebox                           06aug86we
                                                                
| : ?graferror   ( flag -- )    0= abort" Graphic-Error" ;      
                                                                
: graf_dragbox                                                  
   ( startx starty width height boundx boundy boundw boundh --  
     finishx finishy )                                          
   intin 8 + 4 array!  intin 2 array!  intin 4+ 2 array!        
   &71 8 3 0 AES  ?graferror  intout 2+ @ intout 4+ @ ;         
                                                                
: graf_movebox                                                  
   ( sourcex sourcey width height destx desty -- )              
   intin 8 + 2 array!  intin 2 array!  intin 4+ 2 array!        
   &72 6 1 0 AES  ?graferror ;                                  
                                                                
                                                                
\ *** Block No. 23 Hexblock 17 
\  graf_growbox graf_shrinkbox                         06aug86we
                                                                
: graf_growbox     ( stx sty stw sth  fix fiy fiw fih -- )      
   intin 8 array!  &73 8 1 0 AES  ?graferror ;                  
                                                                
: graf_shrinkbox   ( fix fiy fiw fih  stx sty stw sth -- )      
   intin 8 array!  &74 8 1 0 AES  ?graferror ;                  
                                                                
\ st means start   fi means finish                              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 24 Hexblock 18 
\  graf_watchbox graf_slidebox                        bp 12oct86
                                                                
: graf_watchbox   ( object instate outstate -- inside/outside ) 
   objc_tree 2@ addrin 2!  intin 2+ 3 array!                    
   &75 4 1 1 AES  ;                                             
                                                                
: graf_slidebox   ( parent object vhflag -- vhpos )             
   objc_tree 2@ addrin 2!  intin 3 array!                       
   &76 3 1 1 AES  ;                                             
                                                                
                                                                
\\ graf_handle is defined in BASICS.SCR  !                      
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 25 Hexblock 19 
\  graf_mouse graf_mkstate                            bp 12oct86
                                                                
2Variable mofaddr      0. mofaddr 2!                            
                                                                
: graf_mouse   ( mouseform -- )                                 
   intin !  mofaddr 2@ addrin 2!  &78 1 1 1 AES  ?graferror ;   
                                                                
: graf_mkstate   ( -- )    &79 0 5 0 AES  drop ;                
                                                                
\ Werte in intout                                               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 26 Hexblock 1A 
\ File Selection Loadscreen                           bp 11oct86
                                                                
Onlyforth                                                       
GEM also definitions                                            
                                                                
1 +load                                                         
                                                                
\\                                                              
                                                                
: test          ( -- button )                                   
      show_c  inpath   &30 erase   name count inpath   place    
              insel    $10 erase   name count insel    place    
              fs_label &30 erase   name count fs_label place    
              fsel_exinput   hide_c  ;                          
                                                                
test A:\GEM\*.SCR  AES.SCR  Dies_ist_eine_Textbox!              
\ *** Block No. 27 Hexblock 1B 
\ File Selection                                      bp 11oct86
                                                                
Create inpath    ," \*.SCR"    here  &30 allot   &30 erase      
Create insel                   here  $10 allot   $10 erase      
                                                                
| : count?  ( addr -- )                                         
     dup 1+  BEGIN  count  0=  UNTIL  over - 2-  swap c! ;      
                                                                
: fsel_input  ( -- button )                                     
   inpath 1+ >absaddr  addrin 2!  insel 1+ >absaddr addrin 4+ 2!
   &90 0 2 2 AES  0= abort" File Error"                         
   inpath count?  insel count?  intout 2+ @ ;                   
                                                   -->          
\\ button = 0 is ABBRUCH, = 1 is OK;  the returned strings      
   are in inpath and insel (counted and 0-terminated)           
                                                                
\ *** Block No. 28 Hexblock 1C 
\ File selection mit FSEL_EXINPUT               13jan90 m.bitter
                                                                
Create fs_label ," May the volks4TH be with you!" 0 c,          
                                                                
: fsel_exinput  ( -- button )                                   
   inpath 1+ >absaddr  addrin 2!  insel 1+ >absaddr addrin 4+ 2!
   fs_label 1+ >absaddr  addrin 8 + 2!                          
   &91 0 2 3 AES  0= abort" File Error"                         
   inpath count?  insel count?  intout 2+ @ ;                   
                                                                
                                                                
                                                                
                                                                
\\ button = 0 is ABBRUCH, = 1 is OK;  the returned strings      
   are in inpath and insel (counted and 0-terminated)           
                                                                
\ *** Block No. 29 Hexblock 1D 
\ Windows Loadscreen                                   28jan86we
                                                                
Onlyforth  GEM also definitions                                 
                                                                
 1  4 +thru                                                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 30 Hexblock 1E 
\  windows                                             21aug86we
                                                                
| : ?winderror   ( flag -- )    0= abort" Window-Error" ;       
                                                                
: wind_create                                                   
   ( components leftX topY maxWidth maxHeight -- handle )       
   intin 5 array!   &100 5 1 0 AES  dup 0> ?winderror ;         
                                                                
\\   component bits set mean:                                   
                                                                
 $0001  title bar             $0002  close box                  
 $0004  full box              $0008  move bar                   
 $0010  info line             $0020  size box                   
 $0040  up arrow              $0080  down arrow                 
 $0100  vertical slider       $0200  left arrow                 
 $0400  right arrow           $0800  horizontal slider          
\ *** Block No. 31 Hexblock 1F 
\  windows                                             06aug86we
                                                                
: wind_open  ( W-handle leftX topY width heigth -- )            
   intin 5 array!  &101 5 1 0 AES  ?winderror ;                 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 32 Hexblock 20 
\  windows                                             06aug86we
                                                                
: wind_close  ( Whandle -- )                                    
   intin !  &102 1 1 0 AES  ?winderror ;                        
                                                                
: wind_delete  ( Whandle -- )                                   
   intin !  &103 1 1 0 AES  ?winderror ;                        
                                                                
: wind_get  ( Whandle funktion# -- )                            
   intin 2 array!  &104 2 5 0 AES  ?winderror ;                 
                                                                
: wind_set  ( Whandle funktion# par0 par1 par2 par3 -- )        
   intin 6 array!  &105 6 1 0 AES  ?winderror ;                 
                                                                
: wind_find  ( mouseX mouseY -- Whandle )                       
   intin 2 array!  &106 2 1 0 AES ;                             
\ *** Block No. 33 Hexblock 21 
\  windows                                             06aug86we
                                                                
: wind_update  ( funktion# -- )                                 
   intin !  &107 1 1 0 AES  ?winderror ;                        
                                                                
: wind_calc  ( 0/1 components leftX topY width heigth -- )      
   intin 6 array!  &108 6 5 0 AES  ?winderror ;                 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 34 Hexblock 22 
\  window test                                         02feb86we
                                                                
 $0FEF &0 &20 &600 &300 wind_create  Constant wtesthandle       
                                                                
: windowtest          page                                      
   wtesthandle 1 &20 &500 &300 wind_open                        
   $20 0 DO  wtesthandle 5 1 &20 &500 I - &300 I - wind_set     
             2 +LOOP                                            
   ." Hit any key to continue "  key drop                       
   wtesthandle wind_close ;                                     
                                                                
: end   wtesthandle  wind_delete  ;                             
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 35 Hexblock 23 
\ RSRC Loadscreen                                      21nov86we
                                                                
Onlyforth  GEM also definitions                                 
                                                                
\needs 0"       include strings.scr                             
                                                                
                                                                
1 4 +thru                                                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 36 Hexblock 24 
\  RSRC words                                         bp 12oct86
                                                                
| : ?rsrcerror  ( f -- )        0= abort" Resource-Error" ;     
                                                                
: rsrc_load  ( 0$ -- )  \ needs address of 0-terminated $       
   >absaddr addrin 2!  &110 0 1 1 AES  ?rsrcerror ;             
                                                                
: rsrc_load"   [compile] 0"  compile rsrc_load ;                
   immediate  restrict                                          
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 37 Hexblock 25 
\  rsrc_gaddr                                        20aug86mawe
                                                                
: rsrc_free   ( -- )   &111 0 1 0 AES  ?rsrcerror ;             
                                                                
: rsrc_gaddr  ( type index -- laddr )                           
   intin 2 array!   &112 2 1 0 AES  ?rsrcerror  addrout 2@ ;    
                                                                
\\   type is one of the following:                              
 0 tree          1 object        2 tedinfo       3 iconblk      
 4 bitblk        5 string        6 imagedata     7 obspec       
 8 te_ptext      9 te_ptmplt    $A te_pvalid    $B ib_pmask     
$C ib_pdata     $D ib_ptext     $E bi_pdata     $F ad_frstr     
$10  ad_frimg                                                   
index is the index of the data structure                        
laddr is the long (double) address of the data structure        
         specified by type and index                            
\ *** Block No. 38 Hexblock 26 
\  rsrc_saddr                                          06aug86we
                                                                
: rsrc_saddr  ( type index laddr --)                            
   addrin 2!  intin 2 array!  &113 2 1 1 AES  ?rsrcerror ;      
                                                                
\\ for type index and f see rsrc_gaddr                          
   laddr is the address of a data structure                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 39 Hexblock 27 
\  rsrc_obfix                                          06aug86we
                                                                
: rsrc_obfix  ( index laddr --)                                 
   addrin 2!  intin !  &114 1 1 1 AES  drop ;                   
                                                                
\ index  is index of object                                     
\ laddr  is addr of tree that contains object                   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
