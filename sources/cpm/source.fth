\ *** Block No. 0 Hexblock 0 
\\ volksFORTH CP/M 2.2 rev. 3.80a                        18Nov87
                                                                
Entwicklung des volksFORTH-83 von                               
   K. Schleisiek, B. Pennemann,                                 
   G. Rehfeld, D. Weineck, U. Hoffmann                          
                                                                
Anpassung fuer Intel 8080 und CP/M 2.2 von U. Hoffmann          
                                                                
Dieses File enthaelt den kompletten Sourcetext des Kern-Systems 
fuer die Intel 8080-CPU und die Anpassung an CP/M 2.2 und CP/M+.
Mit Hilfe eines Target-Compilers wird daraus das volksFORTH-    
System erzeugt, daher finden sich an einigen Stellen Anweisungen
an den Target-Compiler, die fuer das Verstaendnis des Systems   
nicht wichtig sind.                                             
Version 3.80a enthaelt gegenueber 3.80 einige Aenderungen, ins- 
besondere die Bdos-Schnittstelle fuer Disk-IO im Kern.          
\ *** Block No. 1 Hexblock 1 
\ CP/M 2.2   volksForth Load Screen                      27Nov87
                                                                
Onlyforth                                                       
    $9000 displace !                                            
Target definitions   $100 here!                                 
                                                                
                                                                
  1 $74 +thru    \ Standard 8080-System                         
                                                                
cr .( unresolved: )  .unresolved   ( ' .blk is .status )        
                                                                
save-target  KERNEL.COM                                         
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ FORTH Preamble and ID                            uho 19May2005
                                                                
Assembler                                                       
                                                                
nop  0 jmp   here 2- >label >boot                               
nop  0 jmp   here 2- >label >cold                               
nop  0 jmp   here 2- >label >restart                            
                                                                
here dup origin!                                                
\ Hier beginnen die Kaltstartwerte der Benutzervariablen        
                                                                
6 rst   0 jmp   end-code  \ for multitasker                     
                                                                
$100 allot                                                      
                                                                
| Create logo ," volksFORTH-83 rev. 3.80a"                      
\ *** Block No. 3 Hexblock 3 
\ Assembler Labels Next Forth-Register                   29Jun86
                                                                
Label dpush   D push    Label hpush   H push                    
Label >next                                                     
   IP ldax   IP inx   A L mov   IP ldax   IP inx   A H mov      
Label >next1                                                    
   M E mov   H inx   M D mov   xchg   pchl                      
end-code                                                        
                                                                
Variable RP                                                     
Variable UP                                                     
\ IP in BC                                                      
\ W  in DE                                                      
\ SP in SP                                                      
Variable IPsave                                                 
                                                                
\ *** Block No. 4 Hexblock 4 
\ Assembler Macros                                       20Oct86
Compiler Assembler also definitions  Forth                      
: Next    T >next jmp [ Forth ]  ;                              
T hpush Forth Constant hpush   T dpush Forth Constant dpush     
T >next Forth Constant >next                                    
                                                                
: rpush ( reg -- )  RP lhld  H dcx  DUP M mov ( high )          
   H dcx   1+ M mov ( low )   RP shld  [ Forth ] ;              
                                                                
: rpop  ( reg -- )  RP lhld   M over 1+ mov ( low )  H inx      
   M swap mov ( high )  H inx   RP shld [ Forth ] ;             
\  rpush und rpop gehen nicht mit HL                            
                                                                
: mvx ( src dest -- )                                           
   2dup  mov ( high )  1+ swap 1+ swap mov  ( low ) [ Forth ] ; 
Target                                                          
\ *** Block No. 5 Hexblock 5 
\ recover ;c: noop                                       20Oct86
                                                                
Create recover   Assembler                                      
   W pop   IP rpush   W IP mvx                                  
Next end-code                                                   
                                                                
Compiler Assembler also definitions   Forth                     
                                                                
: ;c:   0 T recover call  end-code   ]  [ Forth ] ;             
                                                                
Target                                                          
                                                                
| Code di   di Next end-code                                    
| Code ei   ei Next end-code                                    
                                                                
Code noop   >next here 2- !   end-code                          
\ *** Block No. 6 Hexblock 6 
\ User variables                                         04Oct87
                                                                
Constant origin   8 uallot drop  \ Multitasker                  
     \ Felder: entry  link  spare  SPsave                       
     \ Laenge kompatibel zum 68000 und 6502 volksFORTH          
User s0                                                         
User r0                                                         
User dp                                                         
User offset            0 offset !                               
User base              $0A base !                               
User output                                                     
User input                                                      
User errorhandler       \ pointer for Abort" -code              
User voc-link                                                   
User udp                \ points to next free addr in User      
                                                                
\ *** Block No. 7 Hexblock 7 
\ manipulate system pointers                             11Jun86
                                                                
Code sp@  ( -- addr)   0 H lxi   SP dad   hpush jmp   end-code  
                                                                
Code sp!  ( addr --)   H pop   sphl   Next   end-code           
                                                                
                                                                
Code up@  ( -- addr)   UP lhld   hpush jmp   end-code           
                                                                
Code up!  ( addr --)   H pop   UP shld   Next   end-code        
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
\ manipulate returnstack                                 11Jun86
                                                                
Code rp@ ( -- addr )   RP lhld   hpush jmp   end-code           
                                                                
Code rp! ( addr -- )   H pop   RP shld   Next  end-code         
                                                                
                                                                
Code >r  ( 16b -- )    D pop   D rpush   Next end-code restrict 
                                                                
Code r>  ( -- 16b )    D rpop   D push   Next end-code restrict 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
\ r@ rdrop  exit unnest ?exit                            07Oct87
Code r@ ( -- 16b )                                              
   RP lhld   M E mov   H inx   M D mov   D push   Next  end-code
                                                                
Code rdrop                                                      
   RP lhld   H inx   H inx   RP shld   Next end-code   restrict 
                                                                
Code exit    Label >exit   IP rpop   Next  end-code             
Code unnest   >exit  here 2- !                                  
                                                                
Code ?exit ( flag -- )                                          
   H pop   H A mov   L ora   >exit jnz   Next end-code          
                                                                
Code 0=exit ( flag -- )                                         
   H pop   H A mov   L ora   >exit jz    Next end-code          
\ : ?exit ( flag -- )   IF rdrop THEN ;                         
\ *** Block No. 10 Hexblock A 
\ execute  perform                             11Jun86   18Nov87
                                                                
Code execute   ( cfa -- )                                       
   H pop >Next1 jmp end-code                                    
                                                                
Code perform   ( 'cfa -- )                                      
   H pop    M A mov   H inx   M H mov   A L mov  >Next1 jmp     
end-code                                                        
                                                                
                                                                
\\                                                              
: perform   ( addr -- )      @ execute ;                        
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 11 Hexblock B 
\ c@ c! ctoggle                                          07Oct87
                                                                
Code c@   ( addr -- 8b )                                        
   H pop   M L mov   0 H mvi   hpush jmp   end-code             
                                                                
Code c!   ( 16b addr -- )                                       
   H pop   D pop    E M mov   Next   end-code                   
                                                                
Code flip ( 16b1 -- 16b2 )                                      
   H pop   H A mov   L H mov   A L mov   Hpush jmp   end-code   
                                                                
Code ctoggle ( 8b addr -- )                                     
   H pop   D pop   M A mov   E xra   A M mov   Next  end-code   
                                                                
\\                                                              
: ctoggle   ( 8b addr --)      under c@ xor swap c! ;           
\ *** Block No. 12 Hexblock C 
\ @ ! 2@ 2!                                    11Jun86   18Nov87
                                                                
Code @  ( addr -- 16b )   H pop   Label fetch                   
  M E mov   H inx   M D mov   D push   Next   end-code          
                                                                
Code !  ( 16b addr -- )                                         
   H pop   D pop   E M mov   H inx   D M mov   Next   end-code  
                                                                
Code 2@   ( addr -- 32b )   H pop   H push                      
   H inx  H inx   M E mov   H inx   M D mov   H pop   D push    
   M E mov   H inx   M D mov  D push    Next   end-code         
                                                                
Code 2!   ( 32b addr -- )   H pop                               
   D pop    E M mov   H inx   D M mov   H inx                   
   D pop    E M mov   H inx   D M mov   Next   end-code         
                                                                
\ *** Block No. 13 Hexblock D 
\ +! drop swap                                 11Jun86   18Nov87
                                                                
Code +! ( 16b addr -- )  H pop                                  
   Label +store   D pop                                         
     M A mov   E add   A M mov   H inx                          
     M A mov   D adc   A M mov    Next   end-code               
                                                                
\  : +!   ( n addr -- )   under @ + swap ! ;                    
                                                                
                                                                
Code drop   ( 16b -- )   H pop   Next   end-code                
                                                                
Code swap   ( 16b1 16b2 -- 16b2 16b1 )                          
   H pop   xthl   hpush jmp   end-code                          
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
\ dup  ?dup                                              16May86
                                                                
Code dup    ( 16b -- 16b 16b )                                  
   H pop   H push   hpush jmp   end-code                        
                                                                
Code ?dup ( 16b -- 16b 16b / false)                             
   H pop   H A mov   L ora   0<> ?[ H push ]?                   
   hpush jmp   end-code                                         
                                                                
\\                                                              
: ?dup ( 16b -- 16b 16b / false) dup IF dup THEN ;              
                                                                
: dup ( 16b -- 16b 16b )    sp@ @ ;                             
                                                                
                                                                
                                                                
\ *** Block No. 15 Hexblock F 
\ over rot nip under                                     11Jun86
                                                                
Code over   ( 16b1 16b2 - 16b1 16b2 16b1 )                      
   D pop   H pop   H push   dpush jmp   end-code                
Code rot    ( 16b1 16b2 16b3 - 16b2 16b3 16b1 )                 
   D pop   H pop   xthl   dpush jmp   end-code                  
Code nip ( 16b1 16b2 -- 16b2)                                   
   H pop   D pop   hpush jmp   end-code                         
Code under ( 16b1 16b2 -- 16b2 16b1 16b2)                       
   H pop   D pop   H push  dpush jmp   end-code                 
                                                                
\\                                                              
: over   >r swap r> swap ;                                      
: rot   >r dup r> swap ;                                        
: nip   swap drop ;                                             
: under swap over ;                                             
\ *** Block No. 16 Hexblock 10 
\ -rot pick roll -roll                                   11Jun86
Code -rot    ( 16b1 16b2 16b3 -- 16b3 16b1 16b2 )               
   H pop    D pop   xthl   H push   D push   Next   end-code    
                                                                
Code pick    ( n -- 16b.n )                                     
   H pop   H dad   SP dad                                       
   M E mov   H inx   M D mov   D push   Next   end-code         
                                                                
: roll   ( n -- )                                               
   dup >r  pick sp@ dup 2+  r> 1+ 2* cmove> drop ;              
                                                                
: -roll   ( n -- ) >r dup sp@ dup 2+                            
   dup 2+ swap r@ 2* cmove r> 1+ 2* + ! ;                       
\\                                                              
: -rot    ( 16b1 16b2 16b3 -- 16b3 16b1 16b2 )   rot rot ;      
: pick    ( n -- 16b.n )     1+ 2* sp@ + @ ;                    
\ *** Block No. 17 Hexblock 11 
\ double word stack manipulation                         09May86
Code 2swap ( 32b1 32b2 -- 32b2 32b1)                            
   H pop   D pop   xthl   H push                                
   5 H lxi   SP dad   M A mov    D M mov   A D mov              
   H dcx   M A mov   E M mov   A E mov   H pop   dpush jmp      
end-code                                                        
                                                                
Code 2drop ( 32b -- )   H pop   H pop   Next   end-code         
                                                                
Code 2dup ( 32b -- 32b 32b)                                     
   H pop   D pop   D push   H push   dpush jmp   end-code       
                                                                
\\                                                              
: 2swap ( 32b1 32b2 -- 32b2 32b1) rot >r rot r> ;               
: 2drop ( 32b -- ) drop drop ;                                  
: 2dup ( 32b -- 32b 32b) over over ;                            
\ *** Block No. 18 Hexblock 12 
\ + and or xor not                                       09May86
Code +     ( n1 n2 -- n3 )                                      
   H pop   D pop   D dad   hpush jmp end-code                   
Code or    ( 16b1 16b2 -- 16b3 )                                
   H pop   D pop   H A mov  D ora  A H mov                      
   L A mov   E ora   A L mov   hpush jmp end-code               
Code and   ( 16b1 16b2 -- 16b3 )                                
   H pop   D pop   H A mov   D ana   A H mov                    
   L A mov   E ana   A L mov   hpush jmp   end-code             
Code xor   ( 16b1 16b2 -- 16b3 )                                
   H pop   D pop   H A mov   D xra   A H mov                    
   L A mov   E xra   A L mov   hpush jmp   end-code             
Code not   ( 16b1 -- 16b2 )  H pop  Label >not                  
   H A mov   cma    A H mov   L A mov   cma   A L mov           
   hpush jmp   end-code                                         
                                                                
\ *** Block No. 19 Hexblock 13 
\ -  negate                                              16May86
                                                                
Code -    ( n1 n2 -- n3 )                                       
   D pop   H pop                                                
   L A mov   E sub   A L mov                                    
   H A mov   D sbb   A H mov   hpush jmp end-code               
                                                                
Code negate ( n1 -- n2 )                                        
   H pop   H dcx   >not jmp   end-code                          
                                                                
\\                                                              
: -    ( n1 n2 -- n3 )   negate + ;                             
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 20 Hexblock 14 
\ dnegate d+                                   10Mar86   18Nov87
                                                                
Code dnegate   ( d1 -- -d1 )  H pop                             
   Label >dnegate                                               
      D pop   A sub   E sub   A E mov   0 A mvi   D sbb         
      A D mov   0 A mvi   L sbb   A L mov   0 A mvi   H sbb     
      A H mov   dpush jmp   end-code                            
                                                                
Code d+        ( d1 d2 -- d3)                                   
   6 H lxi   SP dad   M E mov   C M mov   H inx                 
   M D mov   B M mov  B pop   H pop   D dad   xchg              
   H pop   L A mov   C adc   A L mov   H A mov   B adc          
   A H mov   B pop   dpush jmp   end-code                       
                                                                
                                                                
                                                                
\ *** Block No. 21 Hexblock 15 
\ 1+ 2+ 3+ 4+ 6+    1- 2- 4-                             27Apr86
Code 1+ ( n1 -- n2 )    H pop   H inx   hpush jmp   end-code    
Code 2+ ( n1 -- n2 )                                            
   H pop    H inx   H inx   hpush jmp   end-code                
Code 3+ ( n1 -- n2 )                                            
   H pop   H inx   H inx   H inx   hpush jmp   end-code         
Code 4+ ( n1 -- n2 )                                            
   H pop   4 D lxi   D dad   hpush jmp   end-code               
| Code  6+ ( n1 -- n2 )                                         
   H pop   6 D lxi   D dad   hpush jmp   end-code               
Code 1- ( n1 -- n2 )    H pop   H dcx   hpush jmp   end-code    
Code 2- ( n1 -- n2 )                                            
   H pop    H dcx   H dcx   hpush jmp   end-code                
Code 4- ( n1 -- n2 )                                            
   H pop   -4 D lxi   D dad   hpush jmp   end-code              
                                                                
\ *** Block No. 22 Hexblock 16 
\ number Constants                                       07Oct87
-1 Constant true      0 Constant false                          
                                                                
 0 ( --  0 )   Constant   0                                     
 1 ( --  1 )   Constant   1                                     
 2 ( --  2 )   Constant   2                                     
 3 ( --  3 )   Constant   3                                     
 4 ( --  4 )   Constant   4                                     
-1 ( -- -1 )   Constant  -1                                     
                                                                
Code on ( addr -- )  H pop   $FF A mvi                          
 Label set   A M mov   H inx   A M mov   Next                   
Code off ( addr -- )   H pop   A xra   set jmp   end-code       
                                                                
\  : on   ( addr -- )   true  swap ! ;                          
\ : off  ( addr -- )   false swap ! ;                           
\ *** Block No. 23 Hexblock 17 
\ words for number literals                              16May86
                                                                
Code lit   ( -- 16b )                                           
   IP ldax   A L mov   IP inx   IP ldax   A H mov   IP inx      
hpush jmp end-code                                              
                                                                
Code clit   ( -- 8b )                                           
   IP ldax   A L mov   0 H mvi   IP inx   hpush jmp             
end-code                                                        
                                                                
: Literal  ( 16b -- )                                           
   dup $FF00 and   IF  compile lit , exit  THEN                 
   compile clit c, ; immediate restrict                         
                                                                
                                                                
                                                                
\ *** Block No. 24 Hexblock 18 
\ comparision words                                      18Nov87
Label  (u<  ( HL,DE  ->  HL u< DE  c,z )                        
   H A mov   D cmp   rnz   L A mov   E cmp   ret                
Label  (<   ( HL,DE  ->  HL  < DE  c,z )                        
   H A mov   D xra   (u< jp    D A mov   H cmp   ret            
                                                                
Label yes  true  H lxi  hpush jmp                               
Code u< ( u1 u2 -- flag )   D pop   H pop                       
   Label uless   (u< call   yes jc                              
   Label no   false H lxi   hpush jmp                           
                                                                
Code <     ( n1 n2 -- flag )   D pop   H pop                    
   Label less    (< call  yes jc  no jmp   end-code             
                                                                
Code u> ( u1 u2 -- flag )  H pop   D pop  uless jmp   end-code  
Code >  ( n1 n2 -- flag )  H pop   D pop   less jmp   end-code  
\ *** Block No. 25 Hexblock 19 
\ comparision words                                      18Nov87
Code 0< ( n1 n2 -- flag )   H pop                               
   Label negative   H dad   yes jc   no jmp   end-code          
                                                                
Code 0> ( n -- flag )   H pop   H A mov   A ora   no jm         
                        L ora   yes jnz   no jmp   end-code     
                                                                
Code 0= ( n -- flag )   H pop                                   
   Label zero=   H A mov   L ora   yes jz    no jmp   end-code  
                                                                
Code 0<>  ( n -- flag )                                         
   H pop   H A mov   L ora   yes jnz   no jmp   end-code        
                                                                
Code =    ( n1 n2 -- flag )  H pop   D pop                      
   L A mov   E cmp   no jnz                                     
   H A mov   D cmp   no jnz   yes jmp   end-code                
\ *** Block No. 26 Hexblock 1A 
\\ comparision words high level                          18Nov87
: 0<  ( n1 -- flag )       8000 and 0<> ;                       
: >   ( n1 n2 -- flag )    swap < ;                             
: 0>  ( n -- flag )        negate 0< ;                          
: 0<> ( n -- flag )        0= not ;                             
: u>  ( u1 u2 -- flag )    swap u< ;                            
: =   ( n1 n2 -- flag )    - 0= ;                               
: uwithin  ( u1 [low up[ -- flag )    over - -rot  - u> ;       
| : minimax  ( n1 n2 flag -- n3 )   rdrop IF swap THEN drop ;   
: min  ( n1 n2 -- n3 )              2dup  > minimax ;           
: max  ( n1 n2 -- n3 )              2dup  < minimax ;           
: umax  ( u1 u2 -- u3 )             2dup u< minimax ;           
: umin  ( u1 u2 -- u3 )             2dup u> minimax ;           
: extend   ( n -- d )               dup 0< ;                    
: dabs  ( d -- ud )                 extend IF dnegate THEN ;    
: abs   ( n -- u)                   extend IF  negate THEN ;    
\ *** Block No. 27 Hexblock 1B 
\ uwthin double number comparison words                  18Nov87
                                                                
Code uwithin ( u1 [low up[ -- flag )  H pop   D pop   xthl      
   (u< call   cs ?[    H pop   no jmp   ]?                      
   D pop   (u< call   yes jc   no jmp   end-code                
                                                                
Code d0= ( d -- flag )  H pop                                   
   H A mov   L ora   H pop   no jnz   zero= jmp  end-code       
                                                                
: d=  ( d1 d2 -- flag )    rot =  -rot =  and ;                 
: d<  ( d1 d2 -- flag )                                         
    rot 2dup =  IF 2drop u< exit THEN  > nip nip ;              
                                                                
                                                                
\\                                                              
: d0= ( d -- flag )        or 0= ;                              
\ *** Block No. 28 Hexblock 1C 
\ minimum maximum                                        18Nov87
                                                                
Code umax ( u1 u2 -- u3 )                                       
   H pop   D pop   (u< call                                     
Label minimax  cs ?[ xchg ]?  hpush jmp   end-code              
                                                                
Code umin ( u1 u2 -- u3 )                                       
   H pop   D pop   (u< call   cmc   minimax jmp   end-code      
                                                                
Code max ( n1 n2 -- n3 )                                        
   H pop   D pop   (< call   minimax jmp   end-code             
                                                                
Code min ( n1 n2 -- n3 )                                        
   H pop   D pop   (< call   cmc   minimax   jmp end-code       
                                                                
                                                                
\ *** Block No. 29 Hexblock 1D 
\ sign extension absolute values                         18Nov87
                                                                
Code extend ( n -- d )  H pop   H push   negative jmp  end-code 
                                                                
Code abs    ( a -- u )  H pop   H A mov  A ora                  
   hpush jp   H dcx  >not jmp  end-code                         
                                                                
Code dabs   ( d -- ud )  H pop   H A mov   A ora                
   hpush jp   >dnegate jmp   end-code                           
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 30 Hexblock 1E 
\ branch ?branch                                         20Nov87
                                                                
Code branch ( -- )  Label >branch                               
   IP H mvx   M E mov   H inx   M D mov   H dcx                 
   D dad   H IP mvx   Next   end-code                           
                                                                
Code ?branch ( fl -- )                                          
   H pop   H A mov   L ora  >branch jz                          
   IP inx   IP inx  Next   end-code                             
                                                                
                                                                
\\                                                              
: branch r> dup @ + >r ;                                        
                                                                
                                                                
                                                                
\ *** Block No. 31 Hexblock 1F 
\ loop primitives                              11Jun86   20Nov87
                                                                
Code bounds ( start count -- limit start )                      
   H pop   D pop   D dad    H push   D push   Next   end-code   
                                                                
Code endloop                                                    
   RP lhld   6 D lxi   D dad   RP shld   next  end-code restrict
                                                                
\\ dodo puts "index | limit | adr.of.DO" on return-stack        
: bounds ( start count -- limit start )     over + swap ;       
                                                                
| : dodo              rdrop r> 2+ dup >r rot >r swap >r >r ;    
                                                                
: (do  ( limit start -- )  over - dodo ;  restrict              
: (?do ( limit start -- )  over - ?dup IF dodo THEN             
                           r> dup  @ +  >r drop ; restrict      
\ *** Block No. 32 Hexblock 20 
\ loop primitives                                        20Nov87
                                                                
Code (do ( limit start -- )  H pop  D pop                       
  Label >do                                                     
    L A mov   E sub   A L mov                                   
    H A mov   D sbb   A H mov                                   
    H push   IP inx   IP inx                                    
    RP lhld   H dcx  IP M mov   H dcx   IP' M mov               
              H dcx   D M mov   H dcx    E  M mov               
    D pop     H dcx   D M mov   H dcx    E  M mov   RP shld     
    Next   end-code   restrict                                  
                                                                
Code (?do ( limit start -- )  H pop  D pop                      
    H A mov   D cmp   >do jnz                                   
    L A mov   E cmp   >do jnz   >branch jmp                     
end-code  restrict                                              
\ *** Block No. 33 Hexblock 21 
\ (loop (+loop                                 14May86   20Nov87
                                                                
Code (loop                                                      
   RP lhld   M inr   0= ?[ H inx   M inr   >next jz   ]?        
Label doloop   RP lhld    4 D lxi  D dad                        
               M IP' mov   H inx   M IP mov   Next              
end-code restrict                                               
                                                                
Code (+loop                                                     
   RP lhld   D pop                                              
   M A mov   E add   A M mov   H inx                            
   M A mov   D adc   A M mov                                    
   rar   D xra   doloop jp   Next                               
end-code restrict                                               
                                                                
                                                                
\ *** Block No. 34 Hexblock 22 
\ loop indices                                 06May86   20Nov87
                                                                
Code I ( -- n )                                                 
   RP lhld                                                      
Label >I     M E mov   H inx   M D mov   D push                 
   H inx     M E mov   H inx   M D mov   H pop   D dad          
   hpush jmp                                                    
end-code                                                        
                                                                
Code J ( -- n )                                                 
   RP lhld   6 D lxi  D dad     >I jmp   end-code               
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 35 Hexblock 23 
\ interpretive conditionals                           UH 25Jan88
                                                                
| Create: remove>>  r> rp! ;                                    
| : >>r ( addr len -- addr ) r>  over rp@  under swap - dup rp! 
     swap >r remove>> >r swap >r  dup >r swap cmove r> ;        
                                                                
| Variable saved-dp     0 saved-dp !                            
                                                                
| Variable level    0 level !                                   
                                                                
| : +level ( -- ) level @ IF 1 level +! exit THEN state @ ?exit 
      1 level !  here saved-dp ! ] ;                            
                                                                
| : -level ( -- ) state @ 0= Abort" unstructured"               
     level @ 0=exit -1 level +!  level @ ?exit  compile unnest  
     [compile] [  saved-dp @  here over dp !  over -  >>r >r ;  
\ *** Block No. 36 Hexblock 24 
\ resolve loops and branches                          UH 25Jan88
                                                                
: >mark     ( -- addr )           here 0 , ;                    
                                                                
: +>mark    ( acf -- addr )       +level , >mark ;              
                                                                
: >resolve  ( addr -- )           here over - swap !  -level ;  
                                                                
: <mark     ( -- addr )           +level  here ;                
                                                                
: <resolve  ( addr -- )           here - , -level ;             
                                                                
: ?pairs    ( n1 n2 -- )          - Abort" unstructured" ;      
                                                                
                                                                
                                                                
\ *** Block No. 37 Hexblock 25 
\ case?                                                  14May86
                                                                
Code case? ( 16b1 16b2 -- 16b1 false / true )                   
   H pop   D pop                                                
   H A mov   D cmp   0= ?[   L A mov   E cmp   yes jz ]?        
   D push   no jmp   end-code                                   
                                                                
\\                                                              
: case? ( 16b1 16b2 -- 16b1 false / true )                      
    over = dup  IF nip THEN ;                                   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 38 Hexblock 26 
\ Branching                                           UH 25Jan88
                                                                
: IF             ['] ?branch +>mark 1 ; immediate               
: THEN           abs 1 ?pairs >resolve ;   immediate            
: ELSE           1 ?pairs  ['] branch +>mark swap               
                 >resolve -1 ;             immediate            
: BEGIN          <mark 2 ;                 immediate            
: WHILE          2 ?pairs 2   ['] ?branch +>mark                
                 -2 2swap ;                immediate            
                                                                
| : (reptil      <resolve                                       
                 BEGIN dup -2 = WHILE drop >resolve REPEAT ;    
                                                                
: REPEAT         2 ?pairs compile  branch (reptil ; immediate   
: UNTIL          2 ?pairs compile ?branch (reptil ; immediate   
                                                                
\ *** Block No. 39 Hexblock 27 
\ Loops                                               UH 25Jan88
                                                                
: DO        ['] (do  +>mark 3 ;       immediate                 
: ?DO       ['] (?do +>mark 3 ;       immediate                 
: LOOP      3 ?pairs compile  (loop compile endloop >resolve ;  
                                         immediate              
: +LOOP     3 ?pairs compile (+loop compile endloop >resolve ;  
                                         immediate              
                                                                
Code LEAVE                                                      
   RP lhld   4 D lxi   D dad   M E mov   H inx   M D mov        
   H inx   RP shld   xchg    H dcx   M D mov   H dcx   M E mov  
   D dad   H IP mvx   Next   end-code   restrict                
                                                                
\\ Returnstack: calladr | index limit | adr of DO               
: LEAVE     endloop r> 2- dup @ + >r ;             restrict     
\ *** Block No. 40 Hexblock 28 
\ um*                                                    16May86
Label (um*   0 H lxi   ( 0=Teil-Produkt )                       
             4 C mvi   ( Schleifen-Zaehler )                    
         [[  H dad ( Schiebe HL 24 bits nach links )            
             ral cs ?[   D dad   0 aci   ]?                     
             H dad   ral   cs ?[   D dad   0 aci   ]?           
      C dcr  0= ?]   ret                                        
                                                                
Code um*      ( u1 u2 -- ud )                                   
   D pop      H pop     B push   H B mov   L A mov   (um* call  
   H push   A H mov   B A mov    H B mov             (um* call  
   D pop    D C mov     B dad      0 aci   L D mov    H L mov   
   A H mov      B pop     dpush jmp end-code                    
                                                                
                                                                
                                                                
\ *** Block No. 41 Hexblock 29 
\ m* * 2* 2/                                             16May86
                                                                
: m*  ( n1 n2 -- d )    dup 0< dup >r IF negate THEN            
                        swap dup 0< IF negate r> not >r THEN    
                        um* r> IF dnegate THEN ;                
                                                                
: *  ( n1 n2 - prod )   um* drop ;                              
                                                                
Code 2*  ( n -- 2*n )   H pop   H dad   hpush jmp   end-code    
                                                                
Code 2/  ( n -- n/2 )                                           
   H pop   H A mov   rlc   rrc   rar   A H mov                  
           L A mov   rar   A L mov   hpush jmp   end-code       
\\                                                              
: 2*  ( n -- 2*n )   2 * ;                                      
: 2/  ( n -- n/2 )   2 / ;                                      
\ *** Block No. 42 Hexblock 2A 
\ um/mod                                                 14May86
Label usl0                                                      
   A E mov   H A mov   C sub   A H mov   E A mov   B sbb        
   cs ?[ H A mov   C add   A H mov   E A mov   D dcr rz         
Label usla                                                      
         H dad   ral   usl0 jnc                                 
         A E mov   H A mov   C sub   A H mov   E A mov   B sbb  
      ]? L inr   D dcr   usla jnz   ret                         
Label usbad     -1 H lxi   B pop   H push   hpush jmp           
Code um/mod   ( d1 n1 -- rem quot )                             
   IP H mvx     B pop   D pop   xthl   xchg                     
   L A mov   C sub   H A mov   B sbb   usbad jnc                
   H A mov   L H mov   D L mov   8 D mvi   D push               
   usla call   D pop   H push   E L mov   usla call             
   A D mov   H E mov   B pop   C H mov   B pop                  
   D push   hpush jmp   end-code                                
\ *** Block No. 43 Hexblock 2B 
\ m/mod                                                  16May86
                                                                
: m/mod ( d n -- mod quot)                                      
   dup >r  abs over 0< IF  under + swap  THEN                   
   um/mod  r@ 0< IF  negate over IF  swap r@ + swap 1-          
   THEN THEN rdrop ;                                            
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 44 Hexblock 2C 
\ /mod / mod */mod */ u/mod  ud/mod                      16May86
                                                                
: /mod   ( n1 n2 -- rem quot )      >r extend r> m/mod ;        
                                                                
: /      ( n1 n2 --     quot )      /mod nip ;                  
                                                                
: mod    ( n1 n2 -- rem )           /mod drop ;                 
                                                                
: */mod  ( n1 n2 n3 -- rem quot )   >r m* r> m/mod ;            
                                                                
: */     ( n1 n2 n3 -- quot )       */mod nip ;                 
                                                                
: u/mod  ( u1 u2 -- urem uquot )    0 swap um/mod ;             
                                                                
: ud/mod ( ud1 u2 -- urem udquot )  >r 0 r@ um/mod r> swap >r   
                                    um/mod r> ;                 
\ *** Block No. 45 Hexblock 2D 
\ cmove cmove>                                 16May86   18Nov87
                                                                
Code cmove ( from to count -- )   IP H mvx   IPsave shld        
     B pop   D pop   H pop                                      
  Label (cmove                                                  
     [[ B A mov   C ora   0= not ?[[                            
        M A mov   H INX   D stax   D inx  B dcx                 
     ]]? IPsave lhld  H IP mvx   Next end-code                  
                                                                
Code cmove>   ( from to count -- )   IP H mvx   IPsave shld     
     B pop   D pop   H pop                                      
  Label (cmove>                                                 
     B dad   H dcx   xchg   B dad   H dcx   xchg                
     [[ B A mov   C ora   0= not ?[[                            
        M A mov   H dcx   D stax   D dcx   B dcx                
     ]]? IPsave lhld  H IP mvx   Next end-code                  
\ *** Block No. 46 Hexblock 2E 
\ move place count                             17Oct86   18Nov87
                                                                
Code move  ( from to quan -- )                                  
      IP H mvx  Ipsave shld   B pop   D pop   H pop             
   Label domove  (u< call   (cmove jnc   (cmove> jmp   end-code 
                                                                
| Code (place ( addr len to -- len to )  IP H mvx  Ipsave shld  
    D pop  B pop  H pop                                         
    B push  D push   D inx   domove jmp   end-code              
                                                                
: place ( addr len to -- )  (place c! ;                         
                                                                
Code count ( adr -- adr+1 len )   H pop   M E mov   0 D mvi     
      H inx    H push   D push   Next  end-code                 
                                                                
                                                                
\ *** Block No. 47 Hexblock 2F 
\ fill erase                                             18Nov87
                                                                
Code fill ( addr quan 8b -- )                                   
   IP H mvx   IPsave shld   D pop   B pop   H pop               
   [[ B A mov   C ora   0<> ?[[                                 
      E M mov   H inx   B dcx                                   
   ]]?  IPsave lhld   H IP mvx   Next   end-code                
                                                                
: erase   ( addr quan --)            0 fill ;                   
                                                                
\\ : fill ( addr quan 8b -- )                                   
   swap ?dup IF >r over c! dup 1+ r> 1- cmove exit THEN 2drop ; 
: count ( adr -- adr+1 len )  dup 1+ swap c@ ;                  
: move   ( from to quan -- )                                    
   >r  2dup u< IF  r> cmove> exit  THEN  r> cmove ;             
: place  ( addr len to --)  over >r  rot over 1+  r> move c! ;  
\ *** Block No. 48 Hexblock 30 
\ here allot , c, pad compile                  11Jun86   18Nov87
                                                                
Code here ( -- addr )  user' dp  D lxi                          
   UP lhld   D dad   fetch jmp   end-code                       
                                                                
Code allot ( n -- )    user' dp  D lxi                          
   UP lhld   D dad   +store jmp  end-code                       
                                                                
: ,      ( 16b -- )  here  ! 2 allot ;                          
: c,     ( 8b -- )   here c! 1 allot ;                          
                                                                
: pad    ( -- addr ) here $42 + ;                               
: compile            r> dup 2+ >r @ , ; restrict                
                                                                
\ : here   ( -- addr ) dp @ ;                                   
\ : allot  ( n -- )    dp +! ;                                  
\ *** Block No. 49 Hexblock 31 
\ input strings                                          11Jun86
                                                                
Variable #tib     0 #tib !                                      
Variable >tib     here >tib ! $50 allot                         
Variable >in      0 >in !                                       
Variable blk      0 blk !                                       
Variable span     0 span !                                      
                                                                
: tib ( -- addr )  >tib @ ;                                     
                                                                
: query ( -- )  tib $50 expect span @ #tib !  >in off blk off ; 
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 50 Hexblock 32 
\\ scan skip /string                           16May86   18Nov87
                                                                
: scan ( addr0 len0 char -- addr1 len1 ) >r                     
   BEGIN dup WHILE  over c@ r@ -  WHILE  1- swap 1+ swap REPEAT 
   rdrop ;                                                      
                                                                
: skip ( addr len del -- addr1 len1 ) >r                        
   BEGIN dup WHILE  over c@ r@ =  WHILE  1- swap 1+ swap REPEAT 
   rdrop ;                                                      
                                                                
: /string ( addr0 len0 +n - addr1 len1 )                        
   over umin rot over + -rot - ;                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 51 Hexblock 33 
\ skip scan                                              18Nov87
Label done    H push   B push   IPsave lhld   H IP mvx   Next   
Code skip   ( addr len del -- addr1 len1 )                      
   IP H mvx   IPsave shld   D pop   B pop   H pop               
   [[ B A mov   C ora   done jz                                 
      M A mov   E cmp   done jnz   H inx   B dcx  ]] end-code   
                                                                
Code scan   ( addr len chr -- addr1 len1 )                      
   IP H mvx   IPsave shld   D pop   B pop   H pop               
   [[ B A mov   C ora   done jz                                 
      M A mov   E cmp   done jz   H inx   B dcx  ]] end-code    
                                                                
Code /string ( addr0 len0 +n - addr1 len1 )   H pop   D pop     
   D push  (u< call  cs ?[ xchg ]?   H pop   xthl   D dad   xthl
   L A mov   E sub   A L mov   H A mov   D sbb   A H mov        
   Hpush jmp   end-code                                         
\ *** Block No. 52 Hexblock 34 
\ capitalize    ohne Umlaute !!                16May86UH 25Jan88
Variable caps  0 caps !                                         
Label ?capital   caps lda   A ana   rz                          
Label (capital ( e --> A,E )   E A mov  Ascii a cpi   rc        
    Ascii z 1+ cpi   rnc   Ascii a Ascii A - sui  A E mov  ret  
                                                                
Code capital ( char -- char')  D pop                            
   (capital call    D push   Next  end-code                     
Code upper ( addr len -- )   D pop   E D mov   H pop   D inr    
   [[ D dcr >next jz  M E mov  (capital call  E M mov  H inx ]] 
end-code                                                        
                                                                
\\ : capital ( char -- char')                                   
   dup  Ascii a   [ Ascii z 1+ ] Literal  uwithin not ?exit     
   [ Ascii a Ascii A - ] Literal - ;                            
: upper ( addr len -- )  bounds   ?DO I c@ capital I c! LOOP ;  
\ *** Block No. 53 Hexblock 35 
\ (word                                                  16May86
                                                                
Code (word ( char adr0 len0 -- addr )                           
   IP H mvx   IPsave shld   B pop   B dcx  D pop                
   >in lhld   D dad   xchg   xthl   xchg   H push   >in lhld    
   C A mov   L sub   A L mov   B A mov   H sbb   A H mov        
   cs ?[ B inx   C A mov   >in sta   B A mov   >in 1+ sta       
                 D pop   H pop   D push                         
   ][ H inx   H B mvx   H pop                                   
      [[  B A mov   C ora   0<>                                 
      ?[[  M A mov   E cmp   0=   ?[[  H inx  B dcx ]]? ]?      
      H push                                                    
      [[  B A mov  C ora   0<>                                  
      ?[[  M A mov   E cmp   0<>   ?[[ H inx  B dcx ]]? ]?      
      xchg    H pop   xthl                                      
      E A mov   L sub   A L mov   D A mov   H sbb   A H mov     
\ *** Block No. 54 Hexblock 36 
\ (word Part2                                            16May86
                                                                
      B A mov  C ora   0<> ?[ H inx ]?   >in shld   ]?          
   H pop   E A mov   L sub   A C mov   D A mov   H sbb   A B mov
   H push   user' dp D lxi   UP lhld   D dad                    
   M A mov   H inx   M H mov  A L mov   D pop   H push          
   C M mov   H inx                                              
   [[ B A mov  C ora 0<>                                        
   ?[[ D ldax  A M mov   H inx  D inx  B dcx ]]?   bl M mvi     
   IPsave lhld   H IP mvx   Next   end-code                     
\\                                                              
: (word  ( char adr0 len0 -- addr )                             
   rot  >r  over swap   >in @ /string                           
   r@ skip   over swap   r> scan >r   rot over swap - r> 0<> -  
   >in !   over - here  dup >r  place  bl r@ count  + c!   r> ; 
                                                                
\ *** Block No. 55 Hexblock 37 
\ source word parse name                       20Oct86UH 25Jan88
                                                                
Variable loadfile                                               
                                                                
: source ( -- addr len )   blk @ ?dup                           
   IF loadfile @ (block  b/blk   exit  THEN  tib #tib @ ;       
                                                                
: word ( char -- addr )   source (word ;                        
                                                                
: parse ( char -- addr len )                                    
   >r  source  >in @ /string  over  swap r>  scan >r            
   over - dup r>  0<> -  >in +! ;                               
                                                                
: name ( -- addr )   bl word  dup count upper  exit ;           
                                                                
                                                                
\ *** Block No. 56 Hexblock 38 
\ state Ascii ," "lit ("  "                              18Nov87
                                                                
Variable state   0 state !                                      
                                                                
: Ascii  ( char -- n )                                          
   bl word  1+ c@  state @ IF [compile] Literal THEN ; immediate
                                                                
Code "lit   RP lhld   M E mov   H inx   M D mov   H dcx         
   D push   D ldax   D inx   E add   A M mov   H inx            
   D A mov   0 aci   A M mov   Next  end-code                   
                                                                
: ,"    Ascii " parse  here over 1+ allot place ;               
: ("    "lit ; restrict                                         
: "     compile (" ," align ; immediate restrict                
                                                                
\  : "lit  r> r> under  count + even  >r >r ;   restrict        
\ *** Block No. 57 Hexblock 39 
\ ." ( .( \ \\ hex decimal                               07Oct87
                                                                
: (."      "lit count type ; restrict                           
: ."       compile (." ," align ; immediate restrict            
                                                                
: (        ascii ) parse 2drop ; immediate                      
: .(       ascii ) parse type ; immediate                       
                                                                
: \        >in @ negate  c/l mod  >in +! ; immediate            
: \\       b/blk >in ! ; immediate                              
: \needs   name find nip 0=exit [compile] \ ;                   
                                                                
: hex      $10 base ! ;                                         
: decimal  $0A base ! ;                                         
                                                                
                                                                
\ *** Block No. 58 Hexblock 3A 
\ number conversion: digit?                    16May86   18Nov87
                                                                
Code digit?    ( char -- n true : false )                       
   user' base D lxi   UP lhld   D dad                           
   D pop   E A mov   Ascii 0 sui   no jc                        
   $0A cpi   cs not ?[ Ascii A Ascii 0 - cpi   no jc            
                      Ascii A Ascii 9 - 1- sui  ]?              
   M cmp   no jnc                                               
   0 H mvi   A L mov   H push   yes jmp   end-code              
                                                                
\\                                                              
: digit? ( char -- digit true/ false )  dup Ascii 9 >           
   IF [ Ascii A Ascii 9 - 1- ] Literal - dup Ascii 9 > and THEN 
   Ascii 0 - dup base @ u< dup ?exit nip ;                      
                                                                
                                                                
\ *** Block No. 59 Hexblock 3B 
\ number conversion:  accumulate  convert                11Jun86
                                                                
| : end?   ( -- flag )                   >in @ 0= ;             
| : char   ( addr0 -- addr1 char )       count -1 >in +! ;      
| : previous   ( addr0 -- addr0 char )   1- count ;             
                                                                
: accumulate ( +d0 adr digit - +d1 adr )                        
   swap >r swap  base @  um* drop rot  base @  um* d+ r> ;      
                                                                
: convert ( +d1 addr0 -- +d2 addr2 )                            
   1+ BEGIN count digit? WHILE accumulate REPEAT 1- ;           
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 60 Hexblock 3C 
\ number conversion: ?nonum punctuation?                 07Oct87
                                                                
| : ?nonum    ( flag -- exit if true ) 0=exit                   
      rdrop 2drop drop rdrop false ;                            
                                                                
| : punctuation?   ( char -- flag )                             
     Ascii , over =  swap  Ascii . =  or ;                      
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 61 Hexblock 3D 
\ number conversion: fixbase?                            07Oct87
                                                                
| : fixbase?  ( char - char false / newbase true )  capital     
   Ascii & case? IF $0A true exit  THEN                         
   Ascii $ case? IF $10 true exit  THEN                         
   Ascii H case? IF $10 true exit  THEN                         
   Ascii % case? IF   2 true exit  THEN     false ;             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 62 Hexblock 3E 
\ number conversion: ?num ?dpl                           07Oct87
                                                                
Variable dpl      -1 dpl !                                      
                                                                
| : ?num      ( flag -- exit if true )  0=exit                  
      rdrop drop r> IF dnegate THEN                             
      rot drop dpl @ 1+ ?dup ?exit  drop true ;                 
                                                                
| : ?dpl     dpl @  -1 =  ?exit  1 dpl +! ;                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 63 Hexblock 3F 
\ number conversion: number?  number                     11Jun86
                                                                
: number?   ( string - string false / n 0< / d 0> )             
   base push  >in push  dup count >in !  dpl on                 
   0 >r ( +sign) 0.0 rot end? ?nonum char                       
   Ascii - case?  IF  rdrop true >r end? ?nonum char  THEN      
   fixbase?       IF  base !        end? ?nonum char  THEN      
   BEGIN digit? 0= ?nonum                                       
     BEGIN accumulate ?dpl end? ?num char digit? 0=  UNTIL      
     previous  punctuation?  0= ?nonum  dpl off  end? ?num  char
   REPEAT ;                                                     
                                                                
: number ( string -- d )                                        
   number? ?dup 0= Abort" ?"  0< IF extend THEN ;               
                                                                
                                                                
\ *** Block No. 64 Hexblock 40 
\ hide reveal immediate restrict                         11Jun86
                                                                
Variable last     0 last !                                      
| : last?   ( -- false / acf true)    last @ ?dup ;             
: hide          last?  IF  2- @ current @ !  THEN ;             
: reveal        last?  IF  2-   current @ !  THEN ;             
: Recursive     reveal ; immediate restrict                     
                                                                
| : flag!    ( 8b --)                                           
      last?  IF  under c@ or over c!  THEN   drop  ;            
                                                                
: immediate     $40 flag! ;                                     
: restrict      $80 flag! ;                                     
                                                                
                                                                
                                                                
\ *** Block No. 65 Hexblock 41 
\ clearstack hallot heap heap?                           04Sep86
                                                                
Code clearstack                                                 
   user' s0 D lxi   UP lhld   D dad   M E mov   H inx   M D mov 
   xchg   sphl   Next   end-code                                
                                                                
: hallot ( quan -- )                                            
   s0 @  over -  swap    sp@ 2+  dup rot -   dup s0 !           
   2 pick  over -    di  move  clearstack  ei   s0 ! ;          
                                                                
: heap    ( -- addr )        s0 @ 6 + ;                         
: heap?   ( addr -- flag )   heap up@ uwithin ;                 
                                                                
| : heapmove   ( from -- from )                                 
     dup  here over -  dup hallot                               
     heap swap cmove   heap over - last +!  reveal ;            
\ *** Block No. 66 Hexblock 42 
\ Does>  ;                                     11Jun86   20Nov87
                                                                
Label (dodoes>                                                  
   IP rpush   IP pop   W inx   W push   Next   end-code         
                                                                
: (;code          r> last @ name> ! ;                           
                                                                
: Does>                                                         
   compile (;code      $CD ( 8080-Call ) c,                     
   compile (dodoes> ; immediate restrict                        
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 67 Hexblock 43 
\ ?head  |  alignments                         20Oct86   18Nov87
                                                                
Variable ?head     0 ?head !                                    
                                                                
: |                ?head @  ?exit  -1 ?head ! ;                 
                                                                
\ machen nichts beim 8080:                                      
: even   ( addr -- addr1 ) ; immediate                          
: align  ( -- )            ; immediate                          
: halign ( -- )            ; immediate                          
                                                                
Variable warning    0 warning !                                 
                                                                
| : exists?         warning @  ?exit  last @ current @          
   (find nip 0=exit  space last @ .name ." exists " ?cr ;       
                                                                
\ *** Block No. 68 Hexblock 44 
\ warning   Create                             20Oct86   18Nov87
                                                                
Defer makeview          ' 0 Is makeview                         
                                                                
: (create ( string -- ) align here                              
    swap count $1F and here 4+ place makeview ,  current @ @ ,  
    here last !  here c@ 1+ allot  align  exists?               
    ?head @ IF  1 ?head +!   dup ,    \ Pointer to Code         
                halign  heapmove  $20 flag!  dup dp !           
            THEN drop  reveal 0 ,                               
    ;Code   W inx   W push   Next   end-code                    
                                                                
: Create   name count 1 $20 uwithin not                         
           Abort" invalid name" 1- (create ;                    
                                                                
                                                                
\ *** Block No. 69 Hexblock 45 
\ nfa?                                                   30Jun86
                                                                
Code nfa? ( thread cfa -- nfa / false )                         
   D pop   H pop                                                
   [[ M A mov   H inx   M H mov   A L mov                       
     H ora   Hpush jz   H push   H inx   H inx   H push   D push
     M A mov   H inx   $1F ani   A E mov   0 D mvi  D dad       
     D pop xthl  M A mov   H pop   $20 ani                      
     0<> ?[ M A mov   H inx   M H mov   A L mov ]?              
     H A mov   D cmp   0= ?[ L A mov  E cmp ]?                  
     H pop   0= ?]   H inx  H inx  Hpush jmp                    
end-code                                                        
\\                                                              
: nfa?    ( thread cfa -- nfa / false)                          
   >r  BEGIN @ dup 0= IF  rdrop exit  THEN  dup 2+ name> r@ =   
       UNTIL 2+ rdrop ;                                         
\ *** Block No. 70 Hexblock 46 
\ >name name> >body .name                      30Jun86   07Oct87
                                                                
: >name   ( cfa -- nfa / false )        voc-link                
   BEGIN @ dup WHILE  2dup 4 - swap nfa?                        
      ?dup IF  -rot 2drop exit  THEN   REPEAT nip ;             
                                                                
Code (name>  ( nfa -- cfa )  H pop   M A mov   H inx   $1F ani  
   A E mov   0 D mvi   D dad   hpush jmp   end-code             
\ : (name>   ( nfa -- cfa )   count  $1F and + ;                
                                                                
: name> ( nfa -- cfa )  dup (name> swap  c@ $20 and  IF @ THEN ;
                                                                
: >body ( cfa -- pfa )  2+ ;    : body> ( pfa -- cfa )  2- ;    
                                                                
: .name   ( nfa -- ) ?dup  IF  dup heap?  IF ." |" THEN         
    count $1F and type  ELSE ." ???"  THEN space ;              
\ *** Block No. 71 Hexblock 47 
\ : ; Constant Variable                                  07Nov87
                                                                
: Create:  Create  hide  current @ context !  0 ] ;             
                                                                
: : Create: ;Code  IP rpush   W inx   W IP mvx   Next end-code  
                                                                
: ;        0 ?pairs   compile unnest   [compile] [   reveal ;   
           immediate restrict                                   
                                                                
: Constant ( n -- )   Create , ;Code                            
   W inx   xchg   M E mov   H inx   M D mov   D push   Next     
   end-code                                                     
                                                                
: Variable         Create 0 , ;                                 
                                                                
                                                                
\ *** Block No. 72 Hexblock 48 
\ uallot User Alias Defer                      11Jun86   18Nov87
: uallot   ( quan -- offset )   even  dup  udp @ +              
    $FF u> Abort" Userarea full" udp @ swap udp +! ;            
                                                                
: User   Create 2 uallot c,                                     
   ;Code W inx   W ldax   A E mov   0 D mvi                     
         UP lhld   D dad   hpush jmp   end-code                 
                                                                
: Alias ( cfa -- )   Create  last  @ dup c@ $20 and             
   IF  -2 allot  ELSE  $20 flag!  THEN (name> ! ;               
                                                                
| : crash           true Abort" crash" ;                        
                                                                
: Defer   Create ['] crash ,                                    
    ;Code W inx   xchg   M E mov   H inx   M D mov              
          xchg   >next1 jmp   end-code                          
\ *** Block No. 73 Hexblock 49 
\ vp current context also toss                           11Jun86
                                                                
Create vp  $10 allot             Variable current               
                                                                
: context   ( -- adr )              vp dup @ + 2+ ;             
                                                                
| : thru.vocstack ( -- from to )    vp 2+ context ;             
\ "Only Forth also Assembler" gives                             
\ vp:  countword = 6 | Only | Forth | Assembler |               
                                                                
: also          vp @ $0A > Error" Vocabulary stack full"        
                context @  2 vp +! context ! ;                  
: toss          vp @ IF  -2 vp +!  THEN  ;                      
                                                                
                                                                
                                                                
\ *** Block No. 74 Hexblock 4A 
\ Vocabulary Forth Only Onlyforth              24Nov85   18Nov87
                                                                
: Vocabulary                                                    
Create   0 , 0 ,  here  voc-link @ ,  voc-link !                
 Does>   context ! ;                                            
\  | Name | Code | Thread | Coldthread | Voc-link |             
                                                                
Vocabulary Forth                                                
Vocabulary Root                                                 
                                                                
: Only   vp off   Root also ;                                   
                                                                
: Onlyforth Only Forth also definitions ;                       
                                                                
                                                                
                                                                
\ *** Block No. 75 Hexblock 4B 
\ definitions order words                      10Oct87   20Nov87
                                                                
| : init-vocabularys        voc-link @                          
     BEGIN  dup 2- @ over 4- ! @ ?dup 0= UNTIL ;                
                                                                
: definitions            context @ current ! ;                  
                                                                
| : .voc   ( adr -- )    @ 2- >name .name ;                     
                                                                
: order    vp 4+ context DO I .voc -2 +LOOP                     
           2 spaces current .voc ;                              
                                                                
: words          context @                                      
   BEGIN   @ dup stop? 0= and                                   
   WHILE   ?cr dup 2+ .name space                               
   REPEAT  drop ;                                               
\ *** Block No. 76 Hexblock 4C 
\ found -text                                            11Jun86
| : found ( nfa -- cfa n )                                      
   dup c@ >r   (name> r@ $20 and  IF  @       THEN              
                   -1 r@ $80 and  IF  1-      THEN              
                      r> $40 and  IF  negate  THEN  ;           
                                                                
\\                                                              
: -text ( adr1 u adr2 -- false:gleich/+1:str1>str2/-1:str1<str2)
    over bounds DO   drop 1+ dup 1- c@ I c@ - dup               
                   IF dup abs / LEAVE THEN LOOP nip ;           
| Variable string        | Variable strlen                      
: (find    ( string thread -- str false/NFA true )              
   >r count $1F and  strlen ! string !                          
   BEGIN r> ?dup WHILE dup @ >r 2+ dup c@ $1F and strlen @ =    
    IF dup 1+ strlen @ string @ -text 0= ?dup IF rdrop exit THEN
      THEN drop   REPEAT  string @ 1- false ;                   
\ *** Block No. 77 Hexblock 4D 
\ (find                                                  11Jun86
                                                                
Code (find   ( str thr - str false/ NFA true )                  
   H pop   D pop   IP push   D ldax   $1F ani   A C mov   D inx 
Label findloop                                                  
   M A mov   H inx   M H mov  A L mov                           
   H A mov   L ora   0= ?[  IP pop  D dcx   D push   no jmp ]?  
   H push   H inx   H inx   M A mov   $1F ani   C cmp           
   0<>   ?[   H pop   findloop jmp   ]?                         
   D push   H inx   C B mov   B inr                             
   [[ B dcr   0<>  ?[[                                          
      D ldax   M cmp   0<>   ?[ D pop  H pop   findloop jmp ]?  
      H inx   D inx   ]]?                                       
   D pop   H pop   H inx   H inx   IP pop   H push   yes jmp    
end-code                                                        
\\  HL: thread, nfa   DE: string   C: strlen   B: counter       
\ *** Block No. 78 Hexblock 4E 
\ find  '  [compile]  [']  nullstring?                   18Nov87
                                                                
: find    ( string -- cfa n / string false )                    
   context dup @ over  2- @ = IF  2-  THEN                      
   BEGIN  under @ (find  IF  nip found  exit  THEN              
     over vp 2+  u> WHILE  swap 2-  REPEAT  nip false ;         
                                                                
: '    ( -- cfa )      name find ?exit Error" ?" ;              
                                                                
: [compile]            ' , ;                 immediate restrict 
                                                                
: [']                  ' [compile] Literal ; immediate restrict 
                                                                
: nullstring?   ( string -- string false / true )               
   dup c@  0= dup 0=exit  nip ;                                 
                                                                
\ *** Block No. 79 Hexblock 4F 
\ notfound                                     17Oct86UH 25Jan88
                                                                
: no.extensions  ( string -- )                                  
    state @ IF Abort" ?" THEN  Error" ?" ;                      
                                                                
Defer notfound                  ' no.extensions Is notfound     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 80 Hexblock 50 
\ interpret interpreter compiler parser               UH 25Jan88
Defer parser                                                    
                                                                
: interpret ( -- )                                              
    BEGIN  ?stack  name  nullstring? ?exit  parser REPEAT ;     
                                                                
| : interpreter ( str -- ) find ?dup                            
     IF  1 and  IF execute  exit THEN Error" compile only" THEN 
     number? ?exit   notfound ;                                 
                                                                
' interpreter Is parser                                         
                                                                
| : compiler ( str -- ) find  ?dup                              
     IF  0> IF  execute exit THEN , exit THEN                   
     number? ?dup IF 0> IF swap [compile] Literal THEN          
                     [compile] Literal exit THEN  notfound ;    
\ *** Block No. 81 Hexblock 51 
\ [ ]                                                 UH 25Jan88
                                                                
: [      ['] interpreter Is Parser  state off ; immediate       
                                                                
: ]      ['] compiler    Is Parser  state on ;                  
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 82 Hexblock 52 
\  Is                                          09May86UH 25Jan88
                                                                
: (is      r> dup 2+ >r @ ! ;                                   
                                                                
| : def?  ( cfa -- )                                            
     @  [ ' notfound @ ] Literal  - Abort" not deferred" ;      
                                                                
: Is   ( adr -- )     ' dup def? >body                          
   state @ IF  compile (is , exit  THEN  ! ; immediate          
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 83 Hexblock 53 
\ ?stack                                                 30Jun86
| : stackfull ( -- )     depth $20 > Abort" tight stack"        
    reveal last? IF dup heap? IF name> ELSE 4- THEN (forget THEN
    true Abort" Dictionary full" ;                              
                                                                
Code ?stack                                                     
   UP lhld   user' dp D lxi   D dad   M E mov   H inx   M D mov 
   0 H lxi  SP dad   L A mov   E sub   H A mov   D sbb          
   0= ?[ ;c: stackfull ; Assembler ]?    H push                 
   UP lhld   user' s0 D lxi   D dad   M E mov   H inx   M D mov 
   H pop    D A mov   H cmp  c0= ?[ 0= ?[ E A mov  L cmp ]? ]?  
   >next jnc  ;c: true abort" Stack empty" ;                    
\\                                                              
: ?stack     sp@ here - 100 u< IF stackfull THEN                
                sp@ s0 @ u> Abort" Stack empty" ;               
                                                                
\ *** Block No. 84 Hexblock 54 
\ .status push load                                      20Oct86
                                                                
Defer .status   ' noop Is .status                               
                                                                
| Create: pull  r> r> ! ;                                       
                                                                
: push   ( addr -- )   r> swap dup >r @ >r pull >r >r ;         
                       restrict                                 
                                                                
: (load  ( blk offset -- )                                      
   isfile push  loadfile push  fromfile push  blk push >in push 
   >in !  blk !  isfile@ loadfile !  .status  interpret ;       
                                                                
: load   ( blk --)     ?dup 0=exit  0 (load ;                   
                                                                
                                                                
\ *** Block No. 85 Hexblock 55 
\ +load thru +thru --> rdepth depth                      20Oct86
                                                                
: +load    ( offset --)       blk @ + load ;                    
                                                                
: thru     ( from to --)      1+ swap DO I  load LOOP ;         
: +thru    ( off0 off1 --)    1+ swap DO I +load LOOP ;         
                                                                
: -->        1 blk +! >in off .status ; immediate               
                                                                
: rdepth   ( -- +n)           r0 @ rp@ 2+   - 2/ ;              
: depth    ( -- +n)           sp@ s0 @ swap - 2/ ;              
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 86 Hexblock 56 
\ quit (quit abort                                    UH 25Jan88
                                                                
: (prompt ( -- )                                                
    state @ IF cr ." ] " ELSE ."  ok" cr THEN .status ;         
                                                                
Defer prompt    ' (prompt Is prompt                             
                                                                
: (quit       BEGIN prompt query interpret REPEAT ;             
                                                                
Defer 'quit     ' (quit Is 'quit                                
: quit   r0 @ rp!  level off  [compile] [ 'quit ;               
                                                                
: standardi/o     [ output ] Literal output 4 cmove ;           
                                                                
Defer 'abort   ' noop Is 'abort                                 
: abort      end-trace clearstack 'abort standardi/o quit ;     
\ *** Block No. 87 Hexblock 57 
\ (error Abort" Error"                         20Oct86   18Nov87
                                                                
Variable scr    1 scr !       Variable r#    0 r# !             
                                                                
: (error ( string -- )     standardi/o space here .name         
    count type space ?cr                                        
    blk @ ?dup IF scr ! >in @ r# ! THEN quit ;                  
' (error errorhandler !                                         
                                                                
: (abort"     "lit swap IF >r clearstack r>                     
              errorhandler perform exit THEN drop ; restrict    
                                                                
| : (err"      "lit swap IF errorhandler perform exit THEN      
               drop ; restrict                                  
: Abort"       compile (abort" ,"  align  ; immediate restrict  
: Error"       compile (err"   ,"  align  ; immediate restrict  
\ *** Block No. 88 Hexblock 58 
\ -trailing                                    30Jun86   18Nov87
                                                                
Code -trailing   ( addr n1 -- addr n2 )                         
   D pop   H pop   H push                                       
   D dad   xchg   D dcx                                         
Label -trail   H A mov   L ora    hpush jz                      
   D ldax   BL cpi   hpush jnz                                  
   H dcx   D dcx   -trail jmp end-code                          
                                                                
\\                                                              
: -trailing ( addr n1 -- addr n2)                               
    2dup bounds ?DO  2dup + 1- c@ bl - IF LEAVE THEN  1-  LOOP ;
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 89 Hexblock 59 
\ space spaces                                           30Jun86
                                                                
$20 Constant bl                                                 
                                                                
: space                      bl emit ;                          
: spaces   ( u --)           0 ?DO space LOOP ;                 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 90 Hexblock 5A 
\ hold <# #> sign # #s                                   17Oct86
                                                                
| : hld   ( -- addr)            pad 2- ;                        
                                                                
: hold    ( char -- )           -1 hld +! hld @ c! ;            
                                                                
: <#                            hld hld ! ;                     
                                                                
: #>      ( 32b -- addr +n )    2drop hld @ hld over - ;        
                                                                
: sign    ( n -- )              0< IF Ascii - hold THEN ;       
                                                                
: #       ( +d1 -- +d2)         base @ ud/mod rot 9 over <      
   IF [ Ascii A Ascii 9 - 1- ] Literal + THEN Ascii 0 + hold ;  
                                                                
: #s      ( +d -- 0 0 )         BEGIN # 2dup d0= UNTIL ;        
\ *** Block No. 91 Hexblock 5B 
\ print numbers                                          24Dec83
                                                                
: d.r      -rot under dabs <# #s rot sign #>                    
           rot over max over - spaces type ;                    
                                                                
: .r       swap extend rot d.r ;                                
                                                                
: u.r      0 swap d.r ;                                         
                                                                
: d.       0 d.r space ;                                        
                                                                
: .        extend d. ;                                          
                                                                
: u.       0 d. ;                                               
                                                                
                                                                
\ *** Block No. 92 Hexblock 5C 
\ .s list c/l l/s                                        05Oct87
                                                                
: .s    sp@ s0 @ over - $20 umin bounds ?DO I @ u. 2 +LOOP ;    
                                                                
$40 Constant c/l        \ Screen line length                    
$10 Constant l/s        \ lines per screen                      
                                                                
: list ( blk -- )                                               
   scr ! ." Scr " scr @ u.                                      
   l/s 0 DO                                                     
     cr I 2 .r space scr @ block I c/l * + c/l -trailing type   
   LOOP cr ;                                                    
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 93 Hexblock 5D 
\ multitasker primitives                                 20Nov87
                                                                
Code end-trace    \ patch Next to its original state            
   $0A A mvi   ( IP ldax )          >next sta                   
   $6F03 H lxi ( IP inx   A L mov ) >next 1+ shld  Next end-code
                                                                
Code pause   >next here 2- !  end-code                          
                                                                
: lock ( addr -- )  dup @  up@  = IF  drop exit  THEN           
   BEGIN  dup  @ WHILE  pause  REPEAT  up@  swap  ! ;           
                                                                
: unlock ( addr -- )   dup lock off ;                           
                                                                
Label wake   H pop   H dcx   UP shld                            
   6 D lxi   D dad   M A mov   H inx   M H mov  A L mov   sphl  
   H pop   RP shld   IP pop   Next   end-code                   
\ *** Block No. 94 Hexblock 5E 
\ buffer mechanism                             20Oct86   07Oct87
                                                                
User isfile          0 isfile !   \ addr of file control block  
Variable fromfile    0 fromfile !                               
Variable prev        0 prev !     \ Listhead                    
| Variable buffers   0 buffers !  \ Semaphor                    
$408 Constant b/buf               \ physikalische Groesse       
$400 Constant b/blk                                             
\\ Struktur eines Buffers:    0 : link                          
                              2 : file                          
                              4 : blocknummer                   
                              6 : statusflags                   
                              8 : Data ... 1 Kb ...             
Statusflag bits : 15   1 -> updated                             
file :  -1 -> empty buffer,  0 -> no fcb, direct access         
        else addr of fcb  ( system dependent )                  
\ *** Block No. 95 Hexblock 5F 
\ search for blocks in memory                            30Jun86
| Variable pred                                                 
\ DE:blk  BC:file  HL:bufadr                                    
                                                                
Label thisbuffer?   ( Zero = this buffer )                      
   H push   H inx   H inx   M A mov   C cmp   0=                
   ?[ H inx   M A mov   B cmp   0= ?[ H inx   M A mov   E cmp   
      0= ?[ H inx   M A mov   D cmp ]? ]? ]?   H pop   ret      
                                                                
Code (core?  ( blk file -- adr\blk file )                       
   IP H mvx   Ipsave shld                                       
   user' offset D lxi   UP lhld   D dad                         
   M E mov   H inx   M D mov                                    
   B pop   H pop   H push   B push   D dad   xchg               
   prev lhld                                                    
   thisbuffer? call 0= ?[                                       
\ *** Block No. 96 Hexblock 60 
\ search for blocks in memory                            30Jun86
                                                                
Label blockfound                                                
   D pop   D pop   8 D lxi   D dad   H push   ' exit @ jmp ]?   
   [[ pred shld                                                 
      M A mov   H inx   M H mov   A L mov                       
      H ora 0= ?[ IPsave lhld   H IP mvx   Next ]?              
      thisbuffer? call   0= ?]                                  
      xchg   pred lhld   D ldax   A M mov                       
      H inx  D inx   D ldax   A M mov   D dcx                   
      prev lhld    xchg    E M mov  H inx  D M mov              
      H dcx   prev shld                                         
   blockfound jmp   end-code                                    
                                                                
                                                                
                                                                
\ *** Block No. 97 Hexblock 61 
\ (core?                                                 29Jun86
\\                                                              
                                                                
| : this? ( blk file bufadr -- flag )                           
     dup 4+ @  swap 2+ @  d= ;                                  
                                                                
| : (core? ( blk file -- dataaddr / blk file )                  
     BEGIN  over offset @ + over prev @ this?                   
        IF  rdrop 2drop prev @ 8 + exit  THEN                   
        2dup >r offset @ + >r prev @                            
        BEGIN dup @ ?dup 0= IF  rdrop rdrop drop exit  THEN     
              dup r> r> 2dup >r >r rot this?  0=                
        WHILE nip REPEAT                                        
        dup @ rot ! prev @ over ! prev ! rdrop rdrop            
     REPEAT ;                                                   
                                                                
\ *** Block No. 98 Hexblock 62 
\ (diskerr                                     29Jul86   07Oct87
                                                                
: (diskerr                                                      
   ." error! r to retry "  key $FF and                          
   capital Ascii R = not Abort" aborted" ;                      
                                                                
Defer diskerr                                                   
' (diskerr Is diskerr                                           
                                                                
Defer r/w                                                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 99 Hexblock 63 
\ backup emptybuf readblk                                20Oct86
                                                                
| : backup ( bufaddr -- )       dup 6+ @ 0<                     
     IF 2+ dup @ 1+         \ buffer empty if file = -1         
       IF input push output push standardi/o                    
         BEGIN  dup 6+ over 2+ @ 2 pick @ 0 r/w                 
         WHILE ." write " diskerr                               
         REPEAT  THEN  4+ dup @ $7FFF and over ! THEN  drop ;   
                                                                
: emptybuf ( bufaddr -- )      2+ dup on 4+ off ;               
                                                                
| : readblk ( blk file addr -- blk file addr )                  
     dup emptybuf                                               
     input push output push standardi/o >r                      
     BEGIN  over offset @ + over r@ 8 + -rot 1 r/w              
     WHILE  ." read " diskerr  REPEAT r>  ;                     
\ *** Block No. 100 Hexblock 64 
\ take mark updates? core?                     10Mar86   19Nov87
                                                                
| : take ( -- bufaddr)    prev                                  
     BEGIN dup @ WHILE @ dup 2+ @ -1 = UNTIL                    
     buffers lock   dup backup ;                                
                                                                
| : mark ( blk file bufaddr -- blk file )                       
     2+ >r  2dup r@ !  offset @ + r@ 2+ !  r> 4+ off            
     buffers unlock ;                                           
                                                                
| : updates? ( -- bufaddr / flag)                               
     prev  BEGIN  @ dup  WHILE dup 6+ @ 0<  UNTIL ;             
                                                                
: core? ( blk file -- addr /false )   (core? 2drop false ;      
                                                                
                                                                
\ *** Block No. 101 Hexblock 65 
\ block & buffer manipulation                  20Oct86   18Nov87
                                                                
: (buffer ( blk file -- addr )                                  
    BEGIN  (core? take mark  REPEAT ;                           
                                                                
: (block ( blk file -- addr )                                   
    BEGIN  (core? take readblk mark  REPEAT ;                   
                                                                
Code isfile@ ( -- addr )  user' isfile  D lxi                   
   UP lhld   D dad   fetch jmp   end-code                       
                                                                
: buffer ( blk -- addr )   isfile@ (buffer ;                    
                                                                
: block  ( blk -- addr )   isfile@ (block ;                     
                                                                
\ : isfile@ ( -- addr )    isfile @ ;                           
\ *** Block No. 102 Hexblock 66 
\ block & buffer manipulation                            05Oct87
                                                                
: update          $80 prev @ 6+ 1+ ( Byte-Order! )  c! ;        
                                                                
Defer save-dos-buffers                                          
                                                                
: save-buffers ( -- )   buffers lock                            
   BEGIN updates? ?dup WHILE backup REPEAT save-dos-buffers     
   buffers unlock ;                                             
                                                                
: empty-buffers ( -- )   buffers lock prev                      
   BEGIN @ ?dup  WHILE dup emptybuf REPEAT  buffers unlock ;    
                                                                
: flush           save-buffers empty-buffers ;                  
                                                                
                                                                
\ *** Block No. 103 Hexblock 67 
\ Allocating buffers                                     10Oct87
$10000 Constant limit            Variable first                 
                                                                
: allotbuffer ( -- )                                            
   first @  r0 @  -  b/buf 2+  u< ?exit                         
   b/buf negate first +!  first @ dup emptybuf                  
   prev @ over !  prev ! ;                                      
                                                                
: freebuffer ( -- )  first @ limit b/buf - u<                   
   IF first @  backup  prev                                     
     BEGIN dup @  first @ -  WHILE  @  REPEAT                   
   first @  @ swap !  b/buf first +!  THEN ;                    
                                                                
: all-buffers  BEGIN  first @ allotbuffer first @ =  UNTIL ;    
                                                                
| : init-buffers    prev off  limit first ! all-buffers ;       
\ *** Block No. 104 Hexblock 68 
\ endpoints of forget                                    01Jul86
                                                                
| : |? ( nfa -- flag )   c@ $20 and ;                           
| : forget? ( adr nfa -- flag )   \ code in heap or above adr ? 
     name>  under  1+ u<  swap  heap?  or ;                     
                                                                
| : endpoints ( addr -- addr symb )                             
     heap voc-link @ >r                                         
     BEGIN   r> @ ?dup      \ through all Vocabs                
     WHILE dup >r 4- >r    \ link on returnstack                
       BEGIN r> @ >r over 1- dup r@ u<      \ until link or     
                   swap r@ 2+ name> u< and  \ code under adr    
       WHILE r@ heap? [ 2dup ] UNTIL   \ search for name in heap
       r@ 2+ |? IF over r@ 2+ forget?                           
               IF r@ 2+ (name> 2+ umax THEN   \ then update symb
       THEN REPEAT rdrop  REPEAT ;                              
\ *** Block No. 105 Hexblock 69 
\ remove, -words, -tasks                                 20Oct86
                                                                
: remove ( dic sym thread - dic sym )                           
     BEGIN dup @ ?dup      \ unlink forg. words                 
     WHILE dup heap?                                            
       IF  2 pick over u>  ELSE  3 pick over 1+ u<  THEN        
       IF  @ over ! ( unlink word)  ELSE nip THEN  REPEAT drop ;
                                                                
| : remove-words ( dic sym -- dic sym )                         
     voc-link BEGIN  @ ?dup                                     
              WHILE  dup >r  4- remove  r> REPEAT ;             
                                                                
| : remove-tasks ( dic -- )       up@                           
     BEGIN  2+  dup @  up@ -  WHILE  2dup @ swap here uwithin   
            IF dup @ 2+ @  over ! 2-                            
            ELSE @ THEN REPEAT 2drop ;                          
\ *** Block No. 106 Hexblock 6A 
\ remove-vocs trim                             20Oct86   07Oct87
                                                                
| : remove-vocs ( dic symb -- dic symb )                        
     voc-link remove       thru.vocstack                        
     DO 2dup I @ -rot uwithin                                   
       IF  [ ' Forth 2+ ]  Literal I !  THEN  -2 +LOOP          
     2dup  current @  -rot  uwithin                             
     IF  [ ' Forth 2+ ] Literal current ! THEN ;                
                                                                
Defer custom-remove     ' noop Is custom-remove                 
                                                                
| : trim ( dic symb -- )                                        
     over  remove-tasks remove-vocs remove-words                
     custom-remove  heap swap - hallot dp ! 0 last ! ;          
                                                                
                                                                
\ *** Block No. 107 Hexblock 6B 
\ deleting words from dict.                    01Jul86   18Nov87
                                                                
: clear        here  dup up@  trim  dp ! ;                      
                                                                
: (forget ( adr --)    dup heap? Abort" is symbol"              
                       endpoints  trim ;                        
                                                                
: forget   ' dup [ dp ] Literal @  u< Abort" protected"         
            >name  dup  heap?                                   
            IF name> ELSE 4- THEN (forget ;                     
                                                                
: empty   [ dp ] Literal @ up@ trim                             
          [ udp ] Literal @ udp ! ;                             
                                                                
                                                                
                                                                
\ *** Block No. 108 Hexblock 6C 
\ save bye stop? ?cr                                     18Nov87
                                                                
: save    here  up@ trim                                        
   voc-link @ BEGIN  dup 4- @ over 2- ! @ ?dup  0= UNTIL        
   up@ origin $100 cmove ;                                      
                                                                
: bye       flush empty (bye ;                                  
                                                                
| : end?    key #cr = IF true rdrop THEN ;                      
                                                                
: stop? ( -- flag )     key? IF end? end? THEN false ;          
                                                                
: ?cr                   col c/l u> 0=exit cr ;                  
                                                                
                                                                
                                                                
\ *** Block No. 109 Hexblock 6D 
\ in/output structure                                    07Jun86
                                                                
| : Out:   Create dup c, 2+ Does> c@ output @ + perform ;       
                                                                
: Output:  Create:  Does> output ! ;                            
0   Out: emit   Out: cr   Out: type   Out: del                  
    Out: page   Out: at   Out: at?    drop                      
                                                                
: row ( -- row)     at? drop ;                                  
: col ( -- col)     at? nip ;                                   
                                                                
| : In:    Create dup c, 2+ Does> c@ input @ + perform ;        
                                                                
: Input:   Create:  Does> input ! ;                             
0   In: key   In: key?   In: decode   In: expect  drop          
                                                                
\ *** Block No. 110 Hexblock 6E 
\ Alias  only definitionen                               18Nov87
                                                                
Root definitions Forth                                          
                                                                
: seal  [ ' Root >body ] Literal off ; \ "erase" Root Vocab.    
                                                                
' Only        Alias Only                                        
' Forth       Alias Forth                                       
' words       Alias words                                       
' also        Alias also                                        
' definitions Alias definitions                                 
                                                                
Host Target                                                     
                                                                
                                                                
                                                                
\ *** Block No. 111 Hexblock 6F 
\ 'restart 'cold                               22Oct86   10Oct87
                                                                
Defer 'restart  ' noop Is 'restart                              
                                                                
| : (restart   ['] (quit Is 'quit drvinit 'restart              
     [ errorhandler ] Literal @ errorhandler !                  
     ['] noop Is 'abort clearstack                              
     standardi/o interpret quit ;                               
                                                                
Defer 'cold    ' noop Is 'cold                                  
                                                                
| : (cold      origin up@ $100 cmove  $80 count                 
     $50 umin >r tib r@ move r> #tib ! >in off  blk off         
     init-vocabularys init-buffers  flush  'cold                
     Onlyforth page &24 spaces  logo count type cr (restart  ;  
                                                                
\ *** Block No. 112 Hexblock 70 
\ cold bootsystem                                        20Oct86
                                                                
Code cold   here >cold !                                        
   s0 lhld   6 D lxi   D dad   origin D lxi   $3F C mvi         
   [[ D ldax   A M mov  H inx   D inx   C dcr   0= ?]           
   ' (cold >body IP lxi                                         
Label bootsystem                                                
   s0 lhld   6 D lxi   D dad   UP shld                          
   user' s0  D lxi   D dad                                      
   M E mov   H inx   M D mov   xchg   sphl                      
   user' r0 D lxi    UP lhld    D dad                           
   M E mov   H inx   M D mov    xchg   RP shld                  
   $C3 ( jmp ) A mvi  $30 sta   wake H lxi  $31 shld ( Tasker ) 
   Next                                                         
end-code                                                        
                                                                
\ *** Block No. 113 Hexblock 71 
\ restart boot                                           20Oct86
                                                                
Code restart      here >restart !                               
   ' (restart >body IP lxi   bootsystem jmp   end-code          
                                                                
Label boot     here >boot !    \ find link to Main:             
   s0 lhld   6 D lxi   D dad   H B mvx    origin D lxi          
   [[ [[ xchg   H inx   H inx   M E mov   H inx   M D mov       
         D A mov   B cmp 0= ?]   E A mov   C cmp 0= ?]   H B mvx
   6 lhld   0 L mvi   ' limit >body shld                        
  -$1100 D lxi   D dad   r0 shld   \ set initial RP             
   -$400 D lxi   D dad   s0 shld   \ set initial SP             
       6 D lxi   D dad   xchg   B H mvx                         
       D M mov   H dcx   E M mov   \ set link to Maintask       
  >cold 2- jmp                                                  
end-code                                                        
\ *** Block No. 114 Hexblock 72 
\ "search                                                05Mar88
                                                                
Label notfound      H pop   H pop                               
   IPsave lhld   H IP mvx   False H lxi   hpush jmp             
                                                                
Code "search ( text tlen buf blen -- addr tf / ff )             
   IP H mvx  IPsave shld   D pop    H pop    xthl               
   H A mov   L ora  notfound jz                                 
   E A mov  L sub  A C mov   D A mov  H sbb  A B mov notfound jc
   B inx   D pop   xthl   M A mov  xthl  H push   xchg          
Label scanfirst                                                 
   A E mov  ?capital call  E D mov                              
   [[  M E mov   H inx   B A mov  C ora   notfound jz   B dcx   
       ?capital call  E A mov  D cmp  0=  ?]                    
   B D mvx   B pop   xchg   xthl   xchg   H push  B push  D push
                                                                
\ *** Block No. 115 Hexblock 73 
\ "search  part 2                                        27Nov87
                                                                
Label match                                                     
   B dcx   B A mov  C ora  0<> ?[                               
   D inx   D ldax   D push   A E mov  ?capital call   E D mov   
   M E mov  H inx  ?capital call  E A mov   D cmp   D pop       
   match jz   H pop   B pop   D pop                             
   M A mov   xthl   B push   H B mvx  xchg  scanfirst jmp ]?    
   D pop   D pop   H pop   D pop  H dcx  H push                 
   IPsave lhld  H IP mvx  True H lxi  hpush jmp                 
end-code                                                        
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 116 Hexblock 74 
\ Rest of Standard-System                      04Oct87   07Oct87
                                                                
2 +load  \ Operating System                                     
                                                                
Host    ' Transient 8 + @  Transient Forth Context @ 6 + !      
                                                                
Target Forth also definitions                                   
                                                                
Vocabulary Assembler  Assembler definitions                     
Transient Assembler                                             
>Next Constant >Next                                            
hpush Constant hpush                                            
dpush Constant dpush                                            
                                                                
Target Forth also definitions                                   
: forth-83 ;     \ last word in Dictionary                      
\ *** Block No. 117 Hexblock 75 
\ System patchup                                         04Oct87
                                                                
$EF00  r0 !                                                     
$EB00  s0 !                                                     
s0 @ 6 +  origin 2+ !  \ link Maintask to itself                
                                                                
\ s0 und r0 werden beim Booten neu an die Speichergroesse       
\ angepasst. Ebenso der Multi-Tasker-Link auf die Maintask      
                                                                
here dp !                                                       
                                                                
Host  Tudp @       Target  udp !                                
Host  Tvoc-link @  Target  voc-link !                           
Host move-threads                                               
                                                                
                                                                
\ *** Block No. 118 Hexblock 76 
\ System dependent Load-Screen                           20Nov87
                                                                
1   +load \ CP/M interface                                      
                                                                
2 4 +thru \ Character IO                                        
                                                                
5 7 +thru \ Default Disk IO                                     
                                                                
8   +load \ Postlude                                            
                                                                
\ 9   +load \ Index                                             
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 119 Hexblock 77 
\ CP/M-Interface                                         05Oct87
Vocabulary Dos    Dos definitions  also                         
Label >bios   pchl                                              
Code biosa ( arg fun -- res )                                   
   1 lhld   D pop   D dcx   D dad   D dad   D dad               
   D pop   IP push   D IP mvx   >bios call                      
Label back                                                      
   IP pop   0 H mvi   A L mov   Hpush jmp   end-code            
                                                                
Code bdosa ( arg fun -- res )                                   
   H pop   D pop   IP push   L C mov   5 call  back jmp         
end-code                                                        
                                                                
: bios ( arg fun -- ) biosa drop ;                              
: bdos ( arg fun -- ) bdosa drop ;                              
                                                                
\ *** Block No. 120 Hexblock 78 
\ Character-IO Constants  Character input                05Oct87
                                                                
Target Dos also                                                 
                                                                
$08 Constant #bs         $0D Constant #cr                       
$0A Constant #lf         $1B Constant #esc                      
$09 Constant #tab        $7F Constant #del                      
$07 Constant #bel        $0C Constant #ff                       
                                                                
: con!   ( c -- )     4 bios ;                                  
: (key?  ( -- ? )   0 2 biosa 0= not ;                          
: getkey ( -- c )   0 3 biosa ;                                 
                                                                
: (key   ( -- c )   BEGIN  pause (key?  UNTIL getkey ;          
                                                                
                                                                
\ *** Block No. 121 Hexblock 79 
\ Character output                           07Oct87  UH 27Feb88
                                                                
| Code ?ctrl ( c -- c' )  H pop   L A mov                       
    $20 cpi  cs ?[ $80 ori ]?   A L mov   Hpush jmp   end-code  
                                                                
: (emit ( c -- )  ?ctrl  con!  pause ;                          
                                                                
: (cr     #cr con!  #lf con! ;                                  
: (del    #bs con!  bl  con!  #bs con! ;                        
: (at? ( -- row col )  0 0 ;                                    
                                                                
: tipp ( addr len -- )   0 ?DO count emit LOOP drop ;           
                                                                
Output: display    [ here output ! ]                            
   (emit (cr tipp (del noop 2drop (at? ;                        
                                                                
\ *** Block No. 122 Hexblock 7A 
\ Line input                                             04Oct87
                                                                
| : backspace ( addr pos1 -- addr pos2 ) dup 0=exit (del 1- ;   
                                                                
: (decode ( addr pos1 key -- addr pos2 )                        
     #bs  case? IF backspace         exit THEN                  
     #del case? IF backspace         exit THEN                  
     #cr  case? IF dup span !  space exit THEN                  
     dup emit >r  2dup +  r> swap c!  1+ ;                      
                                                                
: (expect   ( addr len -- )  span ! 0                           
    BEGIN  span @ over u>  WHILE  key decode  REPEAT  2drop ;   
                                                                
Input:  keyboard    [ here input ! ]                            
     (key  (key?  (decode  (expect ;                            
                                                                
\ *** Block No. 123 Hexblock 7B 
\ Default Disk Interface: Constants and Primitives       18Nov87
                                                                
 $80 Constant b/rec              b/blk b/rec / Constant rec/blk 
                                                                
Dos definitions                                                 
' 2- | Alias dosfcb>         ' 2+ | Alias >dosfcb               
                                                                
: dos-error? ( n -- f )   $FF = ;                               
                                                                
$5C Constant fcb                                                
: reset     (   --     )  0 &13 bdos ;                          
: openfile  ( fcb -- f )    &15 bdosa dos-error? ;              
: closefile ( fcb -- f )    &16 bdosa dos-error? ;              
: dma!      ( dma --   )    &26 bdos  ;                         
: rec@      ( fcb -- f )    &33 bdosa ;                         
: rec!      ( fcb -- f )    &34 bdosa ;                         
\ *** Block No. 124 Hexblock 7C 
\ Default Disk Interface: open and close                 20Nov87
                                                                
Target Dos also     Defer drvinit       Dos definitions         
                                                                
| Variable  opened                                              
: default  ( -- )  opened off                                   
   fcb 1+ c@ bl = ?exit   $80 count here place   #tib off       
   fcb dup  dosfcb> dup  isfile ! fromfile !                    
   openfile Abort" default file not found!"  opened on  ;       
' default Is drvinit                                            
                                                                
: close-default ( -- ) opened @ not ?exit                       
    fcb closefile Abort" can't close default-file!" ;           
' close-default Is save-dos-buffers                             
                                                                
                                                                
\ *** Block No. 125 Hexblock 7D 
\ Default Disk Interface: read/write                     14Feb88
                                                                
Target Dos also                                                 
                                                                
| : rec# ( 'dosfcb -- 'rec# )  &33 + ;                          
                                                                
: (r/w  ( adr blk file r/wf -- flag )  >r                       
    dup 0= Abort" no Direct Disk IO supported! " >dosfcb        
    swap rec/blk *  over rec#   0 over 2+ c!   !                
    r> rot  b/blk bounds                                        
    DO I dma!  2dup IF rec@ drop                                
       ELSE rec! IF 2drop true endloop exit THEN THEN           
       over rec#   0 over 2+ c!  1 swap +!                      
    b/rec +LOOP  2drop false ;                                  
                                                                
' (r/w Is r/w                                                   
\ *** Block No. 126 Hexblock 7E 
\ Postlude                                               20Nov87
                                                                
Defer postlude                                                  
                                                                
| : (bye ( -- )   postlude 0 0 bdos ;                           
                                                                
| : #pages ( -- n ) here $100 -  $100 u/mod swap 0=exit 1+ ;    
                                                                
: .size ( -- ) base push  decimal                               
     cr ." Size: &" #pages u. ." Pages" ;                       
                                                                
' .size Is postlude                                             
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 127 Hexblock 7F 
\  index findex                                          20Nov87
                                                                
| : range  ( from to -- to+1 from )                             
     2dup > IF swap THEN 1+ swap ;                              
                                                                
: index ( from to --)                                           
   range DO  cr I 4 .r I space block  c/l type                  
             stop? IF LEAVE THEN LOOP ;                         
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
