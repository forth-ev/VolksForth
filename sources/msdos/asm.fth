\ *** Block No. 0 Hexblock 0 
\ 8086 Assembler                                     cas 10nov05
This 8086 Assembler was written by Klaus Schleisiek.            
Assembler Definitions are created with the definig word         
CODE and closed with the word END-CODE.                         
                                                                
The 8086 Registers naming and usage in volksFORTH               
                                                                
Intel vForth  Used for                   8bit-Register          
AX      A     free                           A+ A-              
DX      D     topmost Stackitem              D+ D-              
CX      C     free                           C+ C-              
BX      R     Returnstack Pointer            R+ R-              
BP      U     User Pointer                                      
SP      S     Stack Pointer                                     
SI      I     Instruction Pointer                               
DI      W     Word Pointer, mostly free                         
\ *** Block No. 1 Hexblock 1 
\ 8086 Assembler   loadscreen                        cas 10nov05
  Onlyforth                                                     
                                                                
| : u2/ ( 16b -- 15b )   2/ $7FFF and ;                         
| : 8*  ( 15b -- 16b )   2* 2* 2* ;                             
| : 8/  ( 16b -- 13b )   u2/ 2/ 2/ ;                            
                                                                
  Vocabulary Assembler                                          
  Assembler also definitions                                    
                                                                
  3 &21 thru clear  .( Assembler loaded ) cr                    
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ conditional Assembler compiler                     cas 10nov05
  here                                                          
                                                                
  : temp-assembler  ( addr -- )  hide  last off  dp !           
     " ASSEMBLER"  find nip ?exit   here $1800 + sp@ u>         
     IF  display cr ." Assembler won't fit" abort  THEN         
     here   sp@ $1800 - dp !  1 load   dp ! ;                   
                                                                
  temp-assembler   \\                                           
                                                                
  : blocks   ( n -- addr / ff )                                 
     first @ >r   dup 0 ?DO  freebuffer  LOOP                   
     [ b/blk negate ] Literal * first @ +   r@ u> r> and ;      
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\ Code generating primitives                         cas 10nov05
                                                                
  Variable >codes  \ points at table of execution vectors       
                                                                
| Create nrc  ] c, , here ! c! [                                
                                                                
  : nonrelocate  nrc >codes ! ;  nonrelocate                    
                                                                
| : >exec ( n -- n+2 )   Create dup c, 2+                       
    Does> c@  >codes @ + perform ;                              
                                                                
0 | >exec >c,       | >exec >,       | >exec >here              
  | >exec >!        | >exec >c!   drop                          
                                                                
                                                                
                                                                
\ *** Block No. 4 Hexblock 4 
\ 8086 Registers                                     cas 10nov05
                                                                
 0 Constant A    1 Constant C    2 Constant D    3 Constant R   
 4 Constant S    5 Constant U    6 Constant I    7 Constant W   
' I Alias SI    ' W Alias DI    ' R Alias BX                    
                                                                
 8 Constant A-   9 Constant C-  $A Constant D-  $B Constant R-  
$C Constant A+  $D Constant C+  $E Constant D+  $F Constant R+  
' R- Alias B-   ' R+ Alias B+                                   
                                                                
  $100 Constant E:         $101 Constant C:                     
  $102 Constant S:         $103 Constant D:                     
                                                                
| Variable isize        ( specifies Size by prefix)             
| : Size: ( n -- )  Create c,  Does>  c@ isize ! ;              
  0 Size: byte      1 Size: word  word    2 Size: far           
\ *** Block No. 5 Hexblock 5 
\ 8086 Assembler  System variables                   cas 10nov05
                                                                
| Variable direction    \ 0 reg>EA, -1 EA>reg                   
| Variable size         \ 1 word, 0 byte, -1 undefined          
| Variable displaced    \ 1 direct, 0 nothing, -1 displaced     
| Variable displacement                                         
                                                                
| : setsize              isize @  size ! ;                      
| : long?   ( n -- f )   $FF80 and dup 0< not ?exit $FF80 xor ; 
| : wexit                rdrop word ;                           
| : moderr               word true Abort" invalid" ;            
| : ?moderr ( f -- )     0=exit  moderr ;                       
| : ?word                size @ 1- ?moderr ;                    
| : far?    ( -- f )     size @ 2 = ;                           
                                                                
                                                                
\ *** Block No. 6 Hexblock 6 
\ 8086 addressing modes                              cas 10nov05
                                                                
| Create (EA  7 c, 0 c, 6 c, 4 c, 5 c,                          
| : ()  ( 8b1 -- 8b2 )                                          
     3 - dup 4 u> over 1 = or ?moderr (EA + c@ ;                
                                                                
 -1 Constant #       $C6 Constant #)       -1 Constant C*       
                                                                
  : )   ( u1 -- u2 )                                            
     () 6 case? IF  0 $86 exit  THEN  $C0 or ;                  
  : I)  ( u1 u2 -- u3 )  + 9 - dup 3 u> ?moderr $C0 or ;        
                                                                
  : D)  ( n u1 -- n u2 )                                        
     () over long? IF  $40  ELSE  $80  THEN or ;                
  : DI) ( n u1 u2 -- n u3 )                                     
     I) over long? IF  $80  ELSE  $40  THEN xor ;               
\ *** Block No. 7 Hexblock 7 
\ 8086 Registers and addressing modes                cas 10nov05
                                                                
| : displaced?  ( [n] u1 -- [n] u1 f )                          
     dup #) = IF  1 exit  THEN                                  
     dup $C0 and dup $40 = swap $80 = or ;                      
                                                                
| : displace    ( [n] u1 -- u1 )   displaced? ?dup 0=exit       
     displaced @ ?moderr   displaced !   swap displacement !  ; 
                                                                
| : rmode   ( u1 -- u2 )   1 size !  dup 8 and 0=exit           
     size off  $FF07 and ;                                      
                                                                
| : mmode?  ( 9b - 9b f)     dup $C0 and ;                      
                                                                
| : rmode?  ( 8b1 - 8b1 f)   mmode? $C0 = ;                     
                                                                
\ *** Block No. 8 Hexblock 8 
\ 8086  decoding addressing modes                    cas 10nov05
                                                                
| : 2address  ( [n] source [displ] dest -- 15b / [n] 16b )      
     size on   displaced off   dup # = ?moderr   mmode?         
     IF  displace False  ELSE  rmode True  THEN  direction !    
     >r # case?  IF  r> $80C0 xor  size @ 1+ ?exit  setsize exit
                 THEN  direction @                              
     IF  r> 8* >r mmode? IF  displace                           
         ELSE  dup 8/ 1 and  size @ = ?moderr $FF07 and  THEN   
     ELSE  rmode 8*                                             
     THEN  r> or $C0 xor ;                                      
                                                                
| : 1address  ( [displ] 9b -- 9b )                              
     # case? ?moderr   size on   displaced off   direction off  
     mmode? IF  displace setsize  ELSE  rmode  THEN  $C0 xor ;  
                                                                
\ *** Block No. 9 Hexblock 9 
\ 8086 assembler                                     cas 10nov05
| : immediate?   ( u -- u f )  dup 0< ;                         
                                                                
| : nonimmediate ( u -- u )    immediate? ?moderr ;             
                                                                
| : r/m                        7 and ;                          
                                                                
| : reg                        $38 and ;                        
                                                                
| : ?akku  ( u -- u ff / tf )  dup r/m 0= dup 0=exit  nip ;     
                                                                
| : smode? ( u1 -- u1 ff / u2 tf )  dup $F00 and                
     IF  dup $100 and IF  dup r/m 8* swap reg 8/                
                          or $C0 or  direction off              
                      THEN  True exit                           
     THEN  False ;                                              
\ *** Block No. 10 Hexblock A 
\ 8086 Registers and addressing modes                cas 10nov05
                                                                
| : w,          size @ or >c, ;                                 
                                                                
| : dw,         size @ or  direction @ IF  2 xor  THEN  >c, ;   
                                                                
| : ?word,  ( u1 f -- )  IF  >, exit  THEN  >c, ;               
                                                                
| : direct,     displaced @ 0=exit                              
     displacement @ dup long?  displaced @ 1+ or ?word, ;       
                                                                
| : r/m,        >c, direct, ;                                   
                                                                
| : data,       size @ ?word, ;                                 
                                                                
                                                                
\ *** Block No. 11 Hexblock B 
\ 8086 Arithmetic instructions                       cas 10nov05
                                                                
| : Arith: ( code -- )  Create ,                                
  Does> @ >r   2address  immediate?                             
     IF  rmode? IF  ?akku IF  r> size @                         
                              IF  5 or >c, >, wexit  THEN       
                              4 or >c, >c, wexit  THEN THEN     
         r@ or  $80 size @ or   r> 0<                           
         IF  size @ IF  2 pick long? 0= IF  2 or  size off  THEN
         THEN       THEN  >c, >c, direct,  data,  wexit         
     THEN  r> dw, r/m,  wexit ;                                 
                                                                
  $8000 Arith: add      $0008 Arith: or                         
  $8010 Arith: adc      $8018 Arith: sbb                        
  $0020 Arith: and      $8028 Arith: sub                        
  $0030 Arith: xor      $8038 Arith: cmp                        
\ *** Block No. 12 Hexblock C 
\ 8086 move push pop                                 cas 10nov05
                                                                
  : mov    [ Forth ] 2address  immediate?                       
     IF  rmode? IF  r/m $B0 or size @ IF  8 or  THEN            
                    >c, data, wexit                             
                THEN  $C6 w, r/m, data, wexit                   
     THEN  6 case? IF  $A2 dw, direct, wexit  THEN              
     smode? IF  $8C direction @ IF  2 or  THEN  >c, r/m, wexit  
            THEN  $88 dw, r/m, wexit ;                          
                                                                
| : pupo   [ Forth ] >r  1address  ?word                        
     smode? IF  reg 6 r> IF  1+  THEN  or >c, wexit  THEN       
     rmode? IF  r/m $50 or r> or >c, wexit  THEN                
     r> IF  $8F  ELSE  $30 or $FF  THEN  >c, r/m, wexit ;       
                                                                
  : push  0 pupo ;        : pop   8 pupo ;                      
\ *** Block No. 13 Hexblock D 
\ 8086 inc & dec , effective addresses               cas 10nov05
                                                                
| : inc/dec   [ Forth ] >r 1address   rmode?                    
     IF  size @ IF  r/m $40 or r> or >c, wexit  THEN            
     THEN  $FE w, r> or r/m, wexit ;                            
                                                                
  : dec   8 inc/dec ;         : inc   0 inc/dec ;               
                                                                
| : EA:  ( code -- )   Create c, [ Forth ]                      
  Does> >r 2address nonimmediate                                
     rmode? direction @ 0= or ?moderr r> c@ >c, r/m, wexit ;    
                                                                
  $C4 EA: les   $8D EA: lea   $C5 EA: lds                       
                                                                
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
\ 8086  xchg  segment prefix                         cas 10nov05
  : xchg  [ Forth ]  2address nonimmediate rmode?               
   IF  size @ IF  dup r/m 0=                                    
                  IF  8/ true  ELSE  dup $38 and 0=  THEN       
                  IF  r/m $90 or >c, wexit  THEN                
   THEN       THEN  $86 w, r/m, wexit ;                         
                                                                
| : 1addr:  ( code -- )  Create c, [ Forth ]                    
  Does> c@ >r 1address $F6 w, r> or r/m, wexit ;                
                                                                
  $10 1addr: com     $18 1addr: neg                             
  $20 1addr: mul     $28 1addr: imul                            
  $38 1addr: idiv    $30 1addr: div                             
                                                                
  : seg   ( 8b -)    [ Forth ]                                  
     $100 xor dup $FFFC and ?moderr  8* $26 or >c, ;            
\ *** Block No. 15 Hexblock F 
\ 8086  test not neg mul imul div idiv               cas 10nov05
                                                                
  : test   [ Forth ]  2address immediate?                       
     IF  rmode? IF  ?akku IF  $A8 w, data, wexit  THEN THEN     
         $F6 w, r/m, data, wexit                                
     THEN  $84 w, r/m, wexit ;                                  
                                                                
| : in/out  [ Forth ] >r 1address setsize                       
     $C2 case? IF  $EC r> or w, wexit  THEN                     
     6 - ?moderr  $E4 r> or w,  displacement @ >c, wexit ;      
                                                                
  : out  2 in/out ;          : in   0 in/out ;                  
                                                                
  : int    3 case? IF  $CC >c, wexit  THEN  $CD >c, >c, wexit ; 
                                                                
                                                                
\ *** Block No. 16 Hexblock 10 
\ 8086 shifts  and  string instructions              cas 10nov05
                                                                
| : Shifts:  ( code -- )   Create c,  [ Forth ]                 
  Does> c@ >r C* case? >r 1address                              
        r> direction !  $D0 dw, r> or r/m, wexit ;              
                                                                
  $00 Shifts: rol     $08 Shifts: ror                           
  $10 Shifts: rcl     $18 Shifts: rcr                           
  $20 Shifts: shl     $28 Shifts: shr                           
  $38 Shifts: sar     ' shl Alias sal                           
                                                                
| : Str:  ( code -- )   Create c,                               
  Does> c@ setsize w, wexit ;                                   
                                                                
  $A6 Str: cmps      $AC Str: lods     $A4 Str: movs            
  $AE Str: scas      $AA Str: stos                              
\ *** Block No. 17 Hexblock 11 
\ implied 8086 instructions                          cas 10nov05
                                                                
  : Byte:  ( code -- )  Create c,  Does> c@ >c, ;               
  : Word:  ( code -- )  Create ,   Does> @ >, ;                 
                                                                
 $37 Byte: aaa    $AD5 Word: aad    $AD4 Word: aam              
 $3F Byte: aas     $98 Byte: cbw     $F8 Byte: clc              
 $FC Byte: cld     $FA Byte: cli     $F5 Byte: cmc              
 $99 Byte: cwd     $27 Byte: daa     $2F Byte: das              
 $F4 Byte: hlt     $CE Byte: into    $CF Byte: iret             
 $9F Byte: lahf    $F0 Byte: lock    $90 Byte: nop              
 $9D Byte: popf    $9C Byte: pushf   $9E Byte: sahf             
 $F9 Byte: stc     $FD Byte: std     $FB Byte: sti              
 $9B Byte: wait    $D7 Byte: xlat                               
 $C3 Byte: ret     $CB Byte: lret                               
 $F2 Byte: rep     $F2 Byte: 0<>rep  $F3 Byte: 0=rep            
\ *** Block No. 18 Hexblock 12 
\ 8086  jmp call  conditions                         cas 10nov05
| : jmp/call    >r setsize # case?  [ Forth ]                   
     IF  far? IF  r> IF $EA ELSE $9A THEN  >c, swap >, >, wexit 
              THEN  >here 2+ - r>                               
         IF  dup long? 0= IF  $EB >c, >c, wexit  THEN  $E9      
         ELSE  $E8  THEN  >c, 1- >, wexit                       
     THEN  1address $FF >c, $10 or r> +                         
     far? IF  8 or  THEN  r/m, wexit ;                          
  : call    0 jmp/call ;         : jmp   $10 jmp/call ;         
                                                                
  $71 Constant OS    $73 Constant CS                            
  $75 Constant 0=    $77 Constant >=                            
  $79 Constant 0<    $7B Constant PE                            
  $7D Constant <     $7F Constant <=                            
  $E2 Constant C0=   $E0 Constant ?C0=                          
  : not  1 [ Forth ] xor ;                                      
\ *** Block No. 19 Hexblock 13 
\ 8086 conditional branching                         cas 10nov05
                                                                
  : +ret   $C2 >c, >, ;                                         
  : +lret  $CA >c, >, ;                                         
                                                                
| : ?range    dup long? abort" out of range" ;                  
                                                                
  : ?[     >, >here 1- ;                                        
  : ]?     >here over 1+ - ?range swap >c! ;                    
  : ][     $EB ?[ swap ]? ;                                     
  : ?[[    ?[ swap ;                                            
  : [[     >here ;                                              
  : ?]     >c, >here 1+ - ?range >c, ;                          
  : ]]     $EB ?] ;                                             
  : ]]?    ]] ]? ;                                              
                                                                
\ *** Block No. 20 Hexblock 14 
\ Next  user'  end-code  ;c:                         cas 10nov05
                                                                
  : Next  lods  A W xchg  W ) jmp                               
     >here   next-link @ >,   next-link ! ;                     
                                                                
  : u'   ' >body c@ ;                                           
                                                                
  Forth definitions                                             
                                                                
\needs end-code    : end-code   toss also ;                     
                                                                
  Assembler definitions                                         
                                                                
  : ;c:   recover # call last off  end-code  0 ] ;              
                                                                
                                                                
\ *** Block No. 21 Hexblock 15 
\ 8086 Assembler, Forth words                        cas 10nov05
  Onlyforth                                                     
                                                                
  : Assembler     Assembler [ Assembler ] wexit ;               
                                                                
  : ;code   0 ?pairs compile (;code                             
     reveal  [compile] [  Assembler ; immediate                 
                                                                
  : Code    Create [ Assembler ] >here dup 2- >!  Assembler ;   
                                                                
  : >label   ( addr -- )                                        
     here  | Create immediate swap ,  4 hallot                  
     here 4 - heap 4 cmove   heap  last @ (name> !   dp !       
  Does>  ( -- addr )  @  state @ 0=exit  [compile] Literal ;    
                                                                
  : Label   [ Assembler ] >here >label  Assembler ;             
\ *** Block No. 22 Hexblock 16 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
