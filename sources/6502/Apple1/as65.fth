\ *** Block No. 0 Hexblock 0 
\           FORTH-6502 ASSEMBLER   WFR )             cas 26jan06
( BASIS: FORTH DIMENSIONS VOL III NO. 5)                        
                                                                
Load from Screen 1 for the transient assembler:                 
This 6502 Forth Assembler can be loaded into the heap           
and then not be saved in the final binary to save memory.       
                                                                
Load from Screen 2 for the regular assembler:                   
This 6502 Forth Assembler will be loaded into normal            
memory and will be saved into the final binary.                 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 1 Hexblock 1 
\ TRANSIENT FORTH-6502 ASSEMBLER   WFR )               er14dez88
( BASIS: FORTH DIMENSIONS VOL III NO. 5)                        
                                                                
( INTERNAL LOADING         04MAY85BP/RE)                        
hex                                                             
\ HERE   $200 HALLOT  HEAP DP !                                 
      &10  LOAD                                                 
      &11  LOAD                                                 
     3 &8  THRU                                                 
       &9  LOAD        \ for System-Assembler                   
                                                                
\ DP !                                                          
                                                                
ONLYFORTH                                                       
decimal                                                         
                                                                
\ *** Block No. 2 Hexblock 2 
\ FORTH-65 ASSEMBLER               WFR )               er14dez88
( BASIS: FORTH DIMENSIONS VOL III NO. 5)                        
ONLYFORTH                                                       
Vocabulary tassembler                                           
TASSEMBLER ALSO DEFINITIONS                                     
hex                                                             
                                                                
  8  +load      \ relocate                                      
1 6  +THRU                                                      
\ 7  +load      \ System Assembler                              
decimal                                                         
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 3 Hexblock 3 
\ FORTH-83 6502-ASSEMBLER              )               er14dez88
: END-CODE   CONTEXT 2- @  CONTEXT ! ;                          
CREATE INDEX                                                    
09 c, 09 c, 05 c, 15 c, 15 c, 01 c, 11 c, 80 c,                 
09 c, 80 c, 0D c, 1D c, 19 c, 80 c, 80 c, 80 c,                 
80 c, 00 c, 04 c, 14 c, 14 c, 80 c, 80 c, 80 c,                 
80 c, 80 c, 0C c, 1C c, 1C c, 80 c, 80 c, 2C c,                 
                                                                
| VARIABLE MODE                                                 
                                                                
: MODE:  ( N -)   CREATE C,  DOES>  ( -)     C@ MODE ! ;        
                                                                
0   MODE: .A     1    MODE: #   2 | MODE: MEM    3    MODE: ,X  
4   MODE: ,Y     5    MODE: X)  6   MODE: )Y    0F    MODE: )   
6   MODE: )Y    0F    MODE: )   6   MODE: )Y    0F    MODE: )   
6   MODE: )Y    0F    MODE: )   6   MODE: )Y    0F    MODE: )   
\ *** Block No. 4 Hexblock 4 
\ UPMODE  CPU )                                        er14dez88
| : UPMODE ( ADDR0 F0 - ADDR1 F1)                               
 IF MODE @  8 OR MODE !   THEN  1 MODE @  0F AND ?DUP IF        
 0 DO  DUP +  LOOP THEN   OVER 1+ @ AND 0= ;                    
                                                                
: CPU  ( 8B -)   CREATE  C, DOES>  ( -)    C@ >c, MEM ;         
                                                                
 00 CPU BRK  18 CPU CLC  D8 CPU CLD                             
 58 CPU CLI  B8 CPU CLV  CA CPU DEX                             
 88 CPU DEY  E8 CPU INX  C8 CPU INY                             
 EA CPU NOP  48 CPU PHA  08 CPU PHP                             
 68 CPU PLA  28 CPU PLP  40 CPU RTI                             
 60 CPU RTS  38 CPU SEC  F8 CPU SED                             
 78 CPU SEI  AA CPU TAX  A8 CPU TAY                             
 BA CPU TSX  8A CPU TXA  9A CPU TXS                             
 98 CPU TYA                                                     
\ *** Block No. 5 Hexblock 5 
\ M/CPU                                )               er14dez88
                                                                
: M/CPU  ( MODE OPCODE -)  CREATE C, , DOES>                    
 DUP 1+ @ 80 AND IF 10 MODE +! THEN OVER FF00 AND UPMODE UPMODE 
 IF MEM TRUE ABORT" INVALID" THEN                               
 C@ MODE @ INDEX + C@ + >c, MODE @ 7 AND                        
 IF MODE @  0F AND 7 < IF >c, ELSE >, THEN THEN MEM ;           
                                                                
 1C6E 60 M/CPU ADC   1C6E 20 M/CPU AND    1C6E C0 M/CPU CMP     
 1C6E 40 M/CPU EOR   1C6E A0 M/CPU LDA    1C6E 00 M/CPU ORA     
 1C6E E0 M/CPU SBC   1C6C 80 M/CPU STA    0D0D 01 M/CPU ASL     
 0C0C C1 M/CPU DEC   0C0C E1 M/CPU INC    0D0D 41 M/CPU LSR     
 0D0D 21 M/CPU ROL   0D0D 61 M/CPU ROR    0414 81 M/CPU STX     
 0486 E0 M/CPU CPX   0486 C0 M/CPU CPY    1496 A2 M/CPU LDX     
 0C8E A0 M/CPU LDY   048C 80 M/CPU STY    0480 14 M/CPU JSR     
 8480 40 M/CPU JMP   0484 20 M/CPU BIT                          
\ *** Block No. 6 Hexblock 6 
\ ASSEMBLER CONDITIONALS               )               er14dez88
                                                                
| : RANGE?   ( BRANCH -- BRANCH )                               
     DUP ABS  07F U> ABORT" OUT OF RANGE " ;                    
                                                                
: [[  ( BEGIN)  >here ;                                         
: ?]  ( UNTIL)  >c, >here 1+ - RANGE? >c, ;                     
: ?[  ( IF)     >c,  >here 0 >c, ;                              
: ?[[ ( WHILE)  ?[ SWAP ;                                       
: ]?  ( THEN)   >here OVER >c@  IF SWAP >!                      
      ELSE OVER 1+ - RANGE? SWAP >c! THEN ;                     
: ][  ( ELSE)   >here 1+   1 JMP                                
    SWAP >here OVER 1+ - RANGE?  SWAP >c! ;                     
: ]]  ( AGAIN)  JMP ;                                           
: ]]? ( REPEAT) JMP ]? ;                                        
                                                                
\ *** Block No. 7 Hexblock 7 
\ ASSEMBLER CONDITIONALS               )               er14dez88
                                                                
90 CONSTANT CS    B0 CONSTANT CC                                
D0 CONSTANT 0=    F0 CONSTANT 0<>                               
10 CONSTANT 0<    30 CONSTANT 0>=                               
50 CONSTANT VS    70 CONSTANT VC                                
                                                                
: NOT    20 [ FORTH ] XOR ;                                     
                                                                
: BEQ    0<> ?] ;   : BMI   0>= ?] ;                            
: BNE    0=  ?] ;   : BPL   0<  ?] ;                            
: BCC    CS  ?] ;   : BVC   VS  ?] ;                            
: BCS    CC  ?] ;   : BVS   VC  ?] ;                            
                                                                
                                                                
                                                                
\ *** Block No. 8 Hexblock 8 
\ 2INC/2DEC   WINC/WDEC   KS 19 MAY 84 )               er14dez88
                                                                
: 2INC                                                          
 DUP LDA  CLC  2 # ADC DUP STA  CS ?[  SWAP 1+ INC  ]?  ;       
                                                                
: 2DEC                                                          
 DUP LDA  SEC  2 # SBC DUP STA  CC ?[  SWAP 1+ DEC  ]?  ;       
                                                                
: WINC  DUP INC  0= ?[  SWAP 1+ INC  ]?  ;                      
                                                                
: WDEC  DUP LDA  0= ?[  OVER 1+ DEC  ]?  DEC  ;                 
                                                                
: ;C:   RECOVER JSR  END-CODE ]  0 LAST !  0 ;                  
                                                                
                                                                
                                                                
\ *** Block No. 9 Hexblock 9 
\ ;CODE CODE CODE>          BP 03 02 85)               er14dez88
ONLYFORTH                                                       
                                                                
: ASSEMBLER ASSEMBLER   [ ASSEMBLER ] MEM ;                     
                                                                
: ;CODE [COMPILE] DOES>  -3 >allot                              
    [COMPILE] ;   -2 >allot   ASSEMBLER ; IMMEDIATE             
                                                                
: CODE  CREATE >here DUP 2- >! ASSEMBLER ;                      
                                                                
: >LABEL  ( ADR -)                                              
    >here | CREATE  SWAP ,    4 HALLOT                          
    HEAP 1 AND HALLOT ( 6502-ALIGN) HERE 4 - HEAP  4  CMOVE     
    HEAP LAST @ COUNT 01F AND + !  DP ! DOES>  ( - ADR)   @  ;  
                                                                
: LABEL [ ASSEMBLER ]  >here >LABEL ASSEMBLER ;                 
\ *** Block No. 10 Hexblock A 
\ Code generating primitives                           er14dez88
                                                                
Variable >codes                                                 
| Create nrc ] c, , c@ here allot ! c! [                        
                                                                
: nonrelocate   nrc >codes ! ;      nonrelocate                 
                                                                
| : >exec   Create  c,                                          
            Does>  c@  >codes @  +  @  execute ;                
                                                                
|   0 >exec >c,       |  2 >exec >,       |   4 >exec >c@       
|   6 >exec >here     |  8 >exec >allot   | $0A >exec >!        
| $0C >exec >c!                                                 
                                                                
                                                                
                                                                
\ *** Block No. 11 Hexblock B 
\ FORTH-65 ASSEMBLER               WFR )               er14dez88
( BASIS: FORTH DIMENSIONS VOL III NO. 5)                        
ONLYFORTH                                                       
                                                                
ASSEMBLER ALSO DEFINITIONS                                      
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
