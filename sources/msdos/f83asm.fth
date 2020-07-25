\ *** Block No. 0 Hexblock 0 
\ 8086 Assembler                                     cas 10nov05
                                                                
The 8086 Assembler was written by Mike Perry.                   
To create and assembler language definition, use the defining   
word CODE.  It must be terminated with either END-CODE or       
its synonym C;.  How the assembler operates is a very           
interesting example of the power of CREATE DOES>   Basically    
the instructions are categorized and a defining word is         
created for each category.  When the nmemonic for the           
instruction is interpreted, it compiles itself.                 
                                                                
Adapted for volksFORTH by Klaus Schleisiek                      
                                                                
No really tested, but                                           
   CODE TEST  TOS PUSH   1 # TOS MOV   NEXT   END-CODE          
works!                                                          
\ *** Block No. 1 Hexblock 1 
\ 8086 Assembler                                  ks cas 10nov05
Onlyforth                                                       
Vocabulary Assembler                                            
: octal   8 Base ! ;                                            
                                                                
decimal  1 14 +THRU  clear                                      
                                                                
Onlyforth                                                       
                                                                
  : Code    Create [ Assembler ] here dup 2- !  Assembler ;     
                                                                
CR .( 8086 Assembler loaded )                                   
Onlyforth                                                       
                                                                
                                                                
                                                                
\ *** Block No. 2 Hexblock 2 
\ 8086 Assembler                                  ks 19 m„r 88  
: LABEL   CREATE ASSEMBLER   ;                                  
\ 232 CONSTANT DOES-OP                                          
\ 3 CONSTANT DOES-SIZE                                          
\ : DOES?   ( IP -- IP' F )                                     
\    DUP DOES-SIZE + SWAP C@ DOES-OP =  ;                       
ASSEMBLER ALSO DEFINITIONS                                      
: C;   ( -- )   END-CODE   ;                                    
OCTAL                                                           
DEFER C,         FORTH ' C,       ASSEMBLER IS C,               
DEFER ,          FORTH ' ,        ASSEMBLER IS ,                
DEFER HERE       FORTH ' HERE     ASSEMBLER IS HERE             
DEFER ?>MARK                                                    
DEFER ?>RESOLVE                                                 
DEFER ?<MARK                                                    
DEFER ?<RESOLVE                                                 
\ *** Block No. 3 Hexblock 3 
\ 8086 Assembler   Register Definitions           ks 19 m„r 88  
| : REG    11 * SWAP 1000 * OR CONSTANT   ;                     
| : REGS   ( MODE N -- )   SWAP 0 DO  DUP I REG  LOOP  DROP ;   
                                                                
10 0 REGS   AL  CL  DL  BL  AH  CH  DH  BH                      
10 1 REGS   AX  CX  DX  BX  SP  BP  SI  DI                      
10 2 REGS   [BX+SI] [BX+DI] [BP+SI] [BP+DI] [SI] [DI] [BP] [BX] 
 4 2 REGS   [SI+BX] [DI+BX] [SI+BP] [DI+BP]                     
 4 3 REGS   ES  CS  SS  DS                                      
 3 4 REGS   #   #)  S#)                                         
                                                                
BP Constant UP   [BP] Constant [UP]   \ User Pointer            
SI CONSTANT IP   [SI] CONSTANT [IP]   ( INTERPRETER POINTER )   
DI Constant W    [DI] Constant [W]    \ WORKING REGISTER        
BX Constant RP   [BX] Constant [RP]   \ Return Stack Pointer    
DX Constant TOS                       \ Top Of Stack im Register
\ *** Block No. 4 Hexblock 4 
\ Addressing Modes                                ks 19 m„r 88  
| : MD   CREATE  1000 * ,  DOES>  @ SWAP 7000 AND = 0<>  ;      
| 0 MD R8?   | 1 MD R16?   | 2 MD MEM?   | 3 MD SEG?  | 4 MD #? 
| : REG?   ( n -- f )   7000 AND 2000 < 0<> ;                   
| : BIG?   ( N -- F )   ABS -200 AND 0<>  ;                     
| : RLOW   ( n1 -- n2 )    7 AND ;                              
| : RMID   ( n1 -- n2 )   70 AND ;                              
| VARIABLE SIZE   SIZE ON                                       
: BYTE   ( -- )   SIZE OFF ;                                    
| : OP,   ( N OP -- )   OR C,  ;                                
| : W,   ( OP MR -- )   R16? 1 AND OP,  ;                       
| : SIZE,   ( OP -- OP' )   SIZE @ 1 AND OP,  ;                 
| : ,/C,  ( n f -- )   IF  ,  ELSE  C,  THEN  ;                 
| : RR,   ( MR1 MR2 -- )   RMID SWAP RLOW OR 300 OP,  ;         
| VARIABLE LOGICAL                                              
| : B/L?   ( n -- f )   BIG? LOGICAL @ OR  ;                    
\ *** Block No. 5 Hexblock 5 
\ Addressing                                      ks 19 m„r 88  
| : MEM,   ( DISP MR RMID -- )   OVER #) =                      
     IF  RMID 6 OP, DROP ,                                      
     ELSE  RMID OVER RLOW OR -ROT [BP] = OVER 0= AND            
     IF  SWAP 100 OP, C,  ELSE  SWAP OVER BIG?                  
     IF  200 OP, ,  ELSE  OVER 0=                               
     IF  C, DROP  ELSE  100 OP, C,                              
     THEN THEN THEN THEN  ;                                     
| : WMEM,   ( DISP MEM REG OP -- )   OVER W, MEM,  ;            
| : R/M,   ( MR REG -- )                                        
     OVER REG? IF  RR,  ELSE  MEM,  THEN  ;                     
| : WR/SM,   ( R/M R OP -- )   2 PICK DUP REG?                  
     IF  W, RR,  ELSE  DROP SIZE, MEM,  THEN  SIZE ON  ;        
| VARIABLE INTER                                                
: FAR    ( -- )   INTER ON  ;                                   
| : ?FAR   ( n1 -- n2 )   INTER @ IF  10 OR  THEN  INTER OFF ;  
\ *** Block No. 6 Hexblock 6 
\ Defining Words to Generate Op Codes             ks 19 m„r 88  
| : 1MI   CREATE  C,  DOES>  C@ C,  ;                           
| : 2MI   CREATE  C,  DOES>  C@ C,  12 C,  ;                    
| : 3MI   CREATE  C,  DOES>  C@ C,  HERE - 1-                   
     DUP -200 177 uWITHIN NOT ABORT" Branch out of Range" C, ;  
| : 4MI   CREATE  C,  DOES>  C@ C,  MEM,  ;                     
| : 5MI   CREATE  C,  DOES>  C@ SIZE,  SIZE ON ;                
| : 6MI   CREATE  C,  DOES>  C@ SWAP W,  ;                      
| : 7MI   CREATE  C,  DOES>  C@ 366 WR/SM, ;                    
| : 8MI   CREATE  C,  DOES>  C@ SWAP R16? 1 AND OR  SWAP # =    
     IF  C, C,  ELSE  10 OR  C,  THEN  ;                        
| : 9MI   CREATE  C,  DOES>  C@  OVER R16?                      
     IF  100 OR SWAP RLOW OP,  ELSE  376 WR/SM,  THEN  ;        
| : 10MI  CREATE  C,  DOES>  C@ OVER CL =                       
     IF  NIP 322  ELSE  320  THEN  WR/SM,  ;                    
                                                                
\ *** Block No. 7 Hexblock 7 
\ Defining Words to Generate Op Codes             ks 19 m„r 88  
| : 11MI   CREATE  C, C,  DOES>  OVER #) =                      
   IF  NIP C@ INTER @                                           
     IF  1 AND IF  352  ELSE  232  THEN  C,  SWAP , ,  INTER OFF
     ELSE  SWAP HERE - 2- SWAP  2DUP 1 AND SWAP BIG? NOT AND    
       IF  2 OP, C,  ELSE  C,  1- ,  THEN  THEN                 
   ELSE  OVER S#) = IF  NIP #) SWAP  THEN                       
     377 C, 1+ C@ ?FAR  R/M,  THEN  ;                           
| : 12MI   CREATE  C, C, C,  DOES>  OVER REG?                   
   IF  C@ SWAP RLOW OP,  ELSE  1+ OVER SEG?                     
     IF  C@ RLOW SWAP RMID OP,                                  
     ELSE  COUNT SWAP C@ C,  MEM,                               
   THEN THEN  ;                                                 
| : 14MI   CREATE  C,  DOES> C@                                 
   DUP ?FAR C,  1 AND 0= IF  ,  THEN ;                          
                                                                
\ *** Block No. 8 Hexblock 8 
\ Defining Words to Generate Op Codes             ks 19 m„r 88  
| : 13MI   CREATE  C, C,  DOES>  COUNT >R C@ LOGICAL !  DUP REG?
   IF  OVER REG?                                                
     IF  R> OVER W, SWAP RR,  ELSE  OVER DUP MEM? SWAP #) = OR  
     IF  R> 2 OR WMEM,  ELSE  ( # ) NIP  DUP RLOW 0= ( ACC? )   
     IF  R> 4 OR OVER W, R16? ,/C,                              
     ELSE  OVER B/L? OVER R16? 2DUP AND                         
       -ROT 1 AND SWAP NOT 2 AND OR 200 OP,                     
       SWAP RLOW 300 OR R> OP,  ,/C,                            
     THEN  THEN  THEN                                           
   ELSE  ( MEM )  ROT DUP REG?                                  
     IF  R> WMEM,                                               
     ELSE  ( # ) DROP  2 PICK B/L? DUP NOT 2 AND 200 OR SIZE,   
       -ROT R> MEM,  SIZE @ AND ,/C,  SIZE ON                   
   THEN  THEN  ;                                                
                                                                
\ *** Block No. 9 Hexblock 9 
\ Instructions                                    ks 19 m„r 88  
: TEST   ( source dest -- )   DUP REG?                          
   IF  OVER REG?                                                
     IF  204 OVER W, SWAP RR,  ELSE  OVER DUP MEM? SWAP #) = OR 
     IF  204 WMEM,  ELSE  ( # ) NIP  DUP RLOW 0= ( ACC? )       
     IF  250 OVER W,                                            
     ELSE  366 OVER W,  DUP RLOW 300 OP,                        
     THEN   R16? ,/C,  THEN  THEN                               
   ELSE  ( MEM )  ROT DUP REG?                                  
     IF  204 WMEM,                                              
     ELSE  ( # ) DROP  366 SIZE,  0 MEM,  SIZE @ ,/C,  SIZE ON  
   THEN  THEN  ;                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 10 Hexblock A 
\ Instructions                                    ks 19 m„r 88  
HEX                                                             
: ESC   ( source ext-opcode -- )   RLOW 0D8 OP, R/M,  ;         
: INT   ( N -- )   0CD C,  C,  ;                                
: SEG   ( SEG -- )   RMID 26 OP,  ;                             
: XCHG   ( MR1 MR2 -- )   DUP REG?                              
   IF  DUP AX =                                                 
     IF  DROP RLOW 90 OP,  ELSE  OVER AX =                      
     IF  NIP  RLOW 90 OP,  ELSE  86 WR/SM,  THEN  THEN          
   ELSE  ROT 86 WR/SM,  THEN  ;                                 
                                                                
: CS:   CS SEG ;                                                
: DS:   DS SEG ;                                                
: ES:   ES SEG ;                                                
: SS:   SS SEG ;                                                
                                                                
\ *** Block No. 11 Hexblock B 
\ Instructions                                    ks 19 m„r 88  
: MOV   ( S D -- )   DUP SEG?                                   
   IF  8E C, R/M,  ELSE  DUP REG?                               
     IF  OVER #) = OVER RLOW 0= AND                             
       IF  A0 SWAP W,   DROP   ,  ELSE  OVER SEG?               
       IF  SWAP 8C C, RR,  ELSE  OVER # =                       
       IF  NIP DUP R16? SWAP RLOW OVER 8 AND OR B0 OP, ,/C,     
       ELSE  8A OVER W, R/M,  THEN THEN THEN                    
     ELSE  ( MEM ) ROT DUP SEG?                                 
       IF  8C C, MEM,  ELSE  DUP # =                            
       IF  DROP C6 SIZE, 0 MEM,  SIZE @ ,/C,                    
       ELSE  OVER #) = OVER RLOW 0= AND                         
       IF  A2 SWAP W,  DROP   ,   ELSE  88 OVER W, R/M,         
   THEN THEN THEN THEN THEN   SIZE ON  ;                        
                                                                
                                                                
\ *** Block No. 12 Hexblock C 
\ Instructions                                        12Oct83map
 37  1MI AAA     D5  2MI AAD     D4  2MI AAM     3F  1MI AAS    
0 10 13MI ADC  0 00 13MI ADD   2 20 13MI AND  10 E8 11MI CALL   
 98  1MI CBW     F8  1MI CLC     FC  1MI CLD     FA  1MI CLI    
 F5  1MI CMC   0 38 13MI CMP     A6  5MI CMPS    99  1MI CWD    
 27  1MI DAA     2F  1MI DAS     08  9MI DEC     30  7MI DIV    
       ( ESC )   F4  1MI HLT     38  7MI IDIV    28  7MI IMUL   
 E4  8MI IN      00  9MI INC           ( INT )  0CE  1MI INTO   
0CF  1MI IRET    77  3MI JA      73  3MI JAE     72  3MI JB     
 76  3MI JBE     E3  3MI JCXZ    74  3MI JE      7F  3MI JG     
 7D  3MI JGE     7C  3MI JL      7E  3MI JLE  20 E9 11MI JMP    
 75  3MI JNE     71  3MI JNO     79  3MI JNS     70  3MI JO     
 7A  3MI JPE     7B  3MI JPO     78  3MI JS      9F  1MI LAHF   
 C5  4MI LDS     8D  4MI LEA     C4  4MI LES     F0  1MI LOCK   
0AC  6MI LODS    E2  3MI LOOP    E1  3MI LOOPE   E0  3MI LOOPNE 
                                                                
\ *** Block No. 13 Hexblock D 
\ Instructions                                        12Apr84map
       ( MOV )   0A4  5MI MOVS    20  7MI MUL     18  7MI NEG   
 90  1MI NOP      10  7MI NOT   2 08 13MI OR      E6  8MI OUT   
            8F 07 58 12MI POP     9D  1MI POPF                  
           0FF 36 50 12MI PUSH    9C  1MI PUSHF                 
 10 10MI RCL      18 10MI RCR                                   
 F2  1MI REP      F2  1MI REPNZ   F3  1MI REPZ                  
 C3 14MI RET      00 10MI ROL      8 10MI ROR     9E  1MI SAHF  
 38 10MI SAR    0 18 13MI SBB    0AE  5MI SCAS          ( SEG ) 
 20 10MI SHL      28 10MI SHR     F9  1MI STC     FD  1MI STD   
 FB  1MI STI     0AA  6MI STOS  0 28 13MI SUB           ( TEST )
 9B  1MI WAIT           ( XCHG )  D7  1MI XLAT  2 30 13MI XOR   
 C2 14MI +RET                                                   
                                                                
                                                                
                                                                
\ *** Block No. 14 Hexblock E 
\ Structured Conditionals                         ks 19 m„r 88  
: A?>MARK    ( -- f addr ) TRUE   HERE   0 C,   ;               
: A?>RESOLVE ( f addr -- ) HERE OVER 1+ - SWAP C! true ?pairs ; 
: A?<MARK    ( -- f addr ) TRUE   HERE   ;                      
: A?<RESOLVE ( f addr -- ) HERE 1+ -  C,  true ?pairs ;         
' A?>MARK    ASSEMBLER IS ?>MARK                                
' A?>RESOLVE ASSEMBLER IS ?>RESOLVE                             
' A?<MARK    ASSEMBLER IS ?<MARK                                
' A?<RESOLVE ASSEMBLER IS ?<RESOLVE                             
HEX                                                             
75 CONSTANT 0=   74 CONSTANT 0<>   79 CONSTANT 0<               
78 CONSTANT 0>=  7D CONSTANT <     7C CONSTANT >=               
7F CONSTANT <=   7E CONSTANT >     73 CONSTANT U<               
72 CONSTANT U>=  77 CONSTANT U<=   76 CONSTANT U>               
71 CONSTANT OV                                                  
DECIMAL                                                         
\ *** Block No. 15 Hexblock F 
\ Structured Conditionals                            cas 10nov05
HEX                                                             
: IF      C,   ?>MARK  ;                                        
: THEN    ?>RESOLVE   ;                                         
: ELSE    0EB IF   2SWAP   THEN   ;                             
: BEGIN   ?<MARK   ;                                            
: UNTIL   C,   ?<RESOLVE   ;                                    
: AGAIN   0EB UNTIL   ;                                         
: WHILE   IF   ;                                                
: REPEAT   2SWAP   AGAIN   THEN   ;                             
: DO      # CX MOV   HERE   ;                                   
  : Next  AX lods  AX DI xchg  0 [DI] jmp                       
     [ Assembler ] here  next-link @ ,   next-link ! ;          
\ volksFORTH uses "inline" Next and a linked list, to find all  
\ existing NEXT for the debugger.                               
DECIMAL                                                         
\ *** Block No. 16 Hexblock 10 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 17 Hexblock 11 
\ 8086 Assembler                                      08OCT83HHL
LABEL marks the start of a subroutine whose name returns its    
  address.                                                      
DOES-OP Is the op code of the call instruction used for DOES> U 
C;  A synonym for END-CODE                                      
                                                                
Deferring the definitions of the commas, marks, and resolves    
  allows the same assembler to serve for both the system and the
  Meta-Compiler.                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 18 Hexblock 12 
\ 8086 Assembler   Register Definitions               12Oct83map
                                                                
On the 8086, register names are cleverly defined constants.     
                                                                
The value returned by registers and by modes such as #) contains
both mode and register information. The instructions use the    
mode information to decide how many arguments exist, and what to
assemble.                                                       
  Like many CPUs, the 8086 uses many 3 bit fields in its opcodes
This makes octal ( base 8 ) natural for describing the registers
                                                                
                                                                
We redefine the Registers that FORTH uses to implement its      
virtual machine.                                                
                                                                
                                                                
\ *** Block No. 19 Hexblock 13 
\ Addressing Modes                                    16Oct83map
MD  defines words which test for various modes.                 
R8? R16? MEM? SEG? #?  test for mode equal to 0 thru 4.         
REG?  tests for any register mode ( 8 or 16 bit).               
BIG?  tests offsets size. True if won't fit in one byte.        
RLOW  mask off all but low register field.                      
RMID  mask off all but middle register field.                   
SIZE  true for 16 bit, false for 8 bit.                         
BYTE  set size to 8 bit.                                        
OP,  for efficiency. OR two numbers and assemble.               
W,  assemble opcode with W field set for size of register.      
SIZE,  assemble opcode with W field set for size of data.       
,/C,  assemble either 8 or 16 bits.                             
RR,  assemble register to register instruction.                 
LOGICAL  true while assembling logical instructions.            
B/L?  see 13MI                                                  
\ *** Block No. 20 Hexblock 14 
\ Addressing                                          16Oct83map
These words perform most of the addressing mode encoding.       
MEM,  handles memory reference modes.  It takes a displacement, 
  a mode/register, and a register, and encodes and assembles    
  them.                                                         
                                                                
                                                                
WMEM,  uses MEM, after packing the register size into the opcode
R/M,  assembles either a register to register or a register to  
  or from memory mode.                                          
WR/SM,  assembles either a register mode with size field, or a  
  memory mode with size from SIZE. Default is 16 bit. Use BYTE  
  for 8 bit size.                                               
INTER  true if inter-segment jump, call, or return.             
FAR  sets INTER true.  Usage:  FAR JMP,   FAR CALL,   FAR RET.  
?FAR  sets far bit, clears flag.                                
\ *** Block No. 21 Hexblock 15 
\ Defining Words to Generate Op Codes                 12Oct83map
1MI  define one byte constant instructions.                     
2MI  define ascii adjust instructions.                          
3MI  define branch instructions, with one byte offset.          
4MI  define LDS, LEA, LES instructions.                         
5MI  define string instructions.                                
6MI  define more string instructions.                           
7MI  define multiply and divide instructions.                   
8MI  define input and output instructions.                      
                                                                
9MI  define increment/decrement instructions.                   
                                                                
10MI  define shift/rotate instructions.                         
*NOTE*  To allow both 'ax shl' and 'ax cl shl', if the register 
on top of the stack is cl, shift second register by cl. If not, 
shift top ( only) register by one.                              
\ *** Block No. 22 Hexblock 16 
\ Defining Words to Generate Op Codes                 09Apr84map
11MI  define calls and jumps.                                   
 notice that the first byte stored is E9 for jmp and E8 for call
 so C@ 1 AND  is zero for call, 1 for jmp.                      
 syntax for direct intersegment:   address segment #) FAR JMP   
                                                                
                                                                
                                                                
12MI  define pushes and pops.                                   
                                                                
                                                                
                                                                
                                                                
14MI  defines returns.                                          
  RET    FAR RET    n +RET   n FAR +RET                         
                                                                
\ *** Block No. 23 Hexblock 17 
\ Defining Words to Generate Op Codes                 16Oct83map
13MI  define arithmetic and logical instructions.               
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 24 Hexblock 18 
\ Instructions                                        16Oct83map
TEST  bits in dest                                              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 25 Hexblock 19 
\ Instructions                                        16Oct83map
                                                                
ESC                                                             
INT  assemble interrupt instruction.                            
SEG  assemble segment instruction.                              
XCHG  assemble register swap instruction.                       
                                                                
                                                                
                                                                
                                                                
                                                                
CS: DS: ES: SS: assemble segment over-ride instructions.        
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 26 Hexblock 1A 
\ Instructions                                        12Oct83map
MOV  as usual, the move instruction is the most complicated.    
 It allows more addressing modes than any other, each of which  
 assembles something more or less unique.                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 27 Hexblock 1B 
\ Instructions                                        12Oct83map
Most instructions are defined on these two screens. Mnemonics in
parentheses are defined earlier or not at all.                  
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 28 Hexblock 1C 
\ Instructions                                        12Oct83map
Most instructions are defined on these two screens. Mnemonics in
parentheses are defined earlier or not at all.                  
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 29 Hexblock 1D 
\ Structured Conditionals                             16Oct83map
A?>MARK     assembler version of forward mark.                  
A?>RESOLVE  assembler version of forward resolve.               
A?<MARK     assembler version of backward mark.                 
A?<RESOLVE  assembler version of backward resolve.              
                                                                
                                                                
                                                                
                                                                
                                                                
These conditional test words leave the opcodes of conditional   
branches to be used by the structured conditional words.        
  For example,                                                  
   5 # CX CMP   0< IF  AX BX ADD  ELSE  AX BX SUB  THEN         
                                                                
                                                                
\ *** Block No. 30 Hexblock 1E 
\ Structured Conditionals                             12Oct83map
                                                                
One of the very best features of FORTH assemblers is the ability
to use structured conditionals instead of branching to nonsense 
labels.                                                         
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 31 Hexblock 1F 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 32 Hexblock 20 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
\ *** Block No. 33 Hexblock 21 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
