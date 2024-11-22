
\ *** Block No. 0, Hexblock 0

\ VolksForth 8080 Assembler                           UH 09Mar86

Ideen lieferten:
                    John Cassady
                    Mike Perry
                    Klaus Schleisiek
                    Bernd Pennemann
                    Dietrich Weineck









\ *** Block No. 1, Hexblock 1

\ VolksForth 8080 Assembler Load Screen               UH 03Jun86
Onlyforth Assembler also definitions hex

  1 6 +THRU  cr .( VolksForth 8080-Assembler geladen. ) cr

OnlyForth











\ *** Block No. 2, Hexblock 2

   \ Vektorisierte Erzeugung                          UH 03Jun86
Variable >codes

| Create nrc  ]  c, , c@ here allot ! c! [

: nonrelocate ( -- )   nrc >codes ! ;   nonrelocate

| : >exec ( n -- n+2 )
      Create dup c, 2+ does> c@ >codes @ + perform ;

0    | >exec >c,     | >exec >,  | >exec >c@  | >exec >here
     | >exec >allot  | >exec >!  | >exec >c!
drop




\ *** Block No. 3, Hexblock 3

   \ Register und Definierende Worte                  UH 09Mar86

7 Constant A
0 Constant B   1 Constant C      2 Constant D   3 Constant E
0 Constant I   1 Constant I'     2 Constant W   3 Constant W'
0 Constant IP  1 Constant IP'    4 Constant H   5 Constant L
6 Constant M   6 Constant PSW    6 Constant SP  6 Constant S

| : 1MI Create >c, does> C@ >c, ;
| : 2MI Create >c, does> C@ + >c, ;
| : 3MI Create >c, does> C@ swap 8 * + >c, ;
| : 4MI Create >c, does> C@ >c, >c, ;
| : 5MI Create >c, does> C@ >c, >, ;




\ *** Block No. 4, Hexblock 4

   \ Mnemonics                                        UH 09Mar86
00 1MI nop   76 1MI hlt  F3 1MI di   FB 1MI ei    07 1MI rlc
0F 1MI rrc   17 1MI ral  1F 1MI rar  E9 1MI pchl  EB 1MI xchg
C9 1MI ret   C0 1MI rnz  C8 1MI rz   D0 1MI rnc   D8 1MI rc
2F 1MI cma   37 1MI stc  3F 1MI cmc  F9 1MI sphl  E3 1MI xthl
E0 1MI rpo   E8 1MI rpe  F8 1MI rm   27 1MI daa
80 2MI add   88 2MI adc  90 2MI sub  98 2MI sbb   A0 2MI ana
A8 2MI xra   B0 2MI ora  B8 2MI cmp  02 3MI stax  04 3MI inr
03 3MI inx   09 3MI dad  0B 3MI dcx  C1 3MI pop   C5 3MI push
C7 3MI rst   05 3MI dcr  0A 3MI ldax D3 4MI out   DB 4MI in
C6 4MI adi   CE 4MI aci  D6 4MI sui  DE 4MI sbi   E6 4MI ani
EE 4MI xri   F6 4MI ori  FE 4MI cpi  22 5MI shld  CD 5MI call
2A 5MI lhld  32 5MI sta  3A 5MI lda  C3 5MI jmp
C2 5MI jnz   CA 5MI jz   D2 5MI jnc  DA 5MI jc    E2 5MI jpo
EA 5MI jpe   F2 5MI jp   FA 5MI jm


\ *** Block No. 5, Hexblock 5

   \ Spezial Mnemonics und Spruenge                   UH 09Mar86
DA Constant C0=  D2 Constant C0<>  D2 Constant CS
C2 Constant 0=   CA Constant 0<>   E2 Constant PE
F2 Constant 0<   FA Constant 0>=  : not   8 [ FORTH ] xor   ;

: mov   8 * 40 + + >c, ;
: mvi   8 * 6 + >c, >c, ;     : lxi   8 * 1+ >c, >, ;

: [[  ( -- addr )  >here ;                              \ BEGIN
: ?]  ( addr opcode -- )  >c,  >, ;                     \ UNTIL
: ?[  ( opcode -- addr )  >c,   >here   0 >, ;          \ IF
: ?[[ ( addr -- addr' addr )   ?[ swap ;                \ WHILE
: ]?  ( addr -- )   >here swap >! ;                     \ THEN
: ][  ( addr -- addr' )   >here  1+  0 jmp  swap ]? ;   \ ELSE
: ]]  ( addr -- )   jmp ;                               \ AGAIN
: ]]? ( addr addr' -- )   jmp  ]? ;                     \ REPEAT

\ *** Block No. 6, Hexblock 6

   \ Macros                                           UH 14May86
: end-code    context 2- @   context ! ;

: ;c:   0 recover call   end-code ] ;

: Next   >next jmp ;

: rpush ( reg -- )  RP lhld  H dcx  DUP M mov ( high )
   H dcx   1+ M mov ( low )   RP shld  ;

: rpop  ( reg -- )  RP lhld   M over 1+ mov ( low )  H inx
   M swap mov ( high )  H inx   RP shld ;
\  rpush und rpop gehen nicht mit HL

: mvx ( src dest -- )
   2dup  mov ( high )  1+ swap 1+ swap mov  ( low ) ;

\ *** Block No. 7, Hexblock 7

   \ Definierende Worte                               UH 06Aug86
Forth definitions
: Code ( -- )   Create here dup 2- ! Assembler ;

: ;Code ( -- )  0 ?pairs
   compile [ ' does>  >body 2+ @ , ]
   reveal  [compile] [  Assembler ; immediate

: >label ( adr -- )
   here | Create  swap , 4 hallot   >here 4 - heap 4 cmove
   heap last @  (name> !  dp !
   does> ( -- adr )  @   State @ IF  [compile] Literal THEN ;

: Label  [ Assembler ]  >here >label Assembler ;



\ *** Block No. 8, Hexblock 8

                                                      UH 14May86
















\ *** Block No. 9, Hexblock 9

% VolksForth 8080 Assembler  Shadow-Screens           UH 09Mar86
















\ *** Block No. 10, Hexblock a

% VolksForth 8080 Assembler                           UH 03Jun86

Der 8080 Assembler wurde von John Cassady, in den Forth
Dimensions veroeffentlicht und von Mike Perry im F83
implementiert. Er unterstuetzt den gesamten 8080 Befehlsvorrat
und auch Befehle zur strukturierten Assemblerprogrammierung.
Um ein Wort in Assembler zu definieren wird das definierende
Wort Code benutzt, es kann, muss aber nicht mit end-code beendet
werden. Wie der Assembler arbeitet ist ein interessantes
Beispiel fuer die Maechtigkeit von Create does>.
Am Anfang werden die Befehle in Klassen eingeteilt und fuer
jede Klasse ein definierndes Wort definiert. Wenn der Mnemonic
des Befehls spaeter interpretiert wird, kompiliert er den
entsprechenden Opcode.



\ *** Block No. 11, Hexblock b

   % Vektorisierte Erzeugung                          UH 09Mar86
Zeigt Auf die Tabelle mit den aktuellen Erzeugungs-Operatoren.

Tabelle mit Erzeugungs-Operatoren fuer In-Line Assembler

Schaltet Assembler in den In-Line Modus.

Definierendes Wort fuer Erzeugungs-Operator-Namen.


Die Erzeugungs-Operator-Namen, sie fuehren den entsprechenden
aktuellen Erzeugungsoperator aus.

Mit diesen Erweiterungen kann der Assembler auch fuer den
Target-Compiler benutzt werden.


\ *** Block No. 12, Hexblock c

   % Register und Definierende Worte                  UH 09Mar86

Die 8080 Register werden definiert. Es sind einfach Konstanten
die Information fuer die Mnemonics hinterlassen.
Einige Register der Forth-Maschine:
    IP ist BC, W ist DE


Definierende Worte fuer die Mnemonics.
Fast alle 8080 Befehle fallen in diese 5 Klassen.







\ *** Block No. 13, Hexblock d

   % Mnemonics                                        UH 09Mar86
Die 8080 Mnemonics werden definiert.















\ *** Block No. 14, Hexblock e

   % Spezial Mnemonics und Spruenge                   UH 09Mar86
Vergleiche des 8080

not folgt einem Vergleich, wenn er invertiert werden soll.

die Mnemonics, die sich nicht in die Klassen MI1 bis MI5
einteilen lassen.

Die strukturierten Assembler-Anweisungen.
Die 'Fleischerhaken' werden benutzt, damit keine Verwechselungen
zu den strukturierten Anweisungen in Forth entstehen.
Es findet keine Absicherung der Kontrollstrukturen statt, sodass
sie auch beliebig missbraucht, werden koennen.
Das ist manchmal aus Geschwindigkeitsgruenden leider notwendig.



\ *** Block No. 15, Hexblock f

   % Macros                                           UH 17May86
end-code beendet eine Code-Definition

;c:   Erlaubt das Einbinden von High-Level Forth in Code-Worten.

Next  Assembliert einen Sprung zum Adress-Interpretierer.

rpush Das angegebene Register wird auf den Return-Stack gelegt.


rpop  Das angegebene Register wird vom Return-Stack genommen.

rpush und rpop benutzen das HL Register.

mvx  Ein 16-Bit-Move wie 'mov' fuer 8-Bit Register
     Bewegt Registerpaare HL BC DE

\ *** Block No. 16, Hexblock 10

   % Definierende Worte                               UH 17May86
Code leitet eine Code-Definition ein.

;code ist das Low-Level-Aequivalent von does>


>label erzeugt ein Label auf dem Heap, mit dem angegebenen Wert




Label erzeugt ein Label auf dem Heap, mit dem Wert von here





\ *** Block No. 17, Hexblock 11

















