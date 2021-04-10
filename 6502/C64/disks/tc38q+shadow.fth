
\ *** Block No. 0, Hexblock 0

\  ###  Main Directory  ###  cclv05jan87

Version History     $02
(C16 (C64 )         $03
Relocate System     $04
Special-Assembler   $05
savesystem          $0E
Target-compiler     $10
free                $34

Target Compiler Man $56















\ *** Block No. 1, Hexblock 1

\  ###  Main Directory  ###  cclv05jan87  \ \ Target Compiler Manual      clv06dec88

Version History     $02                   \ Target-Compiler volksFORTH 3.8 6502
(C16 (C64 )         $03
Relocate System     $04
Special-Assembler   $05
savesystem          $0E
Target-compiler     $10
free                $34

Target Compiler Man $56

                                          \ (c) volksFORTH-Developers 1985-2006
                                          \     and Forth Gesellschaft e.V.
                                          \     http://www.forth-ev.de











\ *** Block No. 2, Hexblock 2

\                            cclv06dec88  \ \ ..Gebrauchsanweisung..      clv05jan87

cas18aug06 - english translation          \ 1. Introduction
clv06dec88 - rewritten Manual             \    The Targetcompiler is weird,
                                          \    kryptic and sometimes dangerous.
clv/re apr-oct87                          \    If an error occurs, reboot machine
 for rev 3.8 - C16/C64                    \    and start from scratch.
 Scr 3,4,c,10,12,1b,2f


















\ *** Block No. 3, Hexblock 3

                              clv2:jul87  \ \ ..Manual                    clv06dec88

                                          \ 2. Load

                                          \ 2.1. Relocate System
                                          \    The Relocate Screen on this Disk
                                          \    creats an Environment with Stacks
                                          \    Stacklen  $9400
                                          \    rStacklen $0400

                                          \    Limit must be $c000 which is default
                                          \    for C64, for C16 this value must
                                          \    be down-patched
                                          \ 2.1. load save-system from disk 1
                                          \ 2.2. load Editor (if needed)
                                          \ 2.3. Load Targetcompiler with Loadscr.
                                          \      first only the resident part
                                          \ 2.4. execute savesystem
                                          \ 2.5. load Tragetcompiler predefinitions
                                          \      (see bottom of Loadscreen)






\ *** Block No. 4, Hexblock 4

\ Relocating a system        clv2:jull87  \   a

$9400         $0400
( stacklength rstacklength -)             \                      volksforth 3.8TC  u3
 empty hex                                \  2a    
 over + origin +  origin 0A + ! \ r0
 origin +  dup    origin   1+ ! \ task    \           t and Targetmachine
             6 -  origin  8 + ! \ s0      \        the Sources are prepared for
 (16 $c000 ' limit >body ! C)             \        C16 & C64. Change the definitions
 cold                                     \        of (C16 (C64 (see Screen and
\\ symbolic map of system                 \        Handbook)
up@ origin - is stacklength               \      - Blk ## here #### there ####
r0 @ up@ -   is rstacklength              \        will be printed as status msg.
                                          \      - various <name> exists
disk-buffer  limit        first @         \        Messages are ok
rstack       r0 @         rp@             \      - at the End  SAVESYSTEM <name>
                                          \        will be printed. Change Disk!
user, warm   up@ udp @ +  up@             \        and then press <return> to save
(heap)       up@          heap            \        new Forth System
stack        s0 @         sp@

system       here         origin 0FE +
user, cold   origin 0FE + origin
screen       0800         0400
page 0-3     0400         0000

\ *** Block No. 5, Hexblock 5

( Forth-6502 Assembler             WFR )  \ \\ ..Example session          clv06dec88
( Basis: Forth Dimensions VOL III No. 5)
                                          \ 3. Example for CBM Plus 4
Onlyforth  Assembler also definitions

1 8  +thru                                \    Used Disks:
                                          \    1of4 .. 4of4 - volksForth Disks 3.8
                                          \    TCq          - TargetComp Source
                                          \    TCf          -     "    Files (empty)

                                          \ 3.1. Create a Targetcompiler System


                                          \    Switch on Plus4, insert Disk 1of4

                                          \    DIRECTORY
                                          \    -> ...

                                          \    LOAD "C16ULTRAFORTH83",8
                                          \    -> searching... loading...ready.

                                          \    RUN
                                          \    -> ultraFORTH ... ok



\ *** Block No. 6, Hexblock 6

( Forth-83 6502-Assembler              )  \ \\ ..Example Session          clv06dec88

: end-code   context 2- @  context ! ;    \     insert Disk TCq
                                          \     4 load flush  \ relocate
Create index                              \     -> volksFORTH83 ... ok
0909 , 1505 , 0115 , 8011 ,
8009 , 1D0D , 8019 , 8080 ,               \     insert Disk 3of4
0080 , 1404 , 8014 , 8080 ,               \     19 load flush \ Editor
8080 , 1C0C , 801C , 2C80 ,               \     -> blk 4 blk 5 ...... ok \ appr5 min

| Variable mode                           \     insert Disk 1of4
                                          \     26 load flush \ savesystem
: Mode:  ( n -)   Create c,
  Does>  ( -)     c@ mode ! ;             \     insert Disk TCq
                                          \     $10 load flush \ TC-resident Part
0   Mode: .A        1    Mode: #
2 | Mode: mem       3    Mode: ,X         \     insert Disk TCf
4   Mode: ,Y        5    Mode: X)         \     savesystem @:vf-tc-3.8
6   Mode: )Y       0F    Mode: )          \     -> ok  If Floppy flashes: Error
                                          \     \ @: is: overwrite if needed
                                          \     \ this file will later replace
                                          \     \ the running Forth System




\ *** Block No. 7, Hexblock 7

( Code generating primitives  27jun85we)  \ \\ ..Example Session          clv06dec88
                                          \ 3.2. Compiling a new System
Variable >codes                           \      first as for 3.1.
                                          \      or:
| Create nrc ] c, , c@ here allot ! c! [  \      LOAD "vf-tc-3.8",8
                                          \      -> searching...loading..ready.
: nonrelocate   nrc >codes ! ;
                                          \      RUN
nonrelocate                               \      -> volksFORTH83 ... ok

| : >exec Create c,                       \      insert Disk TCq
         Does> c@ >codes @ + @ execute ;  \      hex 27 30 thru flush
                                          \         \ load transient Part of TC
|  0 >exec >c,       |  2 >exec >,
|  4 >exec >c@       |  6 >exec >here     \      insert Disk 2of4 (Forth Source)
|  8 >exec >allot    | 0A >exec >!        \      $09 l \ edit Screen 9
| 0C >exec >c!                            \              (c64 (c16 (c16+ (c16-
                                          \              (comment in or out depend.
                                          \              on Target-Maschine

                                          \      $f load \ compile system
                                          \      insert TCf or blank disk
                                          \      savetarget c16ultraforth83
                                          \  or  savetarget c64ultraforth83
                                          \ - ENDE -

\ *** Block No. 8, Hexblock 8

( upmode  cpu                          )

| : upmode ( addr0 f0 - addr1 f1)
 IF mode @  8 or mode !   THEN
 1 mode @  0F and ?dup IF
 0 DO  dup +  LOOP THEN
 over 1+ @ and 0= ;

: cpu  ( 8b -)   Create  c,
  Does>  ( -)    c@ >c, mem ;

 00 cpu brk  18 cpu clc  D8 cpu cld
 58 cpu cli  B8 cpu clv  CA cpu dex
 88 cpu dey  E8 cpu inx  C8 cpu iny
 EA cpu nop  48 cpu pha  08 cpu php
 68 cpu pla  28 cpu plp  40 cpu rti
 60 cpu rts  38 cpu sec  F8 cpu sed
 78 cpu sei  AA cpu tax  A8 cpu tay
 BA cpu tsx  8A cpu txa  9A cpu txs
 98 cpu tya






\ *** Block No. 9, Hexblock 9

( m/cpu                                )

: m/cpu  ( mode opcode -)  Create c, ,
 Does>
 dup 1+ @ 80 and IF 10 mode +! THEN
 over FF00 and upmode upmode
 IF mem true Abort" invalid" THEN
 c@ mode @ index + c@ + >c, mode @ 7 and
 IF mode @  0F and 7 <
  IF >c, ELSE >, THEN THEN mem ;

 1C6E 60 m/cpu adc   1C6E 20 m/cpu and
 1C6E C0 m/cpu cmp   1C6E 40 m/cpu eor
 1C6E A0 m/cpu lda   1C6E 00 m/cpu ora
 1C6E E0 m/cpu sbc   1C6C 80 m/cpu sta
 0D0D 01 m/cpu asl   0C0C C1 m/cpu dec
 0C0C E1 m/cpu inc   0D0D 41 m/cpu lsr
 0D0D 21 m/cpu rol   0D0D 61 m/cpu ror
 0414 81 m/cpu stx   0486 E0 m/cpu cpx
 0486 C0 m/cpu cpy   1496 A2 m/cpu ldx
 0C8E A0 m/cpu ldy   048C 80 m/cpu sty
 0480 14 m/cpu jsr   8480 40 m/cpu jmp
 0484 20 m/cpu bit



\ *** Block No. 10, Hexblock a

( Assembler conditionals               )

| : range?   ( branch -- branch )
 dup abs  07F u> Abort" out of range " ;

: [[  ( BEGIN)  >here ;

: ?]  ( UNTIL)  >c, >here 1+ -
                range? >c, ;

: ?[  ( IF)     >c,  >here 0 >c, ;

: ?[[ ( WHILE)  ?[ swap ;

: ]?  ( THEN)   >here over >c@
                IF swap >!
 ELSE over 1+ - range? swap >c! THEN ;

: ][  ( ELSE)   >here 1+   1 jmp
 swap >here over 1+ - range?  swap >c! ;

: ]]  ( AGAIN)  jmp ;

: ]]? ( REPEAT) jmp ]? ;


\ *** Block No. 11, Hexblock b

( Assembler conditionals               )

90 Constant CS    B0 Constant CC
D0 Constant 0=    F0 Constant 0<>
10 Constant 0<    30 Constant 0>=
50 Constant VS    70 Constant VC

: not    20 [ Forth ] xor ;

: beq    0<> ?] ;   : bmi   0>= ?] ;
: bne    0=  ?] ;   : bpl   0<  ?] ;
: bcc    CS  ?] ;   : bvc   VS  ?] ;
: bcs    CC  ?] ;   : bvs   VC  ?] ;













\ *** Block No. 12, Hexblock c

\ 2/w/inc/dec c16 ram/rom..  cclv2:jul87

: 2inc
 dup lda  clc  2 # adc
 dup sta  CS ?[  swap 1+ inc  ]?  ;
: 2dec
 dup lda  sec  2 # sbc
 dup sta  CC ?[  swap 1+ dec  ]?  ;

: winc
 dup inc  0= ?[  swap 1+ inc  ]?  ;
: wdec
 dup lda  0= ?[  over 1+ dec  ]?  dec  ;

: ;c:
 recover jsr  end-code ]  0 last !  0 ;

(16 \ C16+Macros for Bankswitching
: ram $ff3f sta ;   : rom $ff3e sta ;
' Jsr Alias NormJsr   Defer Jsr

: C16+Jsr dup 0c000 u>
 IF rom NormJsr ram ELSE NormJsr THEN ;
' C16+Jsr Is Jsr  C)


\ *** Block No. 13, Hexblock d

( Assembler ;code Code Label  03feb85bp)

Onlyforth

: Assembler
 Assembler   [ Assembler ] mem ;

: ;Code
 [compile] Does>  -3 allot
 [compile] ;      -2 allot   Assembler ;
immediate

: Code  Create here dup 2- ! Assembler ;

: >label  ( adr -)
 here | Create  swap , 4 hallot
        heap 1 and hallot   \ 6502-align
        here 4 - heap  4  cmove
        heap last @ count 01F and + !
 dp !
        Does>  ( - adr)   @  ;

: Label
 [ Assembler ]  >here >label Assembler ;


\ *** Block No. 14, Hexblock e

\ cSave cLoad..               clv08aug87

\needs Code   .( ?! Code ?!) \\
Assembler
(16 \needs rom    .( ?! rom ?!) \\  C)

Onlyforth
$FF90 >label setMsg   $90 >label status
$FFBA >label setlfs $FFBD >label setNam
$FFD8 >label BSAVE  $FFD5 >label BLOAD
Label slPars
 setup jsr (16 rom C)
 $80 # lda setMsg jsr 0 # lda status sta
 N lda sec 8 # sbc  (drv    sta
       CC ?[ dex ]? (drv 1+ stx
 N ldx     N  1+ ldy 1 #  lda setlfs jsr
 N 4 + ldx N 5 + ldy N 2+ lda setnam jsr
 N 6 + ldx N 7 + ldy
 rts end-code

Label slErr \ AR=Kernalerror
 CC ?[ 0 # lda ]? pha
 status lda $BF # and
 (16 ram C) push jmp end-code
                                   -->

\ *** Block No. 15, Hexblock f

\ savesystem                   02oct87re

Onlyforth

Code cSave ( f t+1 Name Nlen dev--err)
 5 # lda    SLPars jsr
 N 8 + # lda bsave jsr
 slErr jmp end-code

: savesystem \ -- Name must follow
 \ Forth-Kernal a la boot:
   scr push 1 scr ! r#  push 0 r# !
 \ Editor  a la boot
   [ Editor ]
   stamp$ push stamp$ off
   (pad   push (pad   off
 \ nun geht's los
   save
   origin $17 - here     0 parse
   8 csave abort" Save-Error" ;






\ *** Block No. 16, Hexblock 10

\ Target compiler loadscr      11jul20pz  \ \\ Terms:
\ Idea and first Implementation by ks/bp  \ Host      'normal' Forth-System in
\ Implemented on 6502  by ks/bp           \           Machine
\ volksFORTH83-Version by bp/we           \ Target    System to be compiled
                                          \ Transient Vocabulary for T-Compilation
Onlyforth hex                             \           in Host
: (blk@  blk @ ;
Defer blk@  ' (blk@ is blk@               \ needs the Specialassembler for TC
                                          \ 'normal' Assembler
\needs (16   .( ?! (16 (64 ?! C) quit
Assembler \needs nonrelocate 5 load       \ Startaddress of Target Systems in Host
Assembler nonrelocate

Variable Image      C000 Image !

Vocabulary Ttools
Vocabulary Defining
                                          \ Tools for Target-System
                                          \ Words for Redefinitions
 1 10 +thru   \ Target compiler
11 13 +thru   \ Target Tools
14 16 +thru   \ Redefinitions
clear
\ hex 17 20 +thru   \ predefinitions


\ *** Block No. 17, Hexblock 11

\ Target header pointers     bp27jun85we  \ \\

Variable tdp                              \ Target-dp
: there  tdp @ ;                          \ here in Target

Variable displace                         \ Startaddress of Target-Systems
Variable ?thead       0 ?thead !          \ If 0, we create header in Target
Variable tlast        0 tlast !           \ nfa of last created word
Variable glast'       0 glast' !          \ cfa of last created ghost
Variable tdoes>                           \ cfa of Code for Does>-Parts
Variable >in:                             \ Address of last : in Block
Variable tvoc         0 tvoc !            \ Tvoc-Link for Host
Variable tvoc-link    0 tvoc-link !       \ Tvoc-Link for Target













\ *** Block No. 18, Hexblock 12

\ Image and byteorder        clv2:jull87  \ \\

Code romoff (64                           \ switches to RAM under OS
 sei 034 # lda 01 sta C) Next jmp

Code romon  (64                           \ switches Back
 036 # lda 01 sta cli C) Next jmp

Code >byte   ( 16b - 8bl 8bh)
 SP )Y lda pha txa SP )Y sta
 SP 2dec txa
 SP )Y sta pla PutA jmp

Code byte>   ( 8bl 8bh - 16b)
 SP X) lda pha SP 2inc pla SP )Y sta
 Next jmp end-code

: >image    ( addr1 - addr2)              \ calculates physical Address from
 displace @  -  image @  + ;              \ target Address

: >heap  ( from quan -)                   \ cmove to Heap with automatic hallot
 heap over - 1 and +      \ 6502-align    \ >image must be adjusted when changing
 dup hallot heap swap cmove ;             \ memory managent for target system

                                          \ same with c@ and c!

\ *** Block No. 19, Hexblock 13

\ Ghost-creating             bp27jun85we  \ \\

0 | Constant <forw>  0 | Constant <res>   \ <forw> points to code for forward
                                          \ <res>  points to code for resolved
| : Make.ghost  ( - cfa.ghost)
 here State @                             \     if compiling, Ghost will be linked
 IF   Context @                           \ under(!) last Context Word
 ELSE Current                             \ else appended to Current as normal
 THEN @ dup @ ,                           \ lfa Ghost points to last word  (s.a)
 name  dup  c@ 1 01F uwithin              \ get name and checks
            not abort" inval.Gname"       \             for valid length
       1 over +!  c@ 1+ allot             \  enlarges name by one blank
 here 2 pick - -rot                       \ calculates namelength ( len start link)
 <forw> , 0 , 0 ,                         \ cf.Ghost, cfa.Target, Ptr to Does>.cfa
 over 2+ c@ 1 and 1 xor >r                \ 6502-align
 over r@ -  here over - >heap             \ cmoved Ghost on Heap
 heap r@ + swap !       Dp !              \ last word points not to Ghost
 heap r> + +  ;                           \ caclculates cfa.ghost

                                          \ Last (Current @) will be linked to ; in
                                          \ reveal





\ *** Block No. 20, Hexblock 14

\ ghost words                ks27jun85we  \ \\

: gfind  ( string - cfa tf / string ff)   \ search for ghost
 dup >r  1 over +!  find  -1 r> +! ;

: ghost   ( - cfa)
 >in @  name  gfind                       \ search for ghost
  IF nip exit THEN                        \ if found, finish
  drop >in ! Make.ghost ;                 \ if not found, it will be created

: Word,  ghost  execute ;                 \ stores cfa.ghost ( <forw> or <res> )

: gdoes>  ( cfa.ghost - cfa.does)
 4 + dup @                                \ Adr of Ptr to Does>.cfa
  IF @ exit THEN                          \ if existing, cfa of Does>
  here dup <forw> , 0 , 4 >heap           \ else create PTR and see above
  DP !  heap dup rot ! ;


                                          \ >heap starts on even addresses
                                          \ => 6502-align





\ *** Block No. 21, Hexblock 15

\ ghost utilities            ks27jun85we  \ \\

: g'   name gfind 0= abort" ?" ;          \ gets cfa of Ghost

: '.
 g' dup @ <forw> case?                    \ get state of ghost, if forward reference
   IF ."  forw"                           \ or already resolved (with cfa.Target)
   ELSE <res> - abort" ??" ."  res" THEN
  2+ dup @ 5 u.r
  2+ @ ?dup                               \ same for possib.  Does>-Parts
   IF dup @ <forw> case?
    IF ."  fdef"
    ELSE <res> - abort"  ??" ."  rdef"
    THEN
    2+ @ 5 u.r THEN ;


' ' Alias h'                              \ while Target-Compilation ' will be done
                                          \ for Ghosts







\ *** Block No. 22, Hexblock 16

\ .unresolved                bp27jun85we  \ \\

| : forward? ( cfa - cfa / exit&true)
 dup @ <forw> =                           \ if cf =<forw>
 over 2+ @ and                            \ and an Address exists in Target System
 IF drop True rdrop exit THEN ;           \ exit unresolved? with True-Flag

| : unresolved? ( addr - f)
 2+ dup c@ 01F and over + c@ BL =         \ checks if name is a Ghost
 IF name> forward? 4 + @                  \ gets cfa and checks for unresolved
    dup IF forward? THEN                  \  same with Does>-Part
 THEN drop  False ;

| : unresolved-words
 BEGIN @ ?dup WHILE dup unresolved?       \ print for Vocabulary all
    IF dup  2+ .name ?cr THEN             \ unresolved words
 REPEAT ;

: .unresolved
 voc-link @                               \ search through all Vocabularies
 BEGIN dup 4 - unresolved-words           \ print non-resolved words
 @ ?dup 0= UNTIL ;




\ *** Block No. 23, Hexblock 17

\ Ext. vocs for t-compilat.  bp27jun85we  \ \\

: Vocabulary                              \ Vocabulary structure:
Vocabulary 0 , here tvoc @ , tvoc ! ;     \ Name           Bytes
                                          \ Code           fuer Vocabularys
Vocabulary Transient    0 tvoc !          \ Latest          0 1
                                          \ Cold-Latest     2 3   normal
Only definitions Forth also               \ Voc-link        4 5
                                          \ -------------------
: T Transient ;   immediate               \ Tlatest         6 7   additional
: H Forth     ;   immediate               \ Tvoc-link       8 9

definitions
                                          \ T and H are Immediate and replacing
                                          \              [ Transient ]
                                          \         or   [ Forth ]  H => Host










\ *** Block No. 24, Hexblock 18

\ Transient primitives       ks04jul85we  \ \\  Words for Target-Compilation

Transient definitions                     \ Words for virtual Target-Memory

: c@    H >image romoff c@ romon ;        \ T c@ acces RAM below OS

: c!    H >image romoff c! romon ;        \ T c! also
                                          \ All following words use c@ and c!
: @     T dup c@ swap 1+ c@ byte> ;

: !     >r  >byte r@ 1+ T c!  r> c! ;

: cmove ( from.mem to.target quan -)      \ cmove works only from Host to
    bounds ?DO                            \ Target, not other direction
    dup H c@ I T c! H 1+ LOOP drop ;

: here  there ;

: allot Tdp +! ;

: c,    T here c! 1 allot H ;             \ On changes to the virtual Memory-
                                          \ Management, c@, c! and >image
: ,     T here !  2 allot H ;             \ must be adjusted!



\ *** Block No. 25, Hexblock 19

\ Transient primitives       bp27jun85we  \ \\  Words for Target-Compilation

: ,"    Ascii " parse dup T c,
        under there swap cmove allot H ;

: fill  ( addr quan 8b -)
        -rot bounds
        ?DO dup  I T c! H LOOP drop ;

: erase  0  T fill ;
: blank  BL T fill ;
: here!  H tdp ! ;














\ *** Block No. 26, Hexblock 1a

\ Resolving                  ks29jun85we  \ \\

Forth definitions

: resolve  ( cfa.ghost cfa.target -)      \ resolves forward references
 over dup @ <res> =                       \ checks, if already resolved
   IF space dup >name .name ." exists  "  \ print warning if yes
      2+ ! drop exit THEN                 \ set cfa.target to new value, End
 >r >r 2+ @ ?dup                          \ Forwardreference available ?
 IF BEGIN dup T @ H                       \ Yes, get address in Target
           2dup = abort" resolve loop"    \  Cancel, if pointing to itself
       r@ rot T ! H ?dup 0= UNTIL THEN    \ and set cfa.target until End
 r> r>  <res> over !  2+ ! ;              \ cfa.target to cfa.ghost and
                                          \ resolved as cf.target
: resdoes>  ( cfa.ghost cfa.target -)
 swap gdoes> dup @ <res> =                \ same for Does>-Parts
 IF 2+ ! exit THEN swap resolve ;

] Does> [ here 3 - 0 ]                    \ Does> compiles a JMP (dodoes>
        dup @ there  rot ! T , H ;        \ (dodoes> brings the on
  ' <forw> >body !                        \ cfa.ghost folllowing address, which is
                                          \ cfa.target, on the stack
] Does> [ here 3 - 0 ]                    \ cfa.ghost is either  <forw> or <res>
        @ T , H ;                         \ cfa.target is the cfa in Target-System
  ' <res>  >body !                        \ also <forw> or already valid

\ *** Block No. 27, Hexblock 1b

\ move-threads  6502-align   clv24.3.87)  \ \\

: move-threads
 Tvoc @  Tvoc-link @                      \ for all Vocabluaries in Transient and
 BEGIN over ?dup WHILE 2- @  over 2- T !  \ Target set Target-Cold-Latest
       @ H  swap @ swap                   \ to Transient-Tlatest
 REPEAT
 error" some undef. Target-Vocs left"     \ Error, if Tvoc-link also points to other
 drop ;                                   \ Vocabularys

 : tlatest   ( - addr)
   Current @ 6 +  ;                       \ Points to Tlatest in Transient-Vocs

 : 6502-talign  ( supposed cfa -- )
 0FF and 0FF =  IF  1 T allot H  THEN ;

: save-target   \ name must follow        \ saves a Targetsystem as Programm-
 08 02 busopen                            \ file on Disk (C64-Special!!)
 0 parse bustype " ,p,w" count bustype
 busoff
 08 02 busout
 displace @ 100 u/mod swap bus! bus!
 there displace @
   DO I T c@ H bus! LOOP
 08 02 busclose ;

\ *** Block No. 28, Hexblock 1c

\ compiling names into targ.   11jul20pz  \ \\

: (theader
 ?thead @  IF 1 ?thead +!                 \ if 0, Header, else ?thead increment
     there 6502-talign exit THEN          \      6502-align and Ende
 >in @ name swap >in !                    \ gets Name an converts to capital
 dup c@ 1 020 uwithin not                 \ Error on wrong length
                  abort" inval. Tname"
 dup  c@ 5 +  there +  6502-talign        \ calulates cfa and 6502-align
 blk@  T , H                              \ Blocknumber for view
 there tlatest dup @  T , H !             \ link in of new name in Current
 there dup tlast !  over                  \ Tlast for immediate and restric create
 c@ 1+ dup  T allot cmove H  ;            \ save Name in Target


: Theader    tlast off                    \          set Tlast to 0
 (theader Ghost                           \ create Header, create Ghost, if new,
 dup glast' ! there resolve ;             \ cfa.ghost to Glast' and resolve








\ *** Block No. 29, Hexblock 1d

\ prebuild defining words    bp27jun85we  \ \\

| : executable?   ( adr - adr f)  dup ;   \ on Create, User, Constant, Variable
                                          \ and Defer, not for : und Vocabulary
| : tpfa,  there , ;                      \ compiling Ptr to pfa.target in Host


| : (prebuild   ( cfa.adr -)              \ creates Header in Host with cfa.adr
 >in @  Create  >in !  here 2- ! ;        \ This points to a Does>-Part in
                                          \ Target (s.below)
: prebuild   ( adr 0.from.: - 0)
 0 ?pairs                                 \ from:
 executable? dup >r                       \ if created word should be executable
 IF [compile] Literal                     \ in current, create a header else not
     compile  (prebuild                   \ with corresponding cfa
 ELSE drop  THEN                          \ else not
 compile Theader  Ghost gdoes> ,          \ create header in Target and compile
 r>  IF compile tpfa, THEN 0 ;            \ as cfa of Does>-Part
immediate restrict                        \ store this address in Host as pdf


                                          \ prebuild is a Defining-Word for
                                          \ Defining-Words !!



\ *** Block No. 30, Hexblock 1e

\ code portion of def.words  bp27jun85we  \ \\

: dummy   0 ;                             \ results that with follwing
                                          \ Defining-Word created words cannot be
                                          \ executed
: Do>     ( - adr.of.jmp.dodoes> 0)       \ Special-Does> for Words created in
          [compile] does>  here 3 -       \ Current, gives pfa in Target !
           compile  @   0  ]  ;


                                          \ Do> ... ;  : ... Build ... ; same as
                                          \ Create ... Does>

                                          \ : Do>     [compile] does> here 3 - 0 ] ;

                                          \ : (build  Create here 2- ! ;
                                          \ : Build  (cfa 0 - 0)
                                          \           0 ?pairs  [compile] Literal
                                          \           compile (build 0 ;







\ *** Block No. 31, Hexblock 1f

\ the 6502 Assembler         bp27jun85we  \ \\

| Create relocate
 ] T c, , c@ here allot ! c!   H [        \ Assembler assembles in Target now

Transient definitions

: Assembler  H
 [ Assembler ] relocate >codes !          \ enables Assembler and relocate
 Assembler ;

: >label  ( 16b -) H
 >in @ name gfind  rot >in !              \ if label already exists as Ghost,
  IF over resolve dup THEN                \   resolve forward reference
 drop Constant   ;                        \ as Constant in Host

: Label  H there T >label  Assembler H ;  \ Label points to there

: Code  H                                 \ Special-Code for Target
 Theader there 2+ T , Assembler H   ;






\ *** Block No. 32, Hexblock 20

\ immed. restr. ' | compile  bp27jun85we  \ \\
                                          \ Controlstructures for Target-System
: ?pairs    ( n1 n2 -- ) H
 - abort" unstructured" ;

: >mark     ( - addr)  H there T 0 , H ;
: >resolve  ( addr -)  H
            there over - swap  T ! H ;
: <mark     ( - addr)  H there ;
: <resolve  ( addr -)  H there - T , H ;

: immediate   H Tlast @ ?dup
 IF dup T c@  040 or swap c! H THEN ;

: restrict    H Tlast @ ?dup
 IF dup T c@  080 or swap c! H THEN ;

: '   ( <name> - cfa)  H                  \ ' in Transient accesses Ghosts and
 g' dup @ <res> -   abort" ?" 2+ @ ;      \ gets cfa.target

: |  H ?thead @ ?exit ?thead on ;         \ The next word will be created without
                                          \ header
: compile  H Ghost , ;                    \ works on Host, not on Target
 immediate  restrict


\ *** Block No. 33, Hexblock 21

\ Target tools               ks27jun85we  \ \\     Tools for Target-System
                                          \     similar to normal tools
Onlyforth Ttools also definitions

| : ttype   ( adr n -)                    \ prints n chars at addr
 bounds ?DO I T c@ H  emit LOOP ;

: .name    ( nfa -)                       \ prints name of word, if nfa <>0
 ?dup
 IF dup 1+ swap T c@ H 01F and ttype
 ELSE ." ??? " THEN  space ?cr ;          \ else  ???

| : nfa? ( cfa lfa - nfa / cfa ff)        \ checks, if lfa nfa of cfa is and
   BEGIN  dup WHILE  2dup 2+ dup          \ returns nfa, else  cfa and ff
            T c@ H 01F and + 1+ =
             IF 2+ nip exit THEN
            T @ H  REPEAT ;

: >name  ( cfa - nfa / ff)                \ converts cfa in nfa, if possible
 Tvoc BEGIN @ dup WHILE under 2- @ nfa?
        ?dup IF nip exit THEN
      swap  REPEAT  nip  ;




\ *** Block No. 34, Hexblock 22

\ Ttools for decompiling     ks29jun85we  \ \\     Tools for Target-System
                                          \     similar to normal tools
| : ?:  dup          4 u.r ." :"  ;
| : @?  dup  T @  H  6 u.r  ;
| : c?  dup  T c@ H  3  .r  ;

: s  ( adr - adr+)                        \ prints string at adr and adjusts adr
 ?: space  c?  3 spaces
 dup 1+ over T c@ H ttype dup
 T c@ H + 1+ ;

: n  ( adr - adr+2)                       \ print name of compile word and adjust
 ?: @? 2 spaces dup                       \ adr
 T @ H [ Ttools ] >name .name H 2+ ;

: d  ( adr n - adr+n)                     \ prints n bytes from adr and adjusts adr
 2dup swap ?: swap 0
   DO c? 1+ LOOP 2 spaces -rot ttype ;








\ *** Block No. 35, Hexblock 23

\ Tools for decompiling      bp29jun85we  \ \\     Tools for Target-System

: l  ( adr - adr+2)
 ?: 5 spaces @? 2+  ;

: c  ( adr - adr+1)   1 d ;

: b  ( adr - adr+1)
 ?: @? dup T @ H over + 5 u.r 2+ ;

: dump   ( adr n -)                       \ prints n Bytes at adr, like d, but nice
 bounds ?DO cr I 8 d drop stop?           \ formatted
   IF LEAVE THEN 8 +LOOP ;

: view                                    \ displays Sourcecodescreen of word
 T ' H  [ Ttools ] >name
 ?dup IF 4 - T @ H edit THEN ;









\ *** Block No. 36, Hexblock 24

\ reinterpretation def.-words  27jun85we  \ \\

Onlyforth                                 \ allows execution of new created defining
                                          \ words during target compilation
: redefinition
 tdoes> @                                 \ is ;code or does> ?
 IF >in push                              \  yes, save systemstate
    [ ' >interpret >body ] Literal push
   State push  Context push               \  >in to begin of last Colondef.
   >in: @ >in !
   name [ ' Transient 2+ ] Literal        \   no as predefinition in
   (find nip 0=                           \   Transient available?
   IF                                     \   yes, print "Redefinition: " and last
    cr ." Redefinition: " here .name      \       Names
   >in: @ >in ! : Defining interpret      \   >in adjust, Forth-: execute and switch
   THEN                                   \   on Definining as Context
 THEN
 0 tdoes> ! ;                             \   reset tdoes>








\ *** Block No. 37, Hexblock 25

\ Create..does> structure    bp27jun85we  \ \\

| : (;tcode                               \ changes the cfa of lst defined word
 Tlast @ dup T c@ + 1+ !  H rdrop ;       \ to Does>-Part in Target

| : changecfa
 compile lit tdoes> @  ,                  \ compiles, which compiles the adr of
 compile (;tcode ;                        \ Does>-part and (;code when executed

Defining definitions
                                          \ ;code and Does> must be defined in redef
: ;code   0 ?pairs
 changecfa  reveal  rdrop  ;              \ same as Do> for prebuild
 immediate restrict                       \ stores last word in Host and jumps
                                          \ in redefinition behind interpret
Defining  ' ;code  Alias  does>
 immediate restrict                       \ Structur of a in Host created word:
                                          \ lfa\name\cfa to jmp (doedoes\pfa to
: ;   [compile] ; rdrop ;                 \ Does>-Part in Target
 immediate restrict
                                          \ Words, created by Redefinition of
                                          \ Defining Words, return their
                                          \ PFA, when executed in Host



\ *** Block No. 38, Hexblock 26

\ redefinition conditionals  bp27jun85we  \ \\

' DO    Alias  DO     immediate restrict  \ Forth-Controllstructures, because
' ?DO   Alias  ?DO    immediate restrict  \ Transient will find words for Target
' LOOP  Alias  LOOP   immediate restrict
' IF    Alias  IF     immediate restrict
' THEN  Alias  THEN   immediate restrict
' ELSE  Alias  ELSE   immediate restrict
' BEGIN Alias  BEGIN  immediate restrict
' UNTIL Alias  UNTIL  immediate restrict
' WHILE Alias  WHILE  immediate restrict
' REPEAT Alias  REPEAT
 immediate restrict













\ *** Block No. 39, Hexblock 27

\ clear Liter. Ascii ['] ."  bp27jun85we  \ \\         Predefinitions
                                          \ Words that must be executable in
Onlyforth Transient definitions           \ Transient

: clear  True abort" There are ghosts" ;

: Literal  ( n -)  H dup $FF00 and
 IF T compile lit ,
 ELSE compile clit c, H THEN ; immediate

: Ascii  H BL word 1+ c@
 State @
  IF T [compile]  Literal H THEN ;
 immediate

: [']     T ' [compile] Literal H ;
          immediate restrict

: "       T compile ("   ," H  ;
          immediate restrict

: ."      T compile (."  ," H  ;
          immediate restrict



\ *** Block No. 40, Hexblock 28

\ Target compilation  ]  [   bp03jul85we  \ \\    main Compileloop

Forth definitions

: tcompile
 ?stack  >in @
 name find ?dup                           \ search for names in Transient and Forth
 IF 0> IF nip execute   >interpret THEN   \ found, execute, if immediate
    drop dup >in !  name THEN             \    else reset  >in
 gfind IF nip execute   >interpret THEN   \ search Ghost and exdcute  cfa
 nullstring? IF drop exit THEN
 number? ?dup                             \ Number?, if yes, execute Literal (T!)
  IF 0> IF swap T [compile] Literal THEN
      [compile] Literal H drop
      >interpret THEN
 drop >in ! Word,                         \ create new ghost and compile forward
 >interpret ; -2 allot                    \ reference

Transient definitions

: ]    H  State on                        \ enable Compiler, set >interpret
 ['] tcompile is >interpret ;             \ from tcompile




\ *** Block No. 41, Hexblock 29

\ Target conditionals        bp27jun85we  \ \\ Conditionals for  Target-Compilation

: IF      T compile ?branch >mark H 1 ;
          immediate restrict
: THEN    abs 1 T ?pairs >resolve H ;
          immediate restrict
: ELSE    T 1 ?pairs compile branch
          >mark swap >resolve H -1 ;
          immediate restrict
: BEGIN   T <mark H 2 ;
          immediate restrict
: WHILE   T 2 ?pairs  2 compile ?branch
          >mark -2  H 2swap ;
          immediate restrict

| : (repeat  T 2 ?pairs  <resolve H
 BEGIN dup -2 = WHILE drop T >resolve H
 REPEAT ;

: UNTIL   T compile ?branch (repeat H  ;
          immediate restrict
: REPEAT  T compile branch  (repeat H  ;
          immediate restrict



\ *** Block No. 42, Hexblock 2a

\ Target conditionals        bp27jun85we  \ \\ Conditionals for  Target-Compilation

: DO     T compile (do  >mark H 3 ;
         immediate restrict
: ?DO    T compile (?do >mark H 3 ;
         immediate restrict
: LOOP   T 3 ?pairs  compile (loop
         compile endloop >resolve H ;
         immediate restrict
: +LOOP  T 3 ?pairs  compile (+loop
         compile endloop >resolve H ;
         immediate restrict














\ *** Block No. 43, Hexblock 2b

\ predefinitions             bp27jun85we  \ \\

: abort"    T compile (abort" ," H ;      \ Immediate-Words for Target
 immediate

: error"    T compile (err"   ," H ;
 immediate




Forth definitions

Variable torigin                          \ origin in target
Variable tudp       0 tudp !              \ udp    in target

: >user  T c@ H torigin @ + ;             \ calculates Address in User-Area from
                                          \ Address of Offset








\ *** Block No. 44, Hexblock 2c

\ Datatypes                  bp27jun85we  \ \\

Transient definitions

: origin!  H torigin !  ;

: user' ( - 8b)   T ' 2 + c@ H ;

: uallot  ( n -) H tudp @ swap tudp +! ;


        Do> >user ;                       \ User-Variable are also executable in
: User  prebuild User  2 T uallot c, ;    \ Transient and return Target-Addresses

           Do> ;
: Create   prebuild Create  ;             \ Witg Create compiled Words are execut-
                                          \ able in Transient and return the
                                          \ pfa.target








\ *** Block No. 45, Hexblock 2d

\ Datatypes                  bp27jun85we  \ \\

           Do> T @ H ;                    \ Also Constant and Variable can be execut
: Constant prebuild Constant  T , ;       \ ed in Transient and rezturn the Target
                                          \ Values
: Variable Create 2 T allot ;


 dummy
: Vocabulary                              \ Vocabularys are executable in transient
 H >in @  Vocabulary  >in !
 T prebuild Vocabulary  0 , 0 ,
 here H tvoc-link @ T ,  H tvoc-link ! ;  \ Vocabulary 'name' created:

          Do>  ;                          \ 1. A Vocabulary Entry in Current with
: Defer   prebuild Defer 2 T allot ;      \    5 fields connected on tvoc
                                          \ 2. A Vocabulary Entry in Current with
: Is  T ' H >body  State @                \    3 Fields connected on tvoc-link
   IF T compile (is  ,                    \ 3. A Ghost
 H ELSE T ! H THEN  ;   immediate






\ *** Block No. 46, Hexblock 2e

\ target defining words      bp27jun85we  \ \\

| : dodoes>
 T compile (;code                         \ creates a Variable-header
 H Glast' @  there  resdoes>
 there tdoes> ! ;                         \ same as Is in Forth

: ;code 0 T ?pairs dodoes>  Assembler
 H [compile] [  redefinition ;
 immediate restrict                       \ creates Code kile ;code in Forth

: does>                                   \ Forwardrefernce for Does>-Part will
 T dodoes>                                \ resolved and tdoes> set for redefinition
 $4C c, compile (dodoes> H ;
 immediate restrict                       \ like ;code in Forth, but with redefiniti
                                          \ on
 dummy
: :  H  tdoes> off  >in @ >in: !
 T prebuild : H current @ context !       \ like Does> in Forth, but with dodoes>
 T ] H 0 ;






\ *** Block No. 47, Hexblock 2f

\ :  Alias  ;                  02oct87re  \ \\

: Alias ( n -- )  H Tlast off
 (theader  Ghost  over resolve            \ disable redefinition
 tlast @ T c@ H 20 or tlast @ T c! ,      \  create small entry in Transient
 H ;                                      \  for with : created words
                                          \  set Context to first fix Voc
: ;  T 0 ?pairs                           \ create Header in Target
 compile unnest [compile] [               \ and resolve forward-refernce with
 H redefinition  ;                        \ Alias-cfa
 immediate  restrict                      \ same as hide in Forth

 dummy                                    \ same as  ; in Forth, and redefinition
: Input:  H  tdoes> off  >in @ >in: !     \ is started
 T prebuild Input:
 H current @ context !  T ] H 0 ;

 dummy
: Output:  H  tdoes> off  >in @ >in: !
 T prebuild Output:
 H current @ context !  T ] H 0 ;





\ *** Block No. 48, Hexblock 30

\ predefinitions             bp03jul85we  \ \\

: compile   T compile compile H ;         \ creates compile as forwardrefernce !!
            immediate  restrict

: Host                                    \ set order to:
 H Onlyforth Ttools also ;                \ Ttools Ttools Forth Only

: Compiler                                \ as Host, order:
 T Host H Transient also definitions ;    \ Transient Transient Ttools Forth Only

: [compile] H Word, ; immediate restrict  \ compils the cfa.target of Ghosts

: Onlypatch H there 3 - 0 tdoes> ! 0 ;    \ Special-Code for Only-Vocabulary

Onlyforth
                                          \ set order for Target-Compilation:
: Target                                  \ Transient Transient Forth Only
 Onlyforth Transient also definitions ;
                                          \ Thanks to Klaus for 'punctuation?' !!!

Transient definitions  Ghost c, drop




\ *** Block No. 49, Hexblock 31



























\ *** Block No. 50, Hexblock 32



























\ *** Block No. 51, Hexblock 33



























\ *** Block No. 52, Hexblock 34



























\ *** Block No. 53, Hexblock 35



























\ *** Block No. 54, Hexblock 36



























\ *** Block No. 55, Hexblock 37



























\ *** Block No. 56, Hexblock 38



























\ *** Block No. 57, Hexblock 39



























\ *** Block No. 58, Hexblock 3a



























\ *** Block No. 59, Hexblock 3b



























\ *** Block No. 60, Hexblock 3c



























\ *** Block No. 61, Hexblock 3d



























\ *** Block No. 62, Hexblock 3e



























\ *** Block No. 63, Hexblock 3f



























\ *** Block No. 64, Hexblock 40



























\ *** Block No. 65, Hexblock 41



























\ *** Block No. 66, Hexblock 42



























\ *** Block No. 67, Hexblock 43



























\ *** Block No. 68, Hexblock 44



























\ *** Block No. 69, Hexblock 45



























\ *** Block No. 70, Hexblock 46



























\ *** Block No. 71, Hexblock 47



























\ *** Block No. 72, Hexblock 48



























\ *** Block No. 73, Hexblock 49



























\ *** Block No. 74, Hexblock 4a



























\ *** Block No. 75, Hexblock 4b



























\ *** Block No. 76, Hexblock 4c



























\ *** Block No. 77, Hexblock 4d



























\ *** Block No. 78, Hexblock 4e



























\ *** Block No. 79, Hexblock 4f



























\ *** Block No. 80, Hexblock 50



























\ *** Block No. 81, Hexblock 51



























\ *** Block No. 82, Hexblock 52



























\ *** Block No. 83, Hexblock 53



























\ *** Block No. 84, Hexblock 54


























