
\ *** Block No. 0, Hexblock 0

\\ Directory 1of4 26oct87re   cas17aug06

.                     0
..                    0
Content               1
Editor-Intro          2
First-Indo            3
Load-System           4
Load-Demo             5
loadfrom              6
simple File           8
help                &10
FORTH-Group         &11
Number Game         &12
buffers             &13
dump                &14
Disassembler        &16
TEST.DIR            &23
savesystem          &26
formatdisk          &27
copydisk            &28
copy2disk           &29




\ *** Block No. 1, Hexblock 1

\\ Content volksForth 1of4    cas17aug06

Directory             0
Content               1
Editor Short Info     2
First Info            3
Load System           4
simple File           8
help                &10
Forth Group         &11
Number-Game         &12
relocate the system &13
dump                &14 -  &15
6502-Disassembler   &16 -  &22
 Test-Folder        &23 -  &25
savesystem          &26
bamallot formatdisk &27
copydisk            &28
2disk copy2disk     &29 -  &30
  free              &31 -  &36
  prg-files         &37 -  &84
Shadows             &85 - &121
  prg-files        &122 - &169

FORTH-GESELLSCHAFT(c)

\ *** Block No. 2, Hexblock 2

  *** volksFORTH  EDITOR Commands
 special Functions:
    Ctrl o  Overwrite   Ctrl i  Insmode
    Ctrl $  .stamp      Ctrl #  .scr#
    Ctrl '  search
 Cursor Control:
    normal Functions, other:
    F7      +tab        F8      -tab
    CLR     >text-end   RETURN  CR
 Char-Editing:
    F5      buf>char    F6      char>buf
    DEL     backspace   INST    insert
    Ctrl d  Delete      Ctrl @  copychar
 Line-Editing:
    F1      newline     F2      killine
    F3      buf>line    F4      line>buf
    Ctrl e  Eraseline   Ctrl r  clrRight
    Ctrl ^  copyline
 Pageing:
    Ctrl n  >Next       Ctrl b  >Back
    Ctrl a  >Alternate  Ctrl w  >shadoW
 Leaving the Editor:
    Ctrl c  Canceled    Ctrl x  updated
    Ctrl f  Flushed     Ctrl l  Loading
FORTH-GESELLSCHAFT (c) 1985-2006

\ *** Block No. 3, Hexblock 3

  You are in Editormode  Screen # 3
     Back to FORTH with RUN/STOP


        *** volksFORTH-83 ***

      Call Editor with
   "l ( n -- )" or with "r ( -- )"

    WARNING! Without FORTH Experience
    work with backup copies of the
    Disks or with write protected Disks

   Some FORTH Words to try outside the
               Editor:
            WORDS   ORDER
              VIEW HELP
          and the C= -Key

       Turn Page back with "Ctrl b"




FORTH-GESELLSCHAFT   (c) bp/ks/re/we/clv

\ *** Block No. 4, Hexblock 4

\ Load a work system           05nov87re

Onlyforth

     2 +load       \ loadfrom
&46 c: loadfrom    \ .blk
  4 c: loadfrom    \ Transient Assemb
&19 c: loadfrom    \ Editor
&26 a: loadfrom    \ savesystem
oldsave
     2 +load       \ loadfrom
  5 c: loadfrom    \ Assembler
&47 c: loadfrom    \ Tracer + Tools
&13 a: loadfrom    \ Buffers








oldsave   \\

FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 5, Hexblock 5

\ Load C64 Demo                21oct87re

(16 .( Nicht fuer C16!) \\ C)

Onlyforth

1 +load   \ loadfrom

limit first @ -   b/buf 8 * -
?\ 8 buffers

\needs demostart : demostart ; 90 allot
\needs tasks        $39 C: loadfrom
\needs help         $A  A: loadfrom
\needs slide        &6  D: loadfrom

1 scr !  0 r# !

Onlyforth

oldsave

\\

FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 6, Hexblock 6

\ getdisk loadfrom             20oct87re

here   $200 hallot  heap dp !

: getdisk  ( drv -- )
 cr  ." Please Insert Disk "
 1+ .
 key drop  .status  cr ;

: loadfrom  ( blk drv -- )
 ?dup 0= IF  load exit  THEN
 flush  getdisk  load
 flush  0 getdisk ;

0 Constant A:       1 Constant B:
2 Constant C:       3 Constant D:

: ?\  ( f -- )   ?exit  [compile] \ ;

                              -->




FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 7, Hexblock 7

\ New save empty clear         20oct87re

' save  Alias oldsave
' clear Alias oldclear
' empty Alias oldempty

: save  state @ IF  compile save  THEN ;
  immediate

: clear state @ IF  compile clear THEN ;
  immediate

: empty state @ IF  compile empty THEN ;
  immediate

dp !






\\

FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 8, Hexblock 8

\ simple filesystem            20oct87re

\needs (search  .( (search missing) \\

' word >body 2+ @ Alias (word

0 Constant folder

' folder >body | Constant >folder

: root   >folder off ; folder

  : directory ( -- addr len )
 folder block  b/blk ;

  : (fsearch ( adr len -- n )
 directory (search
 0= abort" not found"
 folder block -  >in push  >in !
 BEGIN  bl directory (word capitalize
        dup c@ 0= abort" exhausted"
        number? ?dup not
 WHILE  drop  REPEAT  0< ?exit  drop ;

-->

\ *** Block No. 9, Hexblock 9

\ simple Filesystem            20oct87re

: split
 ( adr len char -- adr2 len2 adr1 len1 )
 >r 2dup r@ scan  r>
 over >r  skip  2swap  r> - ;

: read  ( -- n ) \ /path/file
 bl word count dup 0= abort" What?"
 pad place  pad count
 BEGIN  Ascii / split
  dup IF    (fsearch
      ELSE  nip root    THEN  over
 WHILE  >folder +!  REPEAT
 -rot 2drop folder + ;

: ld  read load ;      \ LoaD
: sh  read list ;      \ SHow
: ed  read l ;         \ EDit
: cd  read >folder ! ; \ Change Dir
: ls  folder list ;    \ LiSt Dir



FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 10, Hexblock a

\ help                        14oct85re)

Onlyforth

: help  ( --)
 3 l                \ list Scr # 3

 cr ." Additional Help can be"
 cr ." found on the Net"
 cr ." or in a local FORTH User Group"
 cr ." FORTH-Gesellschaft"
 cr ." www.forth-ev.de" cr ;

       \ print silly text








\\

FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 11, Hexblock b



























\ *** Block No. 12, Hexblock c

\ numbers                     05jul85re)

decimal             \ sorry, but this
                    \ is for YOU !

: alphabetic  ( --)  &36 base ! ;

hex                 \ Ah, much better


\ Look at this:


31067E6.  alphabetic d.       hex
19211D5.  alphabetic d.       hex
   -123.  alphabetic d.       hex


\ Try to explain !



\\

FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 13, Hexblock d

\ relocating the system        20oct87re

| : relocate-tasks  ( newUP -- )
 up@ dup
 BEGIN  1+ under @ 2dup -
 WHILE  rot drop  REPEAT  2drop ! ;

: relocate  ( stacklen rstacklen -- )
 swap  origin +
 2dup + b/buf + 2+  limit u>
  abort" buffers?"
 dup   pad  $100 +  u< abort" stack?"
 over  udp @ $40 +  u< abort" rstack?"
 flush empty
 under  +   origin $A + !        \ r0
 dup relocate-tasks
 up@ 1+ @   origin   1+ !        \ task
       6 -  origin  8 + ! cold ; \ s0

: bytes.more  ( n -- )
 up@ origin -  +  r0 @ up@ - relocate ;

: buffers  ( +n -- )
 b/buf * 2+  limit r0 @ -
 swap - bytes.more ;

\ *** Block No. 14, Hexblock e

\ dump utility                30nov85re
\ adapted from F83 Laxen/Perry

| : .2  ( n --)
 0 <# # # #> type space ;

| : D.2  ( adr len --)
 bounds ?DO  I c@ .2  LOOP ;

: dln  ( adr --)  \ DumpLiNe
 cr  dup 4 u.r  space  dup 8 D.2
 ." z "  8 bounds DO  I c@ emit  LOOP ;

| : ?.n  ( n0 n1 -- n0)
 2dup = IF  rvson  THEN
 2 .r  rvsoff  space ;

| : ?.a  ( n0 n1 -- n0)
 2dup = IF  rvson  THEN  1 .r rvsoff ;

-->



FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 15, Hexblock f

\ dump utility                30nov85re
\ adapted from F83 Laxen/Perry

| : .head  ( adr len -- adr' len')
 swap  dup $FFF0 and  swap $F and
 2 0 DO  cr 5 spaces
  I 8 * 8 bounds DO I ?.n LOOP 2 spaces
  I 8 * 8 bounds DO I ?.a LOOP
 LOOP  rot + ;

: dump  ( adr len --)
 base push  hex  .head
 bounds ?DO  I dln  stop? IF LEAVE THEN
             8 +LOOP cr ;

: z  ( adr n0 ... n7 --)
 row 2- >r  unlink
 8 pick 7 + -7 bounds
 DO  I c!  -1 +LOOP r> 0 at dln  quit ;


clear




\ *** Block No. 16, Hexblock 10

\ disassembler 6502 loadscr    06mar86re

Onlyforth

\needs Tools Vocabulary Tools

Tools also definitions hex

| : table    ( +n -- )
 Create     0 DO
 bl word number drop , LOOP
 Does> ( 8b1 -- 8b2 +n )
 + count swap c@ ;

-->











\ *** Block No. 17, Hexblock 11

\ dis shortcode0               20oct87re

base @  hex

$80 | table shortcode0
0B10 0000 0000 0341 2510 0320 0000 0332
0AC1 0000 0000 03A1 0E10 0000 0000 0362
1D32 0000 0741 2841 2710 2820 0732 2832
08C1 0000 0000 28A1 2D10 0000 0000 2862
2A10 0000 0000 2141 2410 2120 1C32 2132
0CC1 0000 0000 21A1 1010 0000 0000 2162
2B10 0000 0000 2941 2610 2920 1CD2 2932
0DC1 0000 0000 29A1 2F10 0000 0000 2962
0000 0000 3241 3141 1710 3610 3232 3132
04C1 0000 32A1 31B1 3810 3710 0000 0000
2051 1F51 2041 1F41 3410 3310 2032 1F32
05C1 0000 20A1 1FB1 1110 3510 2062 1F72
1451 0000 1441 1541 1B10 1610 1432 1532
09C1 0000 0000 15A1 0F10 0000 0000 1562
1351 0000 1341 1941 1A10 2210 1332 1932
06C1 0000 0000 19A1 2E10 0000 0000 1962

base !

-->

\ *** Block No. 18, Hexblock 12

\ dis scode adrmode            20oct87re

| Create scode
 $23 c, $02 c, $18 c, $01 c,
 $30 c, $1e c, $12 c, $2c c,

| Create adrmode
 $81 c, $41 c, $51 c, $32 c,
 $91 c, $a1 c, $72 c, $62 c,

| : shortcode1 ( 8b1 - 8b2 +n)
 2/ dup 1 and
 IF  0= 0  exit  THEN
 2/ dup $7 and adrmode + c@
 swap 2/ 2/ 2/ $7 and scode + c@ ;

| Variable mode

| Variable length

-->





\ *** Block No. 19, Hexblock 13

\ dis shortcode texttab        06mar86re

| : shortcode ( 8b1 -- +n )
 dup 1 and         ( odd codes)
 IF  dup $89 =
  IF  drop 2  THEN  shortcode1
 ELSE  shortcode0  ( evend codes)
 THEN
 swap dup 3 and length !
 2/ 2/ 2/ 2/ mode ! ;

| : texttab   ( char +n 8b -- )
 Create
 dup c, swap 0 DO >r dup word
 1+ here r@ cmove r@ allot r>
 LOOP 2drop
 Does>  ( +n -- )
 count >r swap r@ * + r> type ;

-->






\ *** Block No. 20, Hexblock 14

\ dis text-table               06mar86re

bl $39 3 | texttab .mnemonic
*by adc and asl bcc bcs beq bit bmi bne
bpl brk bvc bvs clc cld cli clv cmp cpx
cpy dec dex dey eor inc inx iny jmp jsr
lda ldx ldy lsr nop ora pha php pla plp
rol ror rti rts sbc sec sed sei sta stx
sty tax tay tsx txa txs tya
( +n -- )

Ascii / $E 1 | texttab .before
   / /a/ /z/#/ / /(/(/z/z/ /(/


Ascii / $E 3 | texttab .after
     /   /   /   /   /   /,x
 /,y /,x)/),y/,x /,y /   /)  /

-->






\ *** Block No. 21, Hexblock 15

\ dis 2u.r 4u.r                06mar86re

: 4u.r ( u -)
  0 <# # # # # #> type ;

: 2u.r ( u -)
  0 <# # # #> type ;

-->

















\ *** Block No. 22, Hexblock 16

\ dis                          20oct87re

Forth definitions

: dis   ( adr -- ) base push hex
BEGIN
 cr dup 4u.r space dup c@ dup 2u.r space
 shortcode >r length @ dup
 IF over 1+ c@ 2u.r space THEN dup 2 =
 IF over 2+ c@ 2u.r space THEN
 2 swap - 3 * spaces
 r> .mnemonic space 1+
 mode @ dup .before $C =
 IF dup c@ dup $80 and IF $100 - THEN
  over + 1+ 4u.r
 ELSE length @ dup 2 swap - 2* spaces
  ?dup
  IF 2 =
   IF   dup  @ 4u.r
   ELSE dup c@ 2u.r
 THEN THEN THEN mode @ .after length @ +
 stop?  UNTIL drop ;


Onlyforth clear

\ *** Block No. 23, Hexblock 17

\\ Subdirectory test.dir       26oct87re

.                    0
..                -&23
all-words            1
free                 2




















\ *** Block No. 24, Hexblock 18

\ pretty words                 26oct87re

: .type  ( cfa -- )   dup @ swap 2+
             case? IF ." Code" exit THEN
 ['] :     @ case? IF ."    :" exit THEN
 ['] base  @ case? IF ." User" exit THEN
 ['] first @ case? IF ."  Var" exit THEN
 ['] limit @ case? IF ."  Con" exit THEN
 ['] Forth @ case? IF ."  Voc" exit THEN
 ['] r/w   @ case? IF ."  Def" exit THEN
 drop ." ????" ;

: (words  ( link -- )
 BEGIN  stop? abort" stopped"  @ dup
 WHILE  cr dup 2- @ 3 .r space
        dup 2+  dup name> .type space
        .name  REPEAT drop ;

: all-words ( -- )
 voc-link
 BEGIN  @ ?dup
 WHILE  dup 6 - >name
        cr cr .name ."  words:" cr
        ." Blk Type Name "
        dup 4 - (words  REPEAT ;

\ *** Block No. 25, Hexblock 19



























\ *** Block No. 26, Hexblock 1a

\ savesystem                   23oct87re

| : (savsys ( adr len -- )
 [ Assembler ] Next  [ Forth ]
 ['] pause  dup push  !  \ singletask
 i/o push  i/o off  bustype ;

: savesystem   \ name must follow
    \ prepare Forth Kernal
 scr push  1 scr !  r# push  r# off
    \ prepare Editor
 [ Editor ]
 stamp$ dup push off
 (pad   dup push off
    \ now we save the system
 save
 8 2 busopen  0 parse bustype
 " ,p,w" count bustype  busoff
 8 2 busout  origin $17 -
 dup  $100 u/mod  swap bus! bus!
 here over - (savsys  busoff
 8 2 busclose
 0 (drv ! derror? abort" save-error" ;

Onlyforth

\ *** Block No. 27, Hexblock 1b

\ bamallocate, formatdisk      20oct87re

: bamallocate ( --)
 diskopen ?exit
 pad &18 0 readsector 0=
  IF pad 4 + $8C erase
     pad &18 0 writesector drop
  THEN  diskclose
 8 &15 busout " i0" count bustype
 busoff ;

: formatdisk ( --)  \ name must follow
 8 &15 busout " n0:" count bustype
 0 parse bustype busoff
 derror? ?exit
 bamallocate ;

\ example: formatdisk volksFORTH,id






FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 28, Hexblock 1c

\ copydisk                    06jun85we)

| Variable distance

limit first @ - b/buf /  | Constant bufs

| : backupbufs  ( from count --)
 cr ." Insert Source-Disk" key drop cr
 bounds 2dup DO  I block drop  LOOP
 cr ." Insert Destination-Disk"
 key drop cr
 distance @ ?dup
 IF    >r  swap 1- over  r> +  convey
 ELSE  DO  I block drop update  LOOP
       save-buffers THEN ;

: copydisk  ( blk1 blk2] [to.blk --)
 2 pick - distance !  1+ over -
 dup 0> not Abort" RANGE ERROR!"
 bufs /mod ?dup
 IF swap >r 0
    DO dup bufs backupbufs bufs +  LOOP
    r> THEN
 ?dup IF backupbufs ELSE drop THEN ;


\ *** Block No. 29, Hexblock 1d

\ 2disk copy2disk..           clv14jul87


$165 | Constant 1.t
$1EA | Constant 2.t
$256 | Constant 3.t


| : (s#>s+t ( sector# -- sect track)
      dup 1.t u< IF $15 /mod exit THEN
( 3+) dup 2.t u< IF 1.t - $13 /mod $11 +
                            exit THEN
      dup 3.t u< IF 2.t - $12 /mod $18 +
                            exit THEN
                    3.t - $11 /mod $1E +
 ;


| : s#>t+s  ( sector# -- track sect )
 (s#>s+t  1+ swap ;




-->

\ *** Block No. 30, Hexblock 1e

\ ..2disk copy2disk           clv04aug87


| : ?e ( flag--)
  ?dup IF ." Drv " (drv @ . diskclose
          abort" " THEN ;

| : op ( drv#--) (drv ! diskopen ?e ;

: copysector \ adr sec# --
  2dup
  0 op s#>t+s readsector  ?e diskclose
  1 op s#>t+s writesector ?e diskclose ;

: copy2disk \ -- \ for 2 Floppys
 pad dup $110 + sp@ u> abort" no room"
 cr ." Source=0, Dest=1" key drop cr
 base push decimal      0 &682
 DO I . I s#>t+s . . cr $91 con!
    dup I copysector   -1 +LOOP drop ;

: 2disk1551 \ -- switch 1551 to #9
 flush 8 &15 busopen " %9" count bustype
 busoff derror? drop ;


\ *** Block No. 31, Hexblock 1f

\ nothing special here

























\ *** Block No. 32, Hexblock 20



























\ *** Block No. 33, Hexblock 21



























\ *** Block No. 34, Hexblock 22



























\ *** Block No. 35, Hexblock 23



























\ *** Block No. 36, Hexblock 24



























\ *** Block No. 37, Hexblock 25

   F † ± Å H± † ë l  o ¨# VARIABLER#A!F ˝
 \ p |# UALLOTR#∏ 0 " ˜
  ˇ[ ‡+
Userarea
full0 " ã 0 [ \ p Û# USERR#A!F ¸#  Î • 8È
 Ö ∞ F † ±  E Å äHE † ë l  p )$ ALIASR#A!
~ " ∏ J    1 Ñ   / ˛ˇ˝ B      Z S"; \ q ]
$ VP."  |%K%K% FK%    q ë$ CURRENT."K%q ™
$ CONTEXTR  " ¥$; \ R#" Ö ©" #\ s ±% ORDE
RR#R$I   % K%/ ˛ˇ˚ π F _,¥$K%\ s Y% WORDS
R#D$" " ∏ °6ô 1 Ñ   π6∏ a  #ë,B ÊˇÄ \ t
& (FIND;&† ± ô$ à ¯°&) Ö(† ±$™H±$Ö%Ü$ $P
† ¢ lå H±$) E(P‡ © E$Ö)© E%Ö*_(±&Q)PKàP˜†
 •*ë à•)ë à¢ lâ ñ&° Ö$± Ö%°$Ö&) 8E$Ö$ê Ê%
•&) P •$Å •%lB&°$Å ±
¥$" "     J ∏ º
Ø â ‡+ invalid nameX ~ ; # ˝ à!^!" Ñ   º
^![ ∏  !  û    Z  !L ; B   &!Ä ¥ °   Î l
 3"° Ö(± Ö)•  I Ö ê Ê ° Ö&± Ö' &P lå ±&ë
Ö%°&Å Ö$ %P l  •$ I Ö$ê Ê%°$h8) E$Ö$ê Ê%H
)  ±$h°$Ö$HÖ%•$E(P∞•%E)P™lc n ∏! >NAMER#
% " ∏ Ñ   È
Z I ã 1"} Ñ   z
~
  ' NOTFOUN
DI('x æ'
NO.EXTENSIONSR#˙+ Haeh?\ x O' I
NTERPRETR#∏'\  R#û)  ı&} Ñ   º 1 Ñ   ó ∏'
‡+ compile onlyÉ'S p ô Ñ   I'∏'\ R#û)  ı&
} Ñ   < Ñ   ó ∏'  ∏'É'S p } Ñ   < Ñ   ã ^
 ^ B   I'∏'\ y Ì'a[R#/  („(∫'1 ˜ \ y {( ]
R#/ ;(„(∫'1 Â \ R#ì ‡+ Crash\ z ë( DEFER

\ *** Block No. 38, Hexblock 26

  RSIVER#¥ \ R#Ç Ñ   |
J   Ù ‰ Ä \ h H  I
MMEDIATER#  @Z \ h Ú  RESTRICTR#  ÄZ \ i
   CLEARSTACK. † ± Ö H± Ö † l  i    HALLO
TR#∏ " Ù I ã ; a ∏
I ∏ ∏ ; F ç
Ù I , ,
∏ ; \ i @  HEAPR#∏ " z \ i Y  HEAP?R#Ä }
Ø \ R#∏ X Ù I ∏ i Ä ã D Ä Ù I ~ [ ¥ \ • 8
È Ö ∞ F •    ± E H± Â † ê l   Ü ì ‡+ stac
k empty\ \ ï) .STATUSI(Nb."Ô Ô ; \ \ „)ÑP
USHR#Ô ã ∏ P " P Ò)P P \ \ ˝) LOADR#} ô S
 D  *D ; ∏  *∏ ˜ Ì)˘'\ ]  * +LOADR#D " ˜

#*\ ] c* THRUR## ã I   % #*J π \ ] y* +TH
RUR## ã I   % k*J π \ ] V*c-->R#º D [ ∏ ˜
 Ì)\ ] î* RDEPTHR#B   VERTR## L J Ñ   ≠ B
 ÙˇG \ R#º " ô \ R#L ì º [ \ R#G L \ f M
 DPL."ˇˇR#Ñ   9 ~
Ä 9 ° \ R#Ñ   9 Ä Ô Ñ
 µ
Ä   " # } S Ä ì \ R#  &@ Ñ      ì d
  $@ Ñ      ì d   H@ Ñ      ì d   %@ Ñ
F ì d ° \ R#  ,Ù I ã   .I   \ R#  " ì I S
 º   [ \ ."  g
  NUMBER?R#Ê    ,π6D " }
Ñ   é+; ∏ " ô+; 3+\ _ ü+á(ABORT"R#X ã Ñ
 P , Ô   ª d Ä \ R#X ã Ñ     ª d Ä \ _ V+
FABORT"R#6 ‡+\ \ _  ,FERROR"R#6 ˙+\ \ ` %
, BLø#  ` :, -TRAILINGs,ò S ° Ö& ± E%Ö'_$
 ê à±&I  HP Ê%òh•%l  òPÍF'F% ‰òl  A e, S
PACER#?,
7\ A â, SPACESR#° |   ë,J π \ R

\ *** Block No. 39, Hexblock 27

  C _- .R#  ©-\ C µ- U.R#° ©-\ D C- .SR#;
 ∏ " Ù I      £ |   % " H-F ˚ π \ D R- C/
Lø#) D ˛- L/Sø#  D  . LISTR#é+; Ø  Scr é+
" ∏  <x H-Ø  Dr x<π- .° I . °6Ñ   Ñ  7% F
 Ñ-ë,é+" ∑1%  .$ ˜
 .G q,#7J π  7\ E  . P
AUSE  E Z. LOCKR#∏ " } I Ñ   Ä d ∏ " Ñ
Ç.B Ùˇ} ã   /ë(•(ç /•)ç /lC/J  / (DISKERR
R#Ø  error !  r to retry Ø7∏   rI ã   RI
  â ‡+ aborted\ J A/ DISKERRI(L/J  0 R/WI
(∂?R#∏ z " É Ñ j a ∏ " # Ñ 3    *Ù  *q+∏
z Ù a " F ç
" °  0Ñ   Ø  write  0B {ˇ  ÄÙ
 t #   Ä \ R#a ∏ Â t ˜ \ R#∏ {0   *Ù  *q+
P Ù Z " ˜
Ù      ˜
z  #ñ$∏ " ˜
a \ R#ñ$a
D$\ q ∫$ ALSOR#ñ$"    - ˙+ Vocabulary sta
ck fullD$" F ñ$[ D$; \ q ~$ TOSSR#/ ˛ˇñ$[
 \ r  % VOCABULARYR#A!°   °   X % "   % ;
 Î l@ D$; \ r /% FORTHv%v{v{  r A% ONLYÑ%
!8!8O%Î l@ ° ñ$; D$; Â$\ r S% ONLYFORTHR#
Z%I%Â$ø%\ s ó% DEFINITIONSR#D$  $ë • 8È Ö
 ∞ F •&  H 0 òiˇ®HòÅ   ©ˇ$ä† ë l  v 1& FI
NDR#D$∏ " Ù Ö " I Ñ   Ö |
" 9&Ñ   J
î&d Ù
 ñ$a [ Ñ   ã Ö B ~ˇJ
° \ v Ó& 'R#  ı&ô ‡+
 Haeh?\ v 7'I[COMPILE]R#;'  \ v o'C[']R#;
'^ \ v E' NULLSTRING?R#∏ J ô ∏ Ñ   J
\ H
± I Ö H± I Ö † l  w U' >INTERPRETï' (x ´

\ *** Block No. 40, Hexblock 28

  d B ‡ˇJ
\ R#L    1 ˜
\ n °" NAME>R#∏ S"
ã J    1 Ñ   " \ n ‚" >BODYR#a \ n  # .NA
MER#} Ñ   ∏ î Ñ   Ø  |L    1 #7B   Ø  ???
ë,\ o  # CREATE:R#A!ï ¥$" D$; ï(° \ o h#
:R#r#Î • 8È Ö ∞ F • Å • ë •  I Ö äE Ö l
o H#A;R#° ˝ 6 \ (¥ \ o ñ# CONSTANTR#A!
Î • 8È Ö ∞  R#A!/ •(  Î † ± hH± Ö HÖ † l
 z µ( (ISR#Ô ∏ a P " ; \ R#" / I'" Ù I ã
/ ∏'" I   â ‡+ not deferred\ z }(bISR#;'∏
 Û( #1 " Ñ   6 „(  d ; \ ." R#O*   - ‡+ t
ight stacke)J ô Ñ   ì e)‰ ì ‡+ dictionary
 fullØ  still full \ [ ") ?STACK†)† 8• Ò
H• Ò P †  Ü h)\ ég)†  ë • Å  ° I Ö äQ Ö ©
  E häE l  R#Ô ~ " Í"; \ j å EDOES>R#6 Î
  l  6 @ \ R#∏   ˇ1   ˇI I \ R#X   ˇ1   ˇ
I Ñ   ∏ ∏ # X Ù I # Ú º ~ [ º ˝ \ k ˚  ?H
EAD."  k v! |R#^!" S ì ^!; \ l D! WARNING
."  R#Ñ!" S ~ " ¥$" 9&J
Ñ   ë,~ "  #Ø  ex
ists π6\ l Z! CREATER#X D "     " ≥ a I
 \ ] ¨* DEPTHR#; ∏ " ã I   \ R#1 " Ñ   Ø
  compilingd Ø   ok\ ^ G* (QUITR#Ì) 7Ô ˘'
*B Ùˇ\ ^  + 'QUITI( +^  + QUITR#B " B (
&+\ ^ ,+ STANDARDI/OR#/ * Ù Z D \ ^ c+ 'A
BORTI(† ^ A+ ABORTR#, C J+q+3+\ _ P+ SCR.
"  _ à+ R#."  _ î+ (ERRORR#q+ë,X  #L #7ë

\ *** Block No. 41, Hexblock 29

  *∏ L º ;   Â ° P ° °
Ì   ˜   -@ Ñ   9
 ì P Ì   ˜ o Ñ   Ê ; Ì   ˜ J ô   ≠ ® Ì )
˜ J ô Ñ ˇ  î ô     ˜ Ì ) ˜ B Tˇ\ g B  'N
UMBER?I(L g e  NUMBERR#p } ô ‡+ ?É Ñ
\ h v  LAST."  R#~ " } \ h W  HIDER#Ç Ñ
 Ö " ¥$" ; \ h é  REVEALR#Ç Ñ   Ö ¥$" ; \
 h ´ IRECU  #Ë Ö \ B õ, HOLDR#ì ¥,[ ¥," ‰
 \ B æ, <#R#¥,¥,; \ B W, #>R#~
¥," ¥,Ù I
\ B Ë, SIGNR#É Ñ     -E,\ B ˇ, #R#Ê " _

   Ù U Ñ     á˜
  0˜
E,\ B  - #SR# -È
Y
Ñ ¯ˇ\ C @- D.RR#z
|
) |,e-
 -Ì,
Ù } Ù I
 _,#7\ C u- .RR#ã
[-\ C - U.RR#° ã [
-\ C í- D.R#° [-ë,\   ; \ E Ü. UNLOCKR#∏
ç.˜ \ H8È Ö HÈ Ö ©lÅ † ± Ö H± Ö † ° Ö ± Ö
 •  I Ö ê Ê ¢ lê F ∑. FILE<$ F  / PREV."P
ß."  F  / B/BUFø#  † ±(Y" P H@ PÙ`:/† ± ô
$ à ¯†  ± E&Ö&H± E'Ö'≠ /Ö(≠ /Ö) )/P •  I
Ö ê Ê † ©  E(Å •)I ë lf •(Ö*•)Ö+°*Ö(† ±*Ö
) (P l   )/P‚°(Å*† ±(ë*≠ /Å(≠
º  0Ñ
Ø  read  0B YˇÔ \ R# /∏ " Ñ   " ∏ a " ì I
 Ñ Íˇ /ç.∏ !0\ R#a P È
  ; Z " ˜
  a ; Ô
t ˜  /@.\ R# /" ∏ Ñ   ∏ z " É Ñ Óˇ\ R# /"
 ∏ " ô Ñ ˆˇz " É \ L  0 CORE?R#8/~
° \ M
f1 (BUFFERR#8/H0Ó0B ¯ˇ\ M z1 (BLOCKR#8/H0
â0Ó0B ˆˇ\ è1† ± hH± l  M T1 BUFFERR#ç1D1

\ *** Block No. 42, Hexblock 2a

   0ï° E$± Â%lá ' Q  U<  ° Ö$± Ö%•  I Ö ê
 Ê ° E$± Â%∞ lâ lå ( ˝  >R#ã U \ ( )  0>R
#_ É \ ( 7  0<>R#ô â \ ( f  U>R#ã   \ ( v
  =R#I ô \ ( E  D0=R#  ô \ ( S  D=R#µ ı Y
 \ ( É  D<R#
È
I Ñ   - J
J
B   ~
  \ R#9
 Ñ   ã Ä \ ) î  MINR#È
- µ \ ) E  MAXR#È

U µ \ ) W     D+˜ ©  S H±  E&ë H± E'ë ° E
$Å † ± E%ë l  #   1+% ©  A ∞ l  Å ± I ë
l  #    2+c © P‡# <  3+p © PSv © PM\ © PG
# i  1-I 8° È ê l  Å ± È ë l  # B  2-á  ê
‡$ Ä  TRUEø#ˇˇ$ å  FALSEø#  $ ô "-1ì $ ß
!0° $ ∞  1ø#  $ ∏  2ø#  $ B  3ø#  $ L  4ø
#  $ V  ONR#ì ã ; \   IBR#\ " \ ; W  QUER
YR#}   pW7Q " o ; ∏ ˜ D ˜ \ < Á  SCANR#P
∏ Ñ   Ù J   I Ñ   G ã # ã B ‰ˇ9 \ <    SK
IPR#P ∏ Ñ   Ù J   I Ñ   G ã # ã B ‰ˇ9 \ <
 ;  /STRINGR#Ù
Ù ˜
z
I \ Iaê I{∞ IBê
)I[∞  Ä`= J  CAPITAL© °  Ü Å l  > ù  CAP
ITALIZED ° Ö$± Ö%°$Ö&àD&P † l
 • 8È Ö ∞
 F † ± Å H± † ë l    Ì  ROT
† ± Ö%† ± †
ë † ± Ö$•%ë † •$ë H± Ö%° ë † ± Å •%ë † l

 -ROTR#

\   s
 NIPR#ã Ä \   D
 UN
DERR#ã Ù \   T
 PICKR## 3 ; ˜
" \   Ü
 RO
LLR#∏ P ç
; ∏ a Ô # 3 Ú Ä \   ù
 2SWAPR#

P
Ô \   @
 2DROPÙ   V
 2DUPR#Ù Ù \

\ *** Block No. 43, Hexblock 2b

  ÏÊ)Ê'lL 7 º  CMOVE>Ù ©  S  •%E'Ö' •%E)Ö
)Ê%_$ ê à±(ë&òP¯F'F)F%P† l  7 È  MOVER#P
 È
  Ñ   Ô Ú d Ô D \ 8 %  PLACER#Ù P
Ù
# Ô , ‰ \ 8 f  COUNTN ° Ö$ I Å ± Ö%I ë •
8È Ö ∞ F lT 8 D  ERASER#° ® \ 9 è  FILL™
©  S à•$¶' ë(HP˚Ê)JPˆ¶& ë(HJP˙† l  : °
 HERER#L "   F ° Å ± ë ©  E Ö ê Ê l    Í
 R@  • 8È Ö ∞ F ± ë ° l      ÖRDROP    1
 EXITf ° Ö ± Ö l    =  UNNEST^ ° Ö ± Ö l
   s  ?EXITU °    •  I Ö ê Ê (Pæl    K  E
XECUTEô ° Ö ± Ö •  I Ö ê Ê l    ç  PERFOR
MR#" ó \   ±  C@L ° Ö$± Ö%© ë °$l    E  C
!Ê ° Ö$± Ö%H± Å$à•    %Å l  3 ò  M*R#∏ É
∏ P Ñ   _ ã ∏ É Ñ   _ Ô â P û Ô Ñ   µ \ 3
 Î  *R#û Ä \ 3    2*5 °  Å ± *ë l  R#ì ‡+
 division overflow\ 4 .  UM/MODI ° Ö)± Ö(
•  I Ö ê Ê ° Ö%± Ö$H± Ö'H± Ö&¢  F*8•%Â)®•
$Â(∞ &*ê Ö$Ñ%&'&&&%&$JP† F$F%ê  Ü b \ •&
ë H•%ë H•$ë † •'l  5 ^  M/MODR
                        ." ultraFORTH-83
3.80-C64  çˇˇ± Ö ≠ˇˇÖ  • I Ö ∞ LˇˇÊ ∞˘
  END-TRACEE ©•Ö © Ö ©IÖ © Ö l    w  RECO
VER."HÖ HÖ • P F F lR#  Z  NOOP    ô  ORI
GINø#    _  S0<$   ≥  R0<$   Ω  DP<$   G
 OFFSET<$   Q  BASE<$     OUTPUT<$   Î

\ *** Block No. 44, Hexblock 2c

  ER#X I   \ . }  ?PAIRSR#I ‡+ unstructur
ed\ I   h©ˇl  h© ™• 8È Ö ∞ F ä† ë H¢ l  /
 Ù  CASE?b ©  S •$A P •%Q P lâ äl  0 8 BI
FR#6 Ñ ¶ º \ 0 \ DTHENR#= º ˝ Ω \ 0 O DEL
SER#º ˝ 6 B ¶ ã Ω ì \ 0 Ñ EBEGINR#U F \ 0
 ° EWHILER#F ˝ F 6 Ñ ¶ / ˛ˇH
\ R#Ë ∏ / ˛ˇ
I Ñ   Ä Ω   DR#µ Â \ b Q  PARSER#P µ ∏ "
T Ù ã Ô   P Ù I ∏ Ô l I ∏ [ \ b ‚  NAMER#
?,X B d \ c    STATE."  c ) eASCIIR#?,X #
 J 1 " Ñ   ^ \ c 7  ,"R#  "Í X Ù # ˝ n \
c w Ñ"LITR#Ô Ô |
L ˜
P P \ c Q Ç("R#X \ c
 å A"R#6 ë \ \ d ô É(."R#X L #7\ d © B."R
#6 Ø \ \ d ª a(R#  )   UMAXR#È
  µ \ ) È
 UMINR#È
[ µ \ ) ¸  EXTENDR#∏ É \ )    DA
BSR#  Ñ   µ \ ) "  ABSR#  Ñ   _ \ R#9 Ô a
 ∏ P
P ã P P \ * 7 É(DOR#Ù I i \ * C Ñ(
?DOR#Ù I } Ñ   i Ô ∏ " ˜
P Ä \ * U  BOUND
SR#Ù ˜
ã \ * ö áENDLOOPª © l  + Ø Ö(LOOPL
  © A Å ê ± I ë ê l  † ± Ö à±   $ ‡  OFFR
#° ã ; \ % Ò ÑCLIT  • 8È Ö ∞ F ° Å äë Ê P
 Ê l  %   ÉLIT1 • 8È Ö ∞ F ± ë ° Å •  I Ö
 ê Ê l  % ) GLITERALR#∏ /  ˇ1 Ñ   6 /   d
 6     \ & t  0<Ö ±   ©ˇ$äë l  & ~  0=õ °
   ËPÈ& î  UWITHIN± ©  S † ° E$± Â%∞ ° E
&± Â'∞Ωlå ' •  <W ° Ö$± Ö%•  I Ö ê Ê •%q

\ *** Block No. 45, Hexblock 2d

   H±$ Ü ë$lQ Á Ü*† ± ô$ à ¯†  ≠∫ E&Ö&≠ª
E'Ö'8•$Ì∫ Ö$•%Ìª Ö%∞
° ç∫ ± çª lY † •$ %
=°&Q P Ê&P Ê'•$P F%F$l$ •&Ö(•'Ö)°&Q  Ê&P
Ê'•$P F%F$( Ê*•$ %P‚8† •&Ò ç∫ H•'Ò çª  ©
 E Ö ê Ê † ± Å Ö$H± † ë Ö%à•*ë$±(HF* ˜© ë
$† l  b µ  SOURCER#D " } Ñ   ∑1Ô;d } o "
\ b ¨  WOR  ‚
 +˘
 H° Q ë à± † Q ë lô   Û

 OR  H°   ë à± †   ë lô      AND3 H° 1 ë
 à± † 1 ë lô   +  XORp H° q ë à± † q ë lô
 ! h  -K H± 8· ë H± † Ò † ë lô ! E  NOTã
 ä· Å äÒ ë l  ! É  NEGATE¶ 8∞„" õ  DNEGAT
E∑ H8äÒ ë HäÒ ë ä· Å † äÒ ë l   ™®à± ô$ à
 ¯ä E Ö ê Ê ¢ † `" ´   \ : Q  PADR#X   b˜

\ : ‚  ALLOTR#L [ \ : ı  ,R#X ; F ˝ \ :
   C,R#X ‰ º ˝ \ :   áCOMPILER#Ô ∏ a P "
  \ ; ,  #TIB."  ; h  >TIB."` savesystem
@:c64demo
                            ; u  >IN."  ;
 ≤  BLK."  ; æ  SPAN."  ; J  T  I Ö ê Ê l
      CTOGGLER#|
J n ã ‰ \      @$ ° Ö$±
 Ö%±$ë °$l       != ° Ö$± Ö%H± Å$H± † ë$l
Ù   7  +!] ° Ö$± Ö%H±  A$Å$H± † Q$lo   v
 DROPù   Y  SWAPç ± ™† ± Ö$äë •$† ë H¢ ±
Ö$° ë à•$l    Ñ  DUP∫ • 8È Ö ∞ F † ± † ë
H± àl    ≤  ?DUP °   P l  l∫   V  OVERˆ

\ *** Block No. 46, Hexblock 2e

  #∏ P = Ù É Ñ   |
˜
ã G   É Ñ   _ Ù Ñ
ã   ˜
ã G 9 \ 5 X  2/  ±  ± Jë ° Jl  6
 /MODR#P   Ô ‡ \ 6 /  /R#6 J
\ 6 d  MODR#
6 Ä \ 6 r  */MODR#P  Ô ‡ \ 6 B  */R#J J

\ 6 X  U/MODR#° ã G \ 6 á  UD/MODR#P °
G Ô ã P G Ô \ 7 õ  CMOVEF ©  S àD$P F%  †
 l  ±(ë&HP   INPUT<$   ˘  ERRORHANDLER<$
     VOC-LINK<$      UDP<$   *  SP@= • Ö$
• Ö%¢$• 8È Ö ∞ F µ ë µ ¢ l    5  SP!G ° ™
± Ö Ü ¢ l    _  UP@ ¢ lg   W ÉUP!é ¢ ± ï
 à± ï ¢ † •  I Ö ê Ê l    Ü  RP@µ ¢ lg
≠ ÉRP!D ¢ lê   º Ç>RR • 8È Ö ∞ F ° Å ± ë
lù   K ÇR>Ò • 8È Ö ∞  B Óˇ\ 0 ≥ FREPEATR#
F ˝ 6 B Q \ 0 Ì EUNTILR#F ˝ 6 Ñ Q \ 1   B
DOR#6 I ¶ P \ 1   C?DOR#6 | ¶ P \ 1 1 DLO
OPR#P ˝ 6 J 6 π Ω \ 1 e E+LOOPR#P ˝ 6 ˚ 6
 π Ω \ 1 ` ÖLEAVER#π Ô Ö ∏ " ˜
P \ 2 |  U
M*† ± Ö$° Ö%HÜ&Ü'¢  '&&&%&$ê  ± E'Ö'H± àE
&Ö&ê Ê%P Ê$JP|•'ë H•&ë † •$ë •  Í ~
\ d L
 b.(R#  )Í #7\ d } a\R#∏ "  .h #  .$ ∏ ;
\ d Ô b\\R#Ô;∏ ; \ d    \NEEDSR#  ı&J
Ñ
 Û \ d    HEXR#   Ê ; \ d 7  DECIMALR#
Ê ; \ e j  DIGIT?R#  0I ∏    [ Ñ     áI ∏
    [ Ñ   Ê " Ù [ } S Ä ° \ e A  ACCUMULA
TER#ã P ã Ê " û Ä
Ê " û ı Ô \ e †  CON

\ *** Block No. 47, Hexblock 2f

  Ö † l  + B Ü(+LOOP˝  ° A Å ± Q ë Jq  •
 I Ö ê Ê ( Dl  , Ú ÅI' † • 8È Ö ∞ F  ± HH
Q Å à± HHQ † ë l  , ! ÅJs † PR- m ÜBRANCH
D  • A Ö$• Q Ö •$Ö l  - y á?BRANCHÜ °
•  I Ö ê Ê (Kld . Z  >MARKR#X °   \ . û
 >RESOLVER#X Ù I ã ; \ . ≤  <MARKR#X \ .
M  <RESOLV



















\ *** Block No. 48, Hexblock 30



























\ *** Block No. 49, Hexblock 31



























\ *** Block No. 50, Hexblock 32



























\ *** Block No. 51, Hexblock 33



























\ *** Block No. 52, Hexblock 34













                       ©  E(Å •)I ë lr •(
Ö*•)Ö+°*Ö(† ±*Ö) (P l   I/P‚°(Å*† ±(ë*≠i/
Å(≠j/ë(•(çi/•)çj/l£/J ]/ (DISKERRß#ª  err
or !  r to retry  8D   rU ó   RU   ï  , a
bortedH J  0 DISKERR˝( 0J g0 R/W˝(0@ß#D F
 . è ê j m D . / ê 3
 8*  8*Ö+D F
m .
R ô
. ≠ ]0ê   ª  write q0N {ˇ    Ä
` /
 å H ß#m D Ò `   H ß#D ª0
 8*  8*Ö+|
Â
.
%      F
H ]0ê   ª  read q0N Yˇ˚ H
ß#g/D . ê   . D m . ü U ê Íˇw/A.D A0H ß#m
 | ı
% g Â .   % m g ˚ `   w/Ù.H ß#g/. D
ê   D F . è ê ÓˇH ß#g/. D . • ê ˆˇF . è H
 L w0 CORE?ß#X/Í
≠ H M Ü1 (BUFFERß#X/ 1.

\ *** Block No. 53, Hexblock 35

  1N ¯ˇH M ö1 (BLOCKß#X/ 1I0.1N ˆˇH O1† ±
 hH± l+ M ¥1 BUFFERß#M1_1H M |1 BLOCKß#M1
Ω1H N Ô1 UPDATEß#  Äg/. F /  H N  2 SAVE
-BUFFERSß#w/A.r1È ê   A0N Ùˇw/Ù.H N  2
EM
PTY-BUFFERSß#w/A.g/. È ê   D ª0N Úˇw/Ù.H
N h2 FLUSHß#,2x2H O X2 (COPYß#D M1é1ê   g
/. ª0L1ê     ,2Â .   ó ˜1ë ë g  2H O ä2 B
LKMOVEß#,2|
%
G | ı
  ˚ = ê $ % %
 ˚ ≠ à   ü ; ˛ˇ  ı
í2V E N   ˚ ≠ à   ı
í2
H H   V E ,2Í
H O B2 COPYß#H L2H O *3 CON
VEYß#ó / R ô
U D h ï  , no!!L2H P ;3 LIMI
TÙ# @P C3 FIRSTz"  P Q3 ALLOTBUFFERß#Y3.
M . U E/m    E/∞ Y3  G Y3. D ª0g/.
g g
/g H P 3 FREEBUFFERß#Y3. K3E/U   ê . Y3.
 A0g/D . Y3. U ê   . N ÓˇY3. . ó g E/Y3G
H P A3 ALL-BUFFERSß#Y3. ç3Y3. U ê ÚˇH ß#V
    = H ß# #à
/   ó @   H ß#¨ 1 . | ˚ . È
 ê ^ D | Ê U | ˚ . |
S D %   ó % m  #
= ê . % @ ê Zˇ% m 24ê
% m   =4ê   % m
  #m ¸ N ∂ˇe N úˇH C4† ± ô$ à ¯†  ± I Ö*H
± I Ö+† °$Ö,±$Ö- ,5•,E*•-Â+ê •,E&•-Â'l 5
•(E,•)Â-∞ °,Å$±,ë$l 5•,Ö$•-Ö%l4l© ß#1 .
È ê   D | Ê U A4˚ N ÍˇH ß#â / D . â U ê (
 ı
. ó ‰ ª ê   D . / .
g S N   . N NˇÍ

H ß#1 A4 %U   ı
1 . F
ª ê   ; †%1 g ; ˛ˇ

\ *** Block No. 54, Hexblock 36

    E ı
È$. F
ª ê   ; †%È$g H S  4
CUSTOM
-REMOVE˝(´ ß#
a5}5#5Q5¨ ó U U W g ≠ ™ g
H  T A5 CLEARß#‰ D â U5W g H T ¯5 (FORGET
ß#D @  , is symbolo4U5H T  6 FORGETß#P'D
; $ .    , protected~"D @ ê    #N   ë ë
6H T 66 EMPTYß#; $ . â U5; 2 . < g H U S6
 SAVEß#‰ â   U51 . D ë ë .
ë g . È • ê
Íˇâ ∏ ;   P H U ï6 BYEß#,2:<H ß# 8| U ê
 ü e H U N6 STOP?ß# 8ê   |6|6≠ H U 6 ?CR
ß#A78.   U G ê   N7H ß#Ì!D * m  !lÏ V   .
   G H V  7 OUTPUT:ß#á# !lÏ   g H V b7 EM
IT17 V ]7 CR17 V I7 TYPE17 V S7 DEL17 V 
7 PAGE17 V ä7 AT17 V   ñ7 AT?17 V †7 ROWß
#¶7å H V ´7 COLß#¶7V
H ß#Ì!D * m  !lÏ V

 .   G H V ª7 INPUT:ß#á# !lÏ
 g H V Ê7 K
EYU7 V  8 KEY?U7 V  8 DECODEU7 V  8 EXPEC
TU7 W    SEALß#≠ ; Ø%@#g H W 38$ONLYØ%W j
8%FORTHû%W u8%WORDS>&W A8$ALSO %W M8+DEFI
NITIONSÙ%ß#1 . D ë .
Ê U g .   È • ê Íˇ
H ß#≠ g/g K3Y3g  4H X %8 'COLD˝(´ ß#à8®8X
%D8ë7' X Z7N7Ò8H X º8 'RESTART˝(´ ß#; >+
)\+H<Ì8; . . ! g ; ´  )†+¨+H † πd ô  à ˜
≠  I Ö ≠! I Ö † ± Ö H± Ö † ± Ö H± Ö ¢ † ä
Å ë `Z ‚8 COLDz9¢ˇö Ja ≠  I Ö$≠! I Ö%† π
 ë$HP¯  9 ë ©aH8H Z q9 RESTARTì9¢ˇö Ja

\ *** Block No. 55, Hexblock 37

  9 ë ©aÒ8H ¢ † l  Å á9 C64KEY?∫9•F ©ˇhl
+ Å Æ9 GETKEYQ9•F X¨W ΩX ùW Ë‰FPıFFòxI†P
 © l( Ç F9 CURON˝9_S±QÖNÜLl•9Ç Û9 CUROFF
:HÑLÑMÜO•N_SëQ† l  É  : C64KEYß#˚9∂.∏9ê ˙
ˇ :O9H É ): #BSÙ#  É f: #CRÙ#
 É r: C64DE
CODEß#l:l ê   D ê   Ö7S p x:l ê   D } g p
 | ı
  % ó    ˚ D7/ H É ^: C64EXPECTß#}
g ≠ D } .   ê    8 8N ÓˇÍ
E,H É ¶: KEYBOA
RDı72:∏9J:≤:H Ñ V: CON!¯:°   Á•  I Ö ê Ê
ÜTÜX†  ë ∂.H IÄ∞ I `I‡∞ I@` `Ñ Ô: PRINTAB
LE?6;°   ;ê Jäl  Ö '; C64EMITp;°   ;∞ ©.l
˙:Ö d; C64CRß#x:ˆ:H Ö ^; C64DELß#  ùˆ:E,
 ùˆ:H Ö P; C64PAGEß#
  ìˆ:H Ö ã; C64AT™;
©   ¶&_$  ˇ_S±QÖNl•9Ö †; C64AT?M;• 8È Ö
 ∞ F äë 8 ˇ@(òê È(hä¢ Å Hl( Ü B; C64TYPE
˛;©   † D$ ±&  ;∞ ©.  ÁHl <l ;Ü Ú; DISP
LAYr7n;F;¸;Y;ï;®;K;H ‚¸á  < B/BLKÙ#  á ><
 BLK/DRVÙ#™ á l< (DRVz"  ß#C<.      H á \
< DRIVEß#v<0 Â g H á V< >DRIVE  ß#v<0   Â
 . U H á å< DRV?ß#Â .   v<t H á ß< DRVINI
Tß#´ H à æ< I/Oz"  à P< BUSOFFÁ< Lˇ† ¢  ë
 V<Ù.H ¢ †  ë Â<w/Ù.ü  , no deviceH Üê ±ˇ
©` ìˇ Æˇ•ê HHl˜<`â |< (?DEVICE:=°   =•
I Ö ê Ê lÍ<â -= ?DEVICEß#V<A.8=H â o= (BU
SOUTQ=Üù©   •&  =•& ±ˇ•$ ` ìˇ¶&Üöl•9ä E

\ *** Block No. 56, Hexblock 38

  = BUSOUTß#V<A.O=H ä í= BUSOPENß#    õ=
H ä ß= BUSCLOSEß#  ‡  õ=Â<H ä æ= (BUSIN„=
Üù©   •&  =•& ¥ˇ•$ ` ñˇ¶&Üôl•9ä X= BUSIN
ß#V<A.·=H ã  > BUS!!>°  ®ˇl• ã  > BUSTYPE
ß#Ø à   1 V  >V E ∂.H ã +> BUS@v> •ˇl( ã
m> BUSINPUTß#Ø à   t>1  V E ∂.H ã ^> DER
ROR?ß#G<      >t>D   0U ê   D7t>D x:U ê Ù
ˇ• N7• Â<H Ù#E Ù#Í Ù#v ß#D π>  ê      b p
 |   D Ω>  ê   π>U    b      p D A>  ê
Ω>U    b      p A>U    b      H ß#E>/ ó H
 ß#Ò 8*A ≠  -Y-å   ,˘,Y-!-H Ù#  ç Å> READ
SECTORß#G<   õ=ù  u1:13,0,X 5>*?5>Â<∂.ã>
 G<
 >e?I>Â<≠ H ç   k? WRITESECTORß#%
G
<   õ=ù  b-p:13,0X 5>Â<G<
õ=e?5>Â<G<
õ=ù  u2:13,0,X 5>*?5>Â<∂.ã>H é è? DISKOPE
Nß#G<
±=  # >Â<ã>H é Í? DISKCLOSEß#G<

I=Â<H é  @ 1541R/Wß#ó  , no fileF
v<b D
C<g | G ê   Ì-ª  beyond capacityV
p ı?ê
 å V
p ≠ ó ? ? Ê Ø U . å ı
1 %
ê    ?x?
N    ?ù?| e?  ˚ D ê   ê V E F
Í
 @H è &@
INDEXß#/ ó U ! N71 R ∏-1 ˜1/   %Z7¯6ê   ê
 V E H è º@ FINDEXß#ı?ê   Í
p / ó U 1 N71
 R ∏-Ù D 1 ? ?  ?x?| /   %Z7˚ ¯6  ê   ê V
 E  @H è Ò@ INK-POTz"              h≠\aÖ
H@Xç[a©ah©]h ≠[ahähòh• ç\a©6Ö ©ç
}¨
}

\ *** Block No. 57, Hexblock 39

  lR˛ Íˇ ·ˇPılì9ë ca INIT-SYSTEMß#; @ˇD
 @P ; EaD ; ˙ˇg ;   g H XX Ñˇ Åˇ äˇ©6Ö ≠o
aç P≠paç!P≠qaçÜ ©Äçä © ç P© ç P© ç P© çà
x`ë õa C64INIT b Jal•9{  bc(16ß#' X •  ,
C) missing. ; C)U ê „ˇH {  bbC)ß#H { gbc(
64ß#H { rb FORTH-83ß#H | ^b ASSEMBLERã%
5cµ%|    P
   û(2064)    Íl 9Íl<9l4Y
J. {æõæüE{    S;!r®+ F




                     ∑USHAÙ#  | Öb PUSH0A
Ù#( | ìb PUSHÙ#+ | ¢b RPÙ#  | Øb UPÙ#  |
∫b SPÙ#  | Eb IPÙ#  | Pb NÙ#$ | {b PUTAÙ#
  | Âb WÙ#  | Úb SETUPÙ# | ¸b NEXTÙ#  |
 c XYNEXTÙ#•9|  c POPTWOÙ#  | &c POPÙ#© „
ˇH {  bbC)ß#H { gbc(64ß#H { rb FORTH-83ß#
H | ^b ASSEMBLERã%  5cµ%|    P  $Ö&àD&P †
 l  H±$ í ë$l} Û Ü*† ± ô$ à ¯†  ≠F E&Ö&≠G
 E'Ö'8•$ÌF Ö$•%ÌG Ö%∞
° çF ± çG lÖ † •$ %
=°&Q P Ê&P Ê'•$P F%F$l0 •&Ö(•'Ö)°&Q  Ê&P
 Ê'•$P F%F$( Ê*•$ %P‚8† •&Ò çF H•'Ò çG
© E Ö ê Ê † ± Å Ö$H± † ë Ö%à•*ë$±(HF* ˜©
ë$† l  b A  SOURCEß#P . È ê   ˜1f<p È [

\ *** Block No. 58, Hexblock 3a

  . H b ∏  WORDß#A Ò H b }  PARSEß#| A D
. Ä
ó ˚   |
U D ˚ x U D G H b Ó  NAME
ß#S,‰ N p H c    STATEz"  c 5 eASCIIß#S,‰
 / V = . ê   J H c c  ,"ß#  "ˆ ‰
/   z
H c C Ñ"LITß#˚ ˚ à
X   | | H c } Ç("ß#Ñ H
 c ò A"ß#b ù H H d • É(."ß#Ñ X Z7H d µ B.
"ß#b ª H H   d G a(ß#  )ˆ Í
H d X b.(ß#
)ˆ Z7H  d È a\ß#D . 8.t / 8.0 D g H d ¸ b
\\ß#f<D g H d    \NEEDSß#' *'V
ê     H d
)  HEXß#   Ò g H d d  DECIMALß#   Ò g H e
 w  DIGIT?ß#  0U D    G ê     áU D    G ê
   Ò .
G È  å ≠ H e N  ACCUMULATEß#ó |
 ó Ò . ™ å %
Ò . ™     ˚ H e ≠  CONVERTß#
/ X W ê   ∫ N ÙˇS H e Z  END?ß#Ë . • H e
¸  CHARß#X ü Ë G H e    PREVIOUSß#S X H f
 $  DPLz"ˇˇß#ê   e Í
å e ≠ H ß#ê   e å ˚
ê   A %
å ? . / È  å ü H ß#  &l ê      ü
 p   $l ê      ü p   Hl ê      ü p   %l ê
   R ü p ≠ H ß#  ,
U ó   .U     H ß#? .
ü U  H ? G H z"  g 9  NUMBER?ß#Ò 8*D X Ë
 g ? Ò ≠ | ≠ ≠ %
  c     -l ê   e ü |   c
   { ê   Ò g   c   W • c ∫ T   u   W • ê
ˇ/ @ • c ?     u   N TˇH g Ó  'NUMBER?˝(
¯ g Q  NUMBERß#| È •  , ?è ê   $ H h Ç  L
ASTz"  ß#™ . È H h £  HIDEß#Æ ê   ë . È$

\ *** Block No. 59, Hexblock 3b

  . g H h ∫  REVEALß#Æ ê   ë È$. g H h W
IRECURSIVEß#‡ H ß#Æ ê   à
V
 å H h Ù
  IMMEDIATEß#  @  H h    RESTRICTß#  Ä  H
 i 5  CLEARSTACKz † ± Ö H± Ö † l  i k  HA
LLOTß#C .
U ó g m D %
U D C g R ô

U 8
 x C g H i L  HEAPß#C . F H i •  HEAP?ß#¨
 â ª H ß#D   ‰
U D U ¨ ó P ¨
U ™ G ‡
H • 8È Ö ∞ F • ë • Å  ° I Ö äQ Ö ©  E häE
 l+ ß#˚ ™ .  #g H j ∏ EDOES>ß#b  !  l* b
Ï H ß#D   ˇ=   ˇU U H ß#‰   ˇ=   ˇU ê   D
 D / ‰
U / ˛ H ™ G H   H k '! ?HEADz"
k Ç! |ß#ä!.  ü ä!g H l ê! WARNINGz"  ß#∞
!.  ™ . È$. N&V
ê     E,™ . p#ª  exists
 7H l ¶! CREATEß#‰ P .   È$. .   ' V D H
   ª ï  , invalid name‰ ™ g /   ¥!ä!. ê
 H ä!G D @!  J      @!W g N   r!å ‡ ≠
!l !m ‰! NFA?H"° Ö(± Ö)•  I Ö ê Ê ° Ö&± Ö
' &P lò ±&ë Ö%°&Å Ö$ %P l  •$ I Ö$ê Ê%°$h
8) E$Ö$ê Ê%H)  ±$h°$Ö$HÖ%•$E(  P∞•%E)P™l
o n _" >NAMEß#1 . D ê   ı
Ê U ó F"È ê   F

Í
p N ‡ˇV
H ß#X    =   H n V" NAME>ß#D
#ó V    = ê   . H n  # >BODYß#m H n 8# .N
AMEß#È ê   D @ ê   ª  |X    = Z7N   ª  ??
?E,H o h# CREATE:ß#Ì!A È$. ˘$g I(≠ H o }#
 :ß#á# !• 8È Ö ∞ F • Å • ë •  I Ö äE Ö l

\ *** Block No. 60, Hexblock 3c

    o ù#A;ß#≠   b H ≥(‡ H o K# CONSTANTß#
Ì!   !• 8È Ö ∞ F † ± Å H± † ë l  o ·# VAR
IABLEß#Ì!R   H p  $ UALLOTß#D < .     ˇG
 ,
Userarea full< . ó < G H p ($ USERß#Ì!
R 1$*  !• 8È Ö ∞ F † ±  E Å äHE † ë l  p
^$ ALIASß#Ì!™ . D V    = ê   ; ˛ˇ  N
    #g H q   í$ VPz"                q F$
CURRENTz"  q $ CONTEXTß#K$D .   m H ß#K$
m ˘$H q Ô$ ALSOß#K$.    9 ., Vocabulary s
tack full˘$. R K$G ˘$g H q  % TOSSß#; ˛ˇK
$G H r o% VOCABULARYß#Ì!≠   ≠   ‰ 1 .   1
 g  !lÏ ˘$g H r D% FORTHã%  Ob  r ñ% ONLY
π%  X8_% !lÏ ≠ K$g ˘  $g  %H r ®% ONLYFOR
THß#Ø%û% %Ù%H s L% DEFINITIONSß#˘$. È$g H
 ß#. ë ~"p#H s Ê% ORDERß# %U   1  &; ˛ˇ
E R X,È$ &H s  & WORDSß#˘$. . D ¯6• = ê
  7D m p#E,N Êˇå H t 6& (FINDP&† ± ô$ à ¯
°&) Ö(† ±$™H±$Ö%Ü$ $P † ¢ lò H±$) E(P‡ ©
E$Ö)© E%Ö*_(±&Q)PKàP˜† •*ë à•)
ë à¢ lï K
&° Ö$± Ö%°$Ö&) 8E$Ö$ê Ê%•&) P •$Å •%l˜&°$
Å ±$ë • 8È Ö ∞ F •&  H 0 òiˇ®HòÅ   ©ˇ$ä†
ë l  v F& FINDß#˘$D .
ë . U ê   ë à
. N
&ê   V
I&p
K$m G ê   ó ë N ~ˇV
≠ H v #'
 'ß#' *'•  , What?H v L'I[COMPILE]ß#P'  H
 v Ñ'C[']ß#P'J H v ö' NULLSTRING?ß#D V •

\ *** Block No. 61, Hexblock 3d

   D ê   V
H H ± I Ö H± I Ö † l  w ™' >IN
TERPRETJ'4(x ‡' NOTFOUND˝( (x Û'
NO.EXTEN
SIONSß#., Haeh?H x  ( INTERPRETß#Ì'H ß#R)
' *'È ê   H = ê   £ Ì' , compile only∏'
| • ê   ˛'Ì'H ß#R)' *'È ê   h ê   £ Ì'  Ì
'∏' | È ê   h ê   ó J J N   ˛'Ì'H y "(a[
ß#; 4( )Ô'  =   H y Ø( ]ß#; O( )Ô'= Ò H ß
#ü  , CrashH z E( DEFERß#Ì!; Y(   !† ± hH
± Ö HÖ † l  z È( (ISß#˚ D m | . g H ß#. ;
 ˛'.
U ó ; Ì'. U   ï  , not deferredH z
  )bISß#P'D ')@#= . ê   b  )  p g H z" ß#
 +   9  , tight stackY)V • ê   ü Y) ü  ,
 dictionary fullª  s  till full H [ v) ?S
TACKT)† 8• Ò H• Ò P †  ë |)H é{)† ± E H±
Â † ê l   ë ü  , stack emptyH \ I) .STATU
S˝(´ z"˚ ˚ g H \  *ÑPUSHß#˚ ó D | . | %*|
 | H \ 1* LOADß#È •  P 8*P g D 8*D   !*.
(H ] p* +LOADß#P .   w*H ] W* THRUß#/ ó U
   1 w*V E H ] ç* +THRUß#/ ó U     1 *V
E H ] ™*c-->ß#H P G D   !*H ] H* RDEPTHß#
M . ø m U ) H ] ‡* DEPTHß#g C . ó U ) H ß
#= . ê   ª   compilingp ª   okH ^ ˚* (QUI
Tß#!*N7˚ .( +N ÙˇH ^ 6+ 'QUIT˝(>+^ r+ QUI
Tß#M . N ≥(z+H ^ `+ STANDARDI/Oß#; *   Ê
P H ^ W+ 'ABORT˝(´ ^ ï+ ABORTß#x N û+Ö+G

\ *** Block No. 62, Hexblock 3e

  +H _ _+ SCRz"  _ º+ R#z"  _ H+ (ERRORß#
Ö+E,‰ p#X Z7E, 7P . È ê   B+g D . M+g G+H
 _ S+á(ABORT"ß#Ñ ó ê   | x ˚ ! G p å H ß#
Ñ ó ê   ! G p å H _  ,FABORT"ß#b  ,H H _
d,FERROR"ß#b .,H H ` y, BLÙ#  ` N, -TRAIL
INGá,ò  ° Ö& ± E%Ö'_$ ê à±&I  HP Ê%òh•%
l+ òPÍF'F%   ‰òl( A Y, SPACEß#S,D7H A Ω,
SPACESß#≠ à   E,V E H ß#Ù ë H B O, HOLDß#
ü Ë,G Ë,.  H B Ú, <#ß#Ë,Ë,g H B  - #>ß#Í

Ë,. Ë,
U H B  - SIGNß#è ê     -˘,H B 3-
 #ß#Ò . ∞ %

· ê     á    0  ˘,H B k-
 #Sß#o-ı
Ö ê ¯ˇH C T- D.Rß#F
à
5  -Y-%
:-
!-%

È
U X,Z7H C   â- .Rß#ó $ %
è-H C
≥- U.Rß#≠ ó è-H C F- D.ß#≠ è-E,H C X- .ß#
$ }-H C È- U.ß#≠ }-H D ˜- .Sß#g C .
U
    Ø à   1 . ¸-R   E H D  . C/LÙ#) D 2.
L/SÙ#  D >. LISTß#B+g ª  Scr B+. D v<D ¸-
ª  Dr Æ<Ì-d.≠ U . ¯6ê   ê N71 R ∏-E,B+. ˜
11 8.0   8.S Ö,Z7V E N7H E j.   PAUSE  E
Æ. LOCKß#D . â U ê   å p D . ê   ∂.N Ùˇâ
ó g H E ∫. UNLOCKß#D A.  H H8È Ö HÈ Ö ©lÅ
 † ± Ö H± Ö † ° Ö ± Ö •  I Ö ê Ê ¢ lú F Î
. FILEQ$ F 4/ PREVz"  F @/ BUFFERSz"  F m
/ B/BUFÙ#  † ±(Y" P H@ PÙ`Z/† ± ô$ à ¯†
± E&Ö&H± E'Ö'≠i/Ö(≠j/Ö) I/P •  I Ö ê Ê †

\ *** Block No. 63, Hexblock 3f


  E HhH è bi INK-POTz*ˆˆ  Óˆ  ˆˆ  í îi I
NIT-SYSTEMæi¢˜öl•aç>ˇçYié{i©ih©h∫Ω  h© ¢
 l≥¸ç?ˇ@Xç>ˇ©içˇˇ©Dç˛ˇ Ñˇ äˇ≠†iç ˇ≠°iç ˇ≠
¢iç@ ©Äç@ ≠ ˇ  ç ˇç?ˇx`ì Æi C64INIT)j „il
•a          ÖÜáâäãåàî  j C64FKEYSpj¢ J0 Ω
/jù] lrjl•a{ cjc(64ß+'$X •  4 C) missing.
 ; C)U ê „ ˛ˇH { CjbC)ß+H { éjc(16ß+H { ô
j FORTH-83ß+H | •j ASSEMBLERã-  |kµ-|
PUSHAÙ+  | Lj PUSH0AÙ+( | Zj PUSHÙ++ | Èj
 RPÙ+  | ˆj UPÙ+  |  k SPÙ+  |  k IPÙ+  |
  k NÙ+$ | "k PUTAÙ+  | ,k WÙ+  | 9k SETU
PÙ+ | ck NEXTÙ+  | qk XYNEXTÙ+•a| ^k POP
TWOÙ+  | Mk POPÙ+© „

   û(2064)    Íl
z9Ílì9l { ˛. { { ?c    *<„:|+Åb






                             LOGOz" volks
FORTH-83 3.80.1-C64  çˇˇ± Ö ≠ˇˇÖ  • I Ö ∞
 LˇˇÊ ∞˘     END-TRACEP ©•Ö © Ö ©IÖ © Ö l
    B  RECOVERz"HÖ HÖ • P F F lß#  Ö  NOO
P    _  ORIGINÙ#    Ø  S0Q$   æ  R0Q$   H
  DPQ$   R  OFFSETQ$   |  BASEQ$    Í  O

\ *** Block No. 64, Hexblock 40


 UTPUTQ$   ˜  INPUTQ$      ERRORHANDLERQ
$      VOC-LINKQ$   &  UDPQ$   6  SP@i •
Ö$• Ö%¢$• 8È Ö ∞ F µ ë µ ¢ l    a  SP!S °
 ™± Ö Ü ¢ l    K  UP@ã ¢ ls   É ÉUP!ö ¢ ±
 ï à± ï ¢ † •  I Ö ê Ê l    í  RP@A ¢ ls
  π ÉRP!P ¢ lú   H Ç>R~ • 8È Ö ∞ F ° Å ±
ë l©   W Ç
 R>˝ • 8È Ö ∞ F ° Å ± ë ©  E Ö
 ê Ê l    ˆ  R@' • 8È Ö ∞ F ± ë ° l
ÖRDROP    =  EXITr ° Ö ± Ö l    i  UNNEST
J ° Ö ± Ö l    _  ?EXITÅ °    •  I Ö ê Ê
(Pæl    W  EXECUTE• ° Ö ± Ö •  I Ö ê Ê l
   ô  PERFORMß#. £ H   Ω  C@X ° Ö$± Ö%© ë
 °$l    Q  C!Ú ° Ö$±
  Ö%H± Å$à•  I Ö ê Ê
 l    Î  CTOGGLEß#à
V z ó  H      @0 ° Ö
$± Ö%±$ë °$l    *  !i ° Ö$± Ö%H± Å$H± † ë
$l    c  +!I ° Ö$± Ö%H±  A$Å$H± † Q$l[
B  DROP©   Ö  SWAPô ± ™† ± Ö$äë •$† ë H¢
± Ö$° ë à•$l    ê  DUPF • 8È Ö ∞ F † ± †
ë H± àl    æ  ?DUPÎ °   P l  l
 F   ‚  OV
ER
• 8È Ö ∞ F † ± Å H± † ë l    ˘  ROT'

† ± Ö%† ± † ë † ± Ö$•%ë † •$ë H± Ö%° ë †
± Å •%ë † l
 -ROTß#%
%
H   _
 NIPß#ó
 å H   P
 UNDERß#ó
H   Ä
 PICKß#/ ? g
 . H   í
 ROLLß#D | ô
g D m ˚ / ? ˛ å H
 ©
 2SWAPß#%
| %
˚ H   L
 2DROP    ‚
 2D

\ *** Block No. 65, Hexblock 41


 UPß#

H   Ó
 +   H° Q ë à± † Q ë l•
 ˇ
 OR" H°   ë à± †   ë l•      AND? H° 1
 ë à± † 1 ë l•   7  XOR\ H° q ë à± † q ë
l• ! t  -W H± 8· ë H± † Ò † ë l• ! Q  NOT
ó  ä· Å äÒ ë l  ! è  NEGATE≤ 8∞„" ß  DNEG
ATEC H8äÒ ë HäÒ ë ä· Å † äÒ ë l   ™®à± ô$
 à ¯ä E Ö
 ê Ê ¢ † `" ∑  D+  ©   H±  E&
ë H± E'ë ° E$Å † ± E%ë l  # ¸  1+1 ©  A ∞
 l  Å ± I ë l  # *  2+o © P‡# h  3+\ © PS
B © PMH © PG# u  1-U 8° È ê l  Å ± È ë l
 # N  2-ì  ê‡$ å  TRUEÙ#ˇˇ$ ò  FALSEÙ#  $
 • "-1ü $ ≥ !0≠ $ º  1Ù#  $ D  2Ù#  $ N
3Ù#  $ X  4Ù#  $ ‚
 ONß#ü ó g H $ Ï  OF
Fß#≠ ó g H % ˝ ÑCLIT  • 8È Ö ∞ F ° Å äë Ê
 P Ê l  %   ÉLIT= • 8È Ö ∞ F ± ë ° Å •  I
 Ö ê Ê l  % 5 GLITERALß#D ;  ˇ= ê   b ;
 p b   * H & `  0<ë ±   ©ˇ$äë l  & ä  0=ß
 °   ËPÈ& †  UWITHINΩ ©   † ° E$± Â%∞ °
 E&± Â'∞Ωlò ' ±  <„ ° Ö$± Ö%•
  I Ö ê Ê
•%q 0ï° E$± Â%lì ' }  U<  ° Ö$± Ö%•  I Ö
ê Ê ° E$± Â%∞ lï lò (    >ß#ó · H ( 5  0>
ß#∞ è H ( c  0<>ß#• ï H ( r  U>ß#ó   H (
B  =ß#U • H ( Q  D0=ß#  • H (   D=ß#A
Ö H ( è  D<ß#%
ı
U ê   9 V
V
N   Í
  H ß#
e ê   ó å H ) †  MINß#ı
9 A H ) Q  MAXß#

\ *** Block No. 66, Hexblock 42



ı
· A H ) „  UMAXß#ı
  A H ) ı  UMINß#ı

G A H )    EXTENDß#D è H )    DABSß#$ ê
  A H ) .  ABSß#$ ê   ∞ H ß#e ˚ m D | %
|
 ó | | H * c É(DOß#
U u H * O Ñ(?DOß#
U
 È ê   u ˚ D .   | å H * Å  BOUNDSß#
  ó
 H * ¶ áENDLOOPG © l  + ª Ö(LOOPX  © A Å
ê ± I ë ê
 l  † ± Ö à± Ö † l  + N Ü(+LOO
P   ° A Å ± Q ë Jq  •  I Ö ê Ê ( Dl  , ˛
ÅI3 † • 8È Ö ∞ F  ± HHQ Å à± HHQ † ë l  ,
 - ÅJ_ † PR- y ÜBRANCHP  • A Ö$• Q Ö •$Ö
l  - E á?BRANCHí °    •  I Ö ê Ê (Klp .
Ü  >MARKß#‰ ≠   H . ™  >RESOLVEß#‰
U ó
g H . æ  <MARKß#‰ H
 . Y  <RESOLVEß#‰ U
  H . È  ?PAIRSß#U  , unstructuredH I   h
©ˇl+ h© ™• 8È Ö ∞ F ä† ë H¢ l  /    CASE?
n ©   •$A P •%Q P lï äl( 0 d BIFß#b ê ≤
H H 0 H DTHENß#i H   I H 0 { DELSEß#H   b
 N ≤ ó I ü H 0 ê EBEGINß#· R H 0 ≠ EWHILE
ß#R   R b ê ≤ ; ˛ˇT
H ß#Ù D ;
 ˛ˇU ê   å
 I N ÓˇH 0 ø FREPEATß#R   b N } H 0 ˘ EUN
TILß#R   b ê } H 1   BDOß#b U ≤ | H 1 * C
?DOß#b à ≤ | H 1 = DLOOPß#|   b V b E I H
 1 q E+LOOPß#|   b   b E I H 1 L ÖLEAVEß#
E ˚ ë D .   | H 2 à  UM*¨ ± Ö$° Ö%HÜ&Ü'¢
 '&&&%&$ê  ± E'Ö'H± àE&Ö&ê Ê%P Ê$JP|•'ë

\ *** Block No. 67, Hexblock 43


 H•&ë † •$ë •%Å l  3 _  M*ß#D è D | ê
∞ ó D è ê   ∞ ˚ ï | ™ ˚ ê   A H 3 ˜  *ß#™
 å H 3 ,  2*a °  Å ± *ë l  ß#ü  , divisio
n overflowH 4 :  UM/MODU ° Ö)± Ö(•  I Ö ê
 Ê ° Ö%± Ö$H± Ö'H± Ö&¢  F*8•%Â)®•$Â(∞ &*ê
 Ö$Ñ%&'&&&%&$JP† F$F%ê  ë n H •&ë H•%ë H
•$ë † •'l
  5 J  M/MODß#D | i
è ê   à

  ó S % è ê   ∞
ê   ó %   ó S e H 5 ‰
2/+ ±  ± Jë ° Jl  6 $  /MODß#| $ ˚ Ï H 6
;  /ß#b V
H 6 p  MODß#b å H 6 ^  */MODß#|
 ¸ ˚ Ï H 6 N  */ß#V V
H 6 Ñ  U/MODß#≠ ó S
 H 6 ì  UD/MODß#| ≠ % S ˚ ó | S ˚ H 7 ß
CMOVER ©   àD$P F%
  † l  ±(ë&HPÏÊ)Ê'lX
 7 H  CMOVE>  ©    •%E'Ö' •%E)Ö)Ê%_$ ê à
±(ë&òP¯F'F)F%P† l  7 ı  MOVEß#| ı
  ê
˚ ˛ p ˚ P H 8 1  PLACEß#
| %

/ ˚ 8  H
 8 r  COUNTZ ° Ö$ I Å ± Ö%I ë • 8È Ö ∞ F
l‡ 8 P  ERASEß#≠ ¥ H 9 õ  FILL∂ ©   à•$¶
' ë(HP˚Ê)JPˆ¶& ë(HJP˙† l  :
 ≠  HEREß#
W . H : }  PADß#‰   b  H : Ó  ALLOTß#W G
H :    ,ß#‰ g R   H :    C,ß#‰  H   H :
% áCOMPILEß#˚ D m | .   H ; 8  #TIBz"  ;
t  >TIBz"L

         ; A  >INz"  ; æ  BLKz"  ; J  SP

\ *** Block No. 68, Hexblock 44

  ANz"  ; V  TIBß#H . H ; „  QUERYß#È   p
.8} . [ g D   P   H < Û  SCANß#| D ê

V % U ê   S ó / ó N ‰ˇe H <    SKIPß#| D
ê
V % U ê   S ó / ó N ‰ˇe H < g  /STR
INGß#
  %

  F
U H Iaê I{∞ IBê )I[∞  Ä
`= V  CAPITALµ °  í Å l  > ©  CAPITALIZEP
 ° Ö$± Ö%°   ‰òl( A Y4 SPACEß+S4D?H A Ω4
SPACESß+≠ à   E4V E H ß+Ù ë H B O4 HOLDß+
ü Ë4G Ë4.  H B Ú4 <#ß+Ë4Ë4g H B  5 #>ß+Í
 Ë4. Ë4  U H B  5 SIGNß+è ê     -˘4H B 35
 #ß+Ò . ∞ %      · ê     á    0  ˘4H B k5
 #Sß+o5ı Ö ê ¯ˇH C T5 D.Rß+F à 5  5Y5% :5
!5%   È   U X4Z?H C   â5 .Rß+ó $ % è5H C
≥5 U.Rß+≠ ó è5H C F5 D.ß+≠ è5E4H C X5 .ß+
$ }5H C È5 U.ß+≠ }5H D ˜5 .Sß+g C .   U
    Ø à   1 . ¸5R   E H D  6 C/LÙ+) D 26
L/SÙ+  D >6 LISTß+B3g ª$ Scr B3. D FdD ¸5
ª$ Dr ædÌ5d6≠ U . ¯>ê   ê N?1 R ∏5E4B3. ˜
91 860   86S Ö4Z?V E N?H E j6   PAUSE  E
Æ6 LOCKß+D . â U ê   å p D . ê   ∂6N Ùˇâ
ó g H E ∫6 UNLOCKß+D A6  H H8È Ö HÈ Ö ©lÅ
 † ± Ö H± Ö † ° Ö ± Ö •  I Ö ê Ê ¢ lú F Î
6 FILEQ, F 47 PREVz*  F @7 BUFFERSz*  F m
7 B/BUFÙ+  † ±(Y" P H@ PÙ`Z7† ± ô$ à ¯†
± E&Ö&H± E'Ö'≠i7Ö(≠j7Ö) I7P •  I Ö ê Ê †

\ *** Block No. 69, Hexblock 45

   ©  E(Å •)I ë lr •(Ö*•)Ö+°*Ö(† ±*Ö) (P
l   I7P‚°(Å*† ±(ë*≠i7Å(≠j7ë(•(çi7•)çj7l£7
J ]7 (DISKERRß+ª$ error !  r to retry  @D
   rU ó   RU   ï  4 abortedH J  8 DISKERR
˝0 8J g8 R/W˝0Åhß+D F . è ê j m D . / ê 3

 82  82Ö3D F   m . R ô . ≠ ]8ê   ª$ wri
te q8N {ˇ    Ä  ` /   å H ß+m D Ò `   H ß
+D ª8
 82  82Ö3|   Â .     %      F H ]8ê
   ª$ read q8N Yˇ˚ H ß+g7D . ê   . D m .
ü U ê Íˇw7A6D A8H ß+m | ı % g Â .   % m g
 ˚ `   w7Ù6H ß+g7. D ê   D F . è ê ÓˇH ß+
g7. D . • ê ˆˇF . è H L w8 CORE?ß+X7Í ≠ H
 M Ü9 (BUFFERß+X7 9.  9N ¯ˇH M ö9 (BLOCKß
+X7 9I8.9N ˆˇH O9† ± hH± l+ M ¥9 BUFFERß+
M9_9H M |9 BLOCKß+M9Ω9H N Ô9 UPDATEß+  Äg
7. F /  H N  : SAVE-BUFFERSß+w7A6r9È ê
 A8N Ùˇw7Ù6H N  :
EMPTY-BUFFERSß+w7A6g7.
È ê   D ª8N Úˇw7Ù6H N h: FLUSHß+,:x:H O X
: (COPYß+D M9é9ê   g7. ª8L9ê     ,:Â .
ó ˜9ë ë g  :H O ä: BLKMOVEß+,:|   %     G
 | ı   ˚ = ê $ % %   ˚ ≠ à   ü ; ˛ˇ  ı í:
V E N   ˚ ≠ à   ı í:H H   V E ,:Í H O B:
COPYß+H L:H O *; CONVEYß+ó / R ô U D h ï
 4 no!!L:H P ;; LIMITÙ+ ÄP C; FIRSTz*  P
Q; ALLOTBUFFERß+Y;. M . U E7m    E7∞ Y;

\ *** Block No. 70, Hexblock 46

  G Y;. D ª8g7.   g g7g H P ; FREEBUFFER
ß+Y;. K;E7U   ê . Y;. A8g7D . Y;. U ê   .
 N ÓˇY;. . ó g E7Y;G H P A; ALL-BUFFERSß+
Y;. ç;Y;. U ê ÚˇH ß+V    = H ß+ +à /   ó
@(  H ß+¨(1 . | ˚ . È ê ^ D | Ê U | ˚ . |
   S D %   ó % m  +  = ê . % @(ê Zˇ% m 2<
ê     % m   =<ê   % m  +m ¸ N ∂ˇe N úˇH C
<† ± ô$ à ¯†  ± I Ö*H± I Ö+† °$Ö,±$Ö- ,5
•,E*•-Â+ê •,E&•-Â'l =•(E,•)Â-∞ °,Å$±,ë$l
=•,Ö$•-Ö%l<l© ß+1 . È ê   D | Ê U A<˚ N
ÍˇH ß+â / D . â U ê ( ı . ó ‰ ª ê   D . /
 .   g S N   . N NˇÍ H ß+1 A< -U   ı 1 .
F ª ê   ; †-1 g ; ˛ˇ    E ı È,. F ª ê   ;
 †-È,g H S  <
CUSTOM-REMOVE˝0´ ß+  a=}=#=
Q=¨(ó U U(W g ≠ ™'g H  T A= CLEARß+‰ D â
U=W g H T ¯= (FORGETß+D @( 4 is symbolo<U
=H T  > FORGETß+P/D ; $ .    4 protected~
*D @(ê    +N   ë ë  >H T 6> EMPTYß+; $ .
â U=; 2 . < g H U S> SAVEß+‰ â   U=1 . D
ë ë .   ë g . È • ê Íˇâ ∏ ;   P H U ï> BY
Eß+,:ddH ß+ @| U ê   ü e H U N> STOP?ß+ @
ê   |>|>≠ H U > ?CRß+A?86   U G ê   N?H
ß+Ì)D *!m  )lÏ(V   .   G H V  ? OUTPUT:ß+
á+ )lÏ(  g H V b? EMIT1? V ]? CR1? V I? T
YPE1? V S? DEL1? V ? PAGE1? V ä? AT1? V

\ *** Block No. 71, Hexblock 47

   ñ? AT?1? V †? ROWß+¶?å H V ´? COLß+¶?V
 H ß+Ì)D *!m  )lÏ(V
 .   G H V ª? INPUT:
ß+á+ )lÏ(
 g H V Ê? KEYU? V  @ KEY?U? V
@ DECODEU? V  @ EXPECTU? W    SEALß+≠ ; Ø
-@+g H W 3@$ONLYØ-W j@%FORTHû-W u@%WORDS>
.W A@$ALSO -W M@+DEFINITIONSÙ-ß+1 . D ë .
   Ê U g .
 È • ê ÍˇH ß+≠ g7g K;Y;g  <H
X %@ 'COLD˝0´ ß+à@®@X-D@ë?' X Z?N?Ò@H X º
@ 'RESTART˝0´ ß+; >3 1\3XdÌ@; . . ! g ; ´
  1†3¨3H † πd ô  à ˜ ≠  I Ö ≠! I Ö † ± Ö
H± Ö † ± Ö H± Ö ¢ † äÅ ë `Z ‚@ COLDza¢ˇö
„i ≠  I Ö$≠! I Ö%† π  ë$HP¯  a ë ºiH@H Z
qa RESTARTìa¢ˇö „i    a ë ºiÒ@H ¢ † l  Å
áa C64KEY?∫a•Ô
]  ©ˇhl+ Å Æa GETKEYTaç>ˇ
 }Îç?ˇI†P © l( Ç Ia CURONÚa•J EHç
ˇ•II È
ç ˇl  Ç Ëa CUROFF b©ˇç ˇç
ˇl  É  b C64KEY
ß+a∂6∏aê ˙ˇ bRaH É  b #BSÙ+  É =b #CRÙ+

 É ib C64DECODEß+cbl ê   D ê   Ö?S p obl
ê   D }!g p | ı   % ó  ˚ D?/   H É ub C6
4EXPECTß+}!g ≠ D }!.   ê    @ @N ÓˇÍ E4H
É ùb KEYBOARDı?)b∏aAb©bH Ñ Mb CON!Ôb° ç>ˇ
 lˇç?ˇ•  I Ö ê Ê ÜKÜO†  ë ∂6H IÄ∞ I `I‡∞
I@` `Ñ Êb PRINTABLE?3c°   cê Jäl  Ö $c C6
4EMITmc°   c∞ ©.lÒbÖ ac C64CRß+obÌbH Ö [c
 C64DELß+  ùÌbE4  ùÌbH Ö Mc C64PAGEß+  ì

\ *** Block No. 72, Hexblock 48

  ÌbH Ö àc C64ATßc©   ¶&_$ ç>ˇ ˇç?ˇl•aÖ
 ùc C64AT?Jc• 8È Ö ∞ F äë 8ç>ˇ ˇç?ˇ@(òê
È(hä¢ Å Hl(  Ü øc C64TYPE d©   † D$ ±&
 c∞ ©.ç>ˇ lˇç?ˇHl dl cÜ ˆc DISPLAYr?kcCc
dVcíc•cHcH fdç>ˇlrˇá (d B/BLKÙ+  á nd BLK
/DRVÙ+™ á \d (DRVz*  ß+Sd.      H á Ld DR
IVEß+Fd0 Â   g H á Üd >DRIVEß+Fd0   Â . U
 H á úd DRV?ß+Â .   Fdt H á ∑d DRVINITß+´
 H à Nd I/Oz*  à ‡d BUSOFF˜dç>ˇ Lˇç?ˇ† ¢
 ë ÊdÙ6H ¢ †  ë ıdw7Ù6ü  4 no deviceH ÜêÖ
Æç>ˇ ±ˇç?ˇ©`ç>ˇ ìˇç?ˇç>ˇ Æˇç?ˇ•ê HHl
e`â
 Ïd (?DEVICEDe°  *e•  I Ö ê Ê l eâ we ?DE
VICEß+ÊdA6BeH â Ye (  BUSOUTõeÜö©   •& *
e•&ç>ˇ ±ˇç?ˇ•$ `ç>ˇ ìˇç?ˇ¶&Üôl•aä èe BUSO
UTß+ÊdA6ôeH ä He BUSOPENß+    QeH  ä }e
BUSCLOSEß+  ‡  QeıdH ä ıe (BUSIN fÜö©  
•& *e•&ç>ˇ ¥ˇç?ˇ•$ `Ö≠ç>ˇ ñˇç?ˇ¶&Üòl•aä
f BUSINß+ÊdA6 fH ã if BUS!Ff° ç>ˇ ®ˇç?ˇl•
 ã ]f BUSTYPEß+Ø à   1 V DfV E   ∂6H ã Vf
 BUS@°fç>ˇ •ˇç?ˇl( ã òf BUSINPUTß+Ø à   ü
f1  V E ∂6H ã Øf DERROR?ß+Wd   qfüfD   0
U ê   D?üfD obU ê Ùˇ• N?• ıdH Ù+E Ù+Í Ù+v
 ß+D  g  ê      b p |   D  g  ê    gU
b      p D  g  ê    gU    b      p  gU
 b      H ß+ g/ ó H ß+Ò 82A%≠  5Y5å   ,˘

\ *** Block No. 73, Hexblock 49

  4Y5!5H Ù+  ç Rf READSECTORß+Wd   Qeù$ u
1:13,0,X Äf{gÄfıd∂6|f Wd
qfñg∫fıd≠ H ç
 úg WRITESECTORß+% Wd   Qeù$ b-p:13,0X Äf
ıdWd
QeñgÄfıdWd   Qeù$ u2:13,0,X Äf{gÄf
ıd∂6|fH é ‡g DISKOPENß+Wd
Áe  #Dfıd|fH
é ;h DISKCLOSEß+Wd
 fıdH é \h 1541R/Wß+
ó  4 no fi
 leF Fdb D Sdg | G ê   Ì5ª$ be
yond capacityV p fhê   å V p ≠ ó ? ? Ê Ø
U . å ı 1 % ê   Qg©gN   QgÓg| ñg  ˚ D ê
 ê V E F Í HhH è Wh INDEXß+/ ó U ! N?1 R
∏51 ˜9/   %Z?¯>ê   ê V E H è
i FINDEXß+f
hê   Í p / ó U 1 N?1 R ∏5Ù D 1 ? ? Qg©g|
/   %Z?˚ ¯>  ê   ê V   † l  ±(ë&HPÏÊ)Ê'lX
 7 H  CMOVE>  ©    •%E'Ö' •%E)Ö)Ê%_$ ê à
±(ë&òP¯F'F)F%P† l  7 ı  MOVEß+| ı   ê
˚ ˛ p ˚ P H 8 1  PLACEß+  | %   / ˚ 8  H
 8 r  COUNTZ ° Ö$ I Å ± Ö%I ë • 8È Ö ∞ F
l‡ 8 P  ERASEß+≠ ¥ H 9 õ  FILL∂ ©   à•$¶
' ë(HP˚Ê)JPˆ¶& ë(HJP˙† l  :   ≠  HEREß+
W . H : }  PADß+‰   b  H : Ó  ALLOTß+W G
H :  ! ,ß+‰ g R  !H :  ! C,ß+‰  H  !H :
%!áCOMPILEß+˚ D m | .  !H ; 8! #TIBz*  ;
t! >TIBz*L!

         ; A! >INz*  ; æ! BLKz*  ; J! SP

\ *** Block No. 74, Hexblock 4a

  ANz*  ; V! TIBß+H!. H ; „! QUERYß+È!  p
.@}!. [!g D!  P!  H < Û! SCANß+| D ê
V % U ê   S ó / ó N ‰ˇe H <  " SKIPß+| D
ê     V % U ê   S ó / ó N ‰ˇe H < g" /STR
INGß+    %     F U H Iaê I{∞ IBê )I[∞  Ä
`= V" CAPITALµ"°  í"Å l  > ©" CAPITALIZEP
"° Ö$± Ö%°  $Ö&àD&P † l  H±$ í"ë$l}"Û"Ü*†
 ± ô$ à ¯†  ≠F!E&Ö&≠G!E'Ö'8•$ÌF!Ö$•%ÌG!Ö%
∞
° çF!± çG!lÖ#† •$ %=°&Q P Ê&P Ê'•$P F%
F$l0#•&Ö(•'Ö)°&Q  Ê&P Ê'•$P F%F$( Ê*•$ %
P‚8† •&Ò çF!H•'Ò çG! © E Ö ê Ê † ± Å Ö$H±
 † ë Ö%à•*ë$±(HF* ˜© ë$† l  b A" SOURCEß+
P!. È ê   ˜9vdp È![!  . H b ∏# WORDß+A#Ò"
H b }# PARSEß+| A#D!. Ä"  ó ˚  "|   U D ˚
 x U D!G H b Ó# NAMEß+S4‰#N"p H c  $ STAT
Ez*  c 5$eASCIIß+S4‰#/ V =$. ê   J H c c$
 ,"ß+  "ˆ#‰   /  !z H c C$Ñ"LITß+˚ ˚ à X
  | | H c }$Ç("ß+Ñ$H c ò$A"ß+b!ù$H$H d •$
É(."ß+Ñ$X Z?H d µ$B."ß+b!ª$H$H   d G$a(ß+
  )ˆ#Í H d X$b.(ß+  )ˆ#Z?H  d È$a\ß+D!. 8
6t / 860 D!g H d ¸$b\\ß+vdD!g H d  % \NEE
DSß+'$*/V ê    %H d )% HEXß+   Ò g H d d%
 DECIMALß+   Ò g H e w% DIGIT?ß+  0U D
 G ê     áU D    G ê   Ò .   G È  å ≠ H
e N% ACCUMULATEß+ó | ó Ò . ™ å % Ò . ™

\ *** Block No. 75, Hexblock 4b

   ˚ H e ≠% CONVERTß+/ X W%ê   ∫%N ÙˇS H
e Z% END?ß+Ë&. • H e ¸% CHARß+X ü Ë&G H e
  & PREVIOUSß+S X H f $& DPLz*ˇˇß+ê   e Í
 å e ≠ H ß+ê   e å ˚ ê   A % å ?&. / È 
å ü H ß+  &l ê      ü p   $l ê      ü p
 Hl ê      ü p   %l ê   R ü p ≠ H ß+  ,
U ó   .U     H ß+?&. ü U  H ?&G H z*  g
9& NUMBER?ß+Ò 82D X Ë&g ?&Ò ≠ | ≠ ≠ %  &c
& &  -l ê   e ü |  &c& &{&ê   Ò g  &c& &W
%• c&∫%T& &u& &W%• ê ˇ/&@&• c&?&   &u& &
N TˇH g Ó& 'NUMBER?˝0¯&g Q' NUMBERß+|'È •
  4 ?è ê   $ H h Ç' LASTz*  ß+™'. È H h £
' HIDEß+Æ'ê   ë . È,  . g H h ∫' REVEALß+
Æ'ê   ë È,. g H h W'IRECURSIVEß+‡'H ß+Æ'ê
   à V      å H h Ù' IMMEDIATEß+  @ (H h
  ( RESTRICTß+  Ä (H i 5( CLEARSTACKz(† ±
 Ö H± Ö † l  i k( HALLOTß+C .   U ó g m D
 % U D C g R ô   U 8 x(C g H i L( HEAPß+C
 . F H i •( HEAP?ß+¨(â ª H ß+D   ‰   U D
U(¨(ó P ¨(  U ™'G ‡'H • 8È Ö ∞ F • ë • Å
 ° I Ö äQ Ö ©  E häE l+ ß+˚ ™'.  +g H j ∏
(EDOES>ß+b! )  l*!b!Ï(H ß+D   ˇ=   ˇU U H
 ß+‰   ˇ=   ˇU ê   D D / ‰   U / ˛ H ™'G
H  !H k ') ?HEADz*  k Ç) |ß+ä).  ü ä)g H
 l ê) WARNINGz*  ß+∞).  ™'. È,. N.V ê

\ *** Block No. 76, Hexblock 4c

   E4™'. p+ª$ exists  ?H l ¶) CREATEß+‰ P
!.  !È,. .  !'$V D H    ª ï  4 invalid na
me‰ ™'g /  !¥)ä). ê   H ä)G D @) !J(    (
@)W g N   r)å ‡'≠  ! )l )m ‰) NFA?H*° Ö(±
 Ö)•  I Ö ê Ê ° Ö&± Ö' &P lò ±&ë Ö%°&Å Ö$
 %P l  •$ I Ö$ê Ê%°$h8) E$Ö$ê Ê%H)  ±$h°
$Ö$HÖ%•$E(  P∞•%E)P™lo n _* >NAMEß+1 . D
ê   ı Ê U ó F*È ê   F Í p N ‡ˇV H ß+X
=   H n V* NAME>ß+D  +ó V    = ê   . H n
 + >BODYß+m H n 8+ .NAMEß+È ê   D @(ê   ª
$ |X    = Z?N   ª$ ???E4H o h+ CREATE:ß+Ì
)A'È,. ˘,g I0≠ H o }+ :ß+á+ )• 8È Ö ∞ F •
 Å • ë •  I Ö äE Ö l
  o ù+A;ß+≠   b!H ≥
0‡'H o K+ CONSTANTß+Ì) ! )• 8È Ö ∞ F † ±
Å H± † ë l  o ·+ VARIABLEß+Ì)R  !H p  , U
ALLOTß+D < .     ˇG  4
Userarea full< . ó
 < G H p (, USERß+Ì)R 1,*! )• 8È Ö ∞ F †
±  E Å äHE † ë l  p ^, ALIASß+Ì)™'. D V
  = ê   ; ˛ˇ !N       ( +g H q   í, VPz*
               q F, CURRENTz*  q , CONTE
XTß+K,D .   m H ß+K,m ˘,H q Ô, ALSOß+K,.
   9 .4 Vocabulary stack full˘,. R K,G ˘,
g H q  - TOSSß+; ˛ˇK,G H r o- VOCABULARYß
+Ì)≠  !≠  !‰ 1 .  !1 g  )lÏ(˘,g H r D- FO
RTHã-  ∂j  r ñ- ONLYπ-  X@_- )lÏ(≠ K,g ˘

\ *** Block No. 77, Hexblock 4d

  ,g  -H r ®- ONLYFORTHß+Ø-û- -Ù-H s L- D
EFINITIONSß+˘,. È,g H ß+. ë ~*p+H s Ê- OR
DERß+ -U   1  .; ˛ˇ  E R X4È, .H s  . WOR
DSß+˘,. . D ¯>• = ê    ?D m p+E4N Êˇå H t
 6. (FINDP.† ± ô$ à ¯°&) Ö(† ±$™H±$Ö%Ü$ $
P † ¢ lò H±$) E(P‡ © E$Ö)© E%Ö*_(±&Q)PKàP
˜† •*ë à•)  ë à¢ lï K.° Ö$± Ö%°$Ö&) 8E$Ö$
ê Ê%•&) P •$Å •%l˜.°$Å ±$ë • 8È Ö ∞ F •&
 H 0 òiˇ®HòÅ   ©ˇ$ä† ë l  v F. FINDß+˘,D
.   ë . U ê   ë à . N.ê   V I.p   K,m G ê
   ó ë N ~ˇV ≠ H v #/ 'ß+'$*/•  4 What?H
v L/I[COMPILE]ß+P/ !H v Ñ/C[']ß+P/J H v ö
/ NULLSTRING?ß+D V •   D ê   V H H ± I Ö
H± I Ö † l  w ™/ >INTERPRETJ/40x ‡/ NOTFO
UND˝0 0x Û/
NO.EXTENSIONSß+.4 Haeh?H x  0
 INTERPRETß+Ì/H ß+R1'$*/È ê   H = ê   £ Ì
/ 4 compile only∏/ |'• ê   ˛/Ì/H ß+R1'$*
/È ê   h ê   £ Ì/ !Ì/∏/ |'È ê   h ê   ó
J J N   ˛/Ì/H y "0a[ß+; 40 1Ô/  =$  H y Ø
0 ]ß+; O0 1Ô/=$Ò H ß+ü  4 CrashH z E0 DEF
ERß+Ì); Y0 ! )† ± hH± Ö HÖ † l  z È0 (ISß
+˚ D m | . g H ß+. ; ˛/.   U ó ; Ì/. U
ï  4 not deferredH z  1bISß+P/D '1@+=$. ê
   b! 1 !p g H z* ß+ 3   9  4 tight stack
Y1V • ê   ü Y1 ü  4 dictionary fullª$ s

\ *** Block No. 78, Hexblock 4e

  till full H [ v1 ?STACKT1† 8• Ò H• Ò P
†  ë |1H é{1† ± E H± Â † ê l   ë ü  4 sta
ck emptyH \ I1 .STATUS˝0´ z*˚ ˚ g H \  2Ñ
PUSHß+˚ ó D | . | %2| | H \ 12 LOADß+È •
 P!82P!g D!82D!  !2.0H ] p2 +LOADß+P!.
 w2H ] W2 THRUß+/ ó U   1 w2V E H ] ç2 +T
HRUß+/ ó U     1 2V E H ] ™2c-->ß+H P!G
D!  !2H ] H2 RDEPTHß+M . ø m U ) H ] ‡2 D
EPTHß+g C . ó U ) H ß+=$. ê   ª$  compili
ngp ª$  okH ^ ˚2 (QUITß+!2N?˚!.0 3N ÙˇH ^
 63 'QUIT˝0>3^ r3 QUITß+M . N ≥0z3H ^ `3
STANDARDI/Oß+; *   Ê P H ^ W3 'ABORT˝0´ ^
 ï3 ABORTß+x(N û3Ö3G  3H _ _3 SCRz*  _ º3
 R#z*  _ H3 (ERRORß+Ö3E4‰ p+X Z?E4 ?P!. È
 ê   B3g D!. M3g G3H _ S3á(ABORT"ß+Ñ$ó ê
  | x(˚ ! G p å H ß+Ñ$ó ê   ! G p å H _
4FABORT"ß+b! 4H$H _ d4FERROR"ß+b!.4H$H `
y4 BLÙ+  ` N4 -TRAILINGá4ò  ° Ö& ± E%Ö'_
$ ê à±&I  HP Ê%òh•%l+ òPÍF'F%     | ô" D
isk 3  --  Forth-Sourcecodes 5 Ü ô" - 650
2-Assembler q ê ô" - Full-Screen-Editor J
 ö ô" - Debugging Tools  _ ô" - Multitas
ker ó Æ ô" - Printer Driver ° ∏ ô:ô:ô ø B
 ô" Disk 4 --  Grafic/Tape · L ô" - Grafi
c    (only for C64) ˜ V ô" - Tape-Versio

\ *** Block No. 79, Hexblock 4f

  n
‡ ô" - Supertape (only for C16/+4)
%
Í ç640 b
Ù ô" Hardwarerequirements: h
˛
 ô G
  ô" C64/SX64    with Floppy ê
  ô"
            or   Cassetterecorder _
  ô:ô
"      or":ô I
& ô" C16/C116    with min.
32kB RAM Ì
0 ô"             and Floppy Dr
ive   : ô:  ô"      or":ô & d ô" C16/C116
/Plus4  with 64kB RAM k n ô"
with Floppy Drive Q x ô"             or C
assettrecorder Y B ô:ô ® L ô" Have Fun wi
th volksFORTH-83 /cas18aug06 Æ V Ä E Ä è
**************** Z ä è ende der seite Ò î
 è ****************   ˜ û è " ® ô"
            ":è positionieren p ≤ ô"*** (
C)1985-2006 Forth Gesellschaft *** ~ ∑ ô"
*volksForth Website:                  * ¨
 º ô"* http://volksforth.sf.net
  * Z F ô"* http://www.forth-ev.de
       *   Z ô">>>>press key t µo continu
e   <<<<*******   ‰ ° e$: ã e$≤"" â740 6
Ó è **************** j ¯ è start of page
A   è **************** J   ô"ì" ì   ô"
     volksFORTH 83 - rev 3.80.1 õ * ô:ô °
 4 é        * Z F ô"* http://www.forth-ev
.de              *   Z ô">>>>press key t

\ *** Block No. 80, Hexblock 50


   û(4112)    ÍlzaÍlìal W ˛6 W W {Ük
    4dZb|3Hj





         LOGOz* volksFORTH-83 3.80.1-C16+
 çˇˇ± Ö ≠ˇˇÖ  • I Ö ∞ LˇˇÊ ∞˘     END-TRA
CEP ©•Ö © Ö ©IÖ © Ö l    B  RECOVERz*HÖ H
Ö • P F F lß+  Ö  NOOP    _  ORIGINÙ+
Ø  S0Q,   æ  R0Q,   H  DPQ,   R  OFFSETQ,
   |  BASEQ,    Í  O  UTPUTQ,   ˜  INPUTQ
,      ERRORHANDLERQ,      VOC-LINKQ,   &
  UDPQ,   6  SP@i • Ö$• Ö%¢$• 8È Ö ∞ F µ
ë µ ¢ l    a  SP!S ° ™± Ö Ü ¢ l    K  UP@
ã ¢ ls   É ÉUP!ö ¢ ± ï à± ï ¢ † •  I Ö ê
Ê l    í  RP@A ¢ ls   π ÉRP!P ¢ lú   H Ç>
R~ • 8È Ö ∞ F ° Å ± ë l©   W Ç  R>˝ • 8È
Ö ∞ F ° Å ± ë ©  E Ö ê Ê l    ˆ  R@' • 8È
 Ö ∞ F ± ë ° l      ÖRDROP    =  EXITr °
Ö ± Ö l    i  UNNESTJ ° Ö ± Ö l    _  ?EX
ITÅ °    •  I Ö ê Ê (Pæl    W  EXECUTE• °
 Ö ± Ö •  I Ö ê Ê l    ô  PERFORMß+. £ H
  Ω  C@X ° Ö$± Ö%© ë °$l    Q  C!Ú ° Ö$±

\ *** Block No. 81, Hexblock 51

   Ö%H± Å$à•  I Ö ê Ê l    Î  CTOGGLEß+à
V z ó  H      @0 ° Ö$± Ö%±$ë °$l    *  !
i ° Ö$± Ö%H± Å$H± † ë$l    c  +!I ° Ö$± Ö
%H±  A$Å$H± † Q$l[   B  DROP©   Ö  SWAPô
± ™† ± Ö$äë •$† ë H¢ ± Ö$° ë à•$l    ê  D
UPF • 8È Ö ∞ F † ± † ë H± àl    æ  ?DUPÎ
°   P l  l  F   ‚  OVER  • 8È Ö ∞ F † ± Å
 H± † ë l    ˘  ROT' † ± Ö%† ± † ë † ± Ö$
•%ë † •$ë H± Ö%° ë † ± Å •%ë † l       -R
OTß+% % H   _  NIPß+ó å H   P  UNDERß+ó
 H   Ä  PICKß+/ ? g   . H   í  ROLLß+D |
ô g D m ˚ / ? ˛ å H   ©  2SWAPß+% | % ˚ H
   L  2DROP    ‚  2D  UPß+    H   Ó  +
H° Q ë à± † Q ë l•   ˇ  OR" H°   ë à± †
 ë l•      AND? H° 1 ë à± † 1 ë l•   7  X
OR\ H° q ë à± † q ë l• ! t  -W H± 8· ë H±
 † Ò † ë l• ! Q  NOTó  ä· Å äÒ ë l  ! è
NEGATE≤ 8∞„" ß  DNEGATEC H8äÒ ë HäÒ ë ä·
Å † äÒ ë l   ™®à± ô$ à ¯ä E Ö
ê Ê ¢ † `
" ∑  D+  ©   H±  E&ë H± E'ë ° E$Å † ± E%
ë l  # ¸  1+1 ©  A ∞ l  Å ± I ë l  # *  2
+o © P‡# h  3+\ © PSB © PMH © PG# u  1-U
8° È ê l  Å ± È ë l  # N  2-ì  ê‡$ å  TRU
EÙ+ˇˇ$ ò  FALSEÙ+  $ • "-1ü $ ≥ !0≠ $ º
1Ù+  $ D  2Ù+  $ N  3Ù+  $ X  4Ù+  $ ‚

\ *** Block No. 82, Hexblock 52

  ONß+ü ó g H $ Ï  OFFß+≠ ó g H % ˝ ÑCLIT
  • 8È Ö ∞ F ° Å äë Ê P Ê l  %   ÉLIT= •
8È Ö ∞ F ± ë ° Å •  I Ö ê Ê l  % 5 GLITER
ALß+D ;  ˇ= ê   b!;  !p b!  *!H & `  0<ë
±   ©ˇ$äë l  & ä  0=ß °   ËPÈ& †  UWITHI
NΩ ©   † ° E$± Â%∞ ° E&± Â'∞Ωlò ' ±  <„
° Ö$± Ö%•    I Ö ê Ê •%q 0ï° E$± Â%lì ' }
  U<  ° Ö$± Ö%•  I Ö ê Ê ° E$± Â%∞ lï lò
(    >ß+ó · H ( 5  0>ß+∞ è H ( c  0<>ß+•
ï H ( r  U>ß+ó   H ( B  =ß+U • H ( Q  D0=
ß+  • H (   D=ß+A   Ö H ( è  D<ß+% ı U ê
   9 V V N   Í   H ß+e ê   ó å H ) †  MIN
ß+ı 9 A H ) Q  MAXß+  ı · A H ) „  UMAXß+
ı   A H ) ı  UMINß+ı G A H )    EXTENDß+D
 è H )    DABSß+$ ê   A H ) .  ABSß+$ ê
 ∞ H ß+e ˚ m D | % | ó | | H * c É(DOß+
U u H * O Ñ(?DOß+  U È ê   u ˚ D .   | å
H * Å  BOUNDSß+    ó H * ¶ áENDLOOPG © l
 + ª Ö(LOOPX  © A Å ê ± I ë ê   l  † ± Ö
à± Ö † l  + N Ü(+LOOP   ° A Å ± Q ë Jq  •
  I Ö ê Ê ( Dl  , ˛ ÅI3 † • 8È Ö ∞ F  ± H
HQ Å à± HHQ † ë l  , - ÅJ_ † PR- y ÜBRANC
HP  • A Ö$• Q Ö •$Ö l  - E á?BRANCHí °
 •  I Ö ê Ê (Klp . Ü  >MARKß+‰ ≠  !H . ™
  >RESOLVEß+‰   U ó g H . æ  <MARKß+‰ H

\ *** Block No. 83, Hexblock 53

  . Y  <RESOLVEß+‰ U  !H . È  ?PAIRSß+U
4 unstructuredH I   h©ˇl+ h© ™• 8È Ö ∞ F
ä† ë H¢ l  /    CASE?n ©   •$A P •%Q P l
ï äl( 0 d BIFß+b!ê ≤ H H 0 H DTHENß+i H
 I H 0 { DELSEß+H   b!N ≤ ó I ü H 0 ê EBE
GINß+· R H 0 ≠ EWHILEß+R   R b!ê ≤ ; ˛ˇT
H ß+Ù D ;   ˛ˇU ê   å I N ÓˇH 0 ø FREPEAT
ß+R   b!N } H 0 ˘ EUNTILß+R   b!ê } H 1
 BDOß+b!U ≤ | H 1 * C?DOß+b!à ≤ | H 1 = D
LOOPß+|   b!V b!E I H 1 q E+LOOPß+|   b!
 b!E I H 1 L ÖLEAVEß+E ˚ ë D .   | H 2 à
 UM*¨ ± Ö$° Ö%HÜ&Ü'¢  '&&&%&$ê  ± E'Ö'H±
àE&Ö&ê Ê%P Ê$JP|•'ë   H•&ë † •$ë •%Å l  3
 _  M*ß+D è D | ê   ∞ ó D è ê   ∞ ˚ ï | ™
 ˚ ê   A H 3 ˜  *ß+™ å H 3 ,  2*a °  Å ±
*ë l  ß+ü  4 division overflowH 4 :  UM/M
ODU ° Ö)± Ö(•  I Ö ê Ê ° Ö%± Ö$H± Ö'H± Ö&
¢  F*8•%Â)®•$Â(∞ &*ê Ö$Ñ%&'&&&%&$JP† F$F
%ê  ë n H •&ë H•%ë H•$ë † •'l    5 J  M/M
ODß+D | i   è ê   à   ó S % è ê   ∞   ê
 ó %   ó S e H 5 ‰  2/+ ±  ± Jë ° Jl  6 $
  /MODß+| $ ˚ Ï H 6 ;  /ß+b V H 6 p  MODß
+b å H 6 ^  */MODß+| ¸ ˚ Ï H 6 N  */ß+V V
 H 6 Ñ  U/MODß+≠ ó S H 6 ì  UD/MODß+| ≠ %
 S ˚ ó | S ˚ H 7 ß  CMOVER ©   àD$P F%

\ *** Block No. 84, Hexblock 54

    #   ôG(14):è switch to lower case 3
 ç750:è page v   ô" volksFORTH 83 is a co
mplete Y   ô" FORTH-83 System for C64,C16
 ä ( ô" and Plus4 ê 2 ô µ < ô" Versions f
or Atari XL, Apple2 V f ô" MS-DOS, CP/M a
nd Atari ST Â p ô" exists.   z ô" Please
read file   COPYING in ( D ô" Distributio
n Package for > E ô" License Detail o N ç
 640:è page S X ô" volksForth can be down
loaded ë Ç ô" for all platforms from ¥ å
ô" http://volksforth.sf.net or R ñ ô" htt
p://www.forth-ev.de X † ô   ™ ô" PDF Hand
books are also avail  able   ¥ ô / æ ô" F
orth is using its own filesystem [ H ô" s
o please copy always the whole disk Z R ô
" and not only the files! Ä | ô Ü Ê ô õ 
 ç 640:è pageskip µ ˙ ô" The Disks contai
n: ª   ô Y   ô" Disk 1 --  System-Disk
  ô" - File c16ultraforth83 -   Systemker
nal 3 " ô" - File c64ultraforth83 - Syste
mkernal ` , ô" - File c16demo - Worksyste
m with Tape è 6 ô" - File c64demo -
''         Grafic • @ ô" - Help Screens º
 j ô" - Diskutilities B t ô Ô ^ ô" Disk 2
 --  Sourcecode of Forth Kernal ¯ H ç640

\ *** Block No. 85, Hexblock 55

\\   Dies ist der Shadow-Screen
          zum Screen # 0

 Der Screen # 0 laesst sich nicht laden
  (ist der Inhalt von BLK gleich 0, so
   wird der Quelltext von der Tastatur
  geholt, ist BLK von 0 verschieden, so
   wird der entsprechende BLOCK von der
      Diskette geladen), der Editor
  unterstuetzt deshalb auch nicht das
  Shadow-Konzept fuer den Screen # 0.













FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 86, Hexblock 56

Shadow for Scr# 1

Screen # 1 should always contain
a directory listing
a good Rule is:

 FIRST  edit the entry in screen 1

 THEN   edit the sourcecode

If needed screens 2-4 can also be used
for the directory listing








            BTW, have you done

          A BACKUP OF YOUR DISKS?

FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 87, Hexblock 57

Shadow for Scr# 2

The Editor is designed to prevent
accidently deletion of text, neither
at the end of a line nor at the end
of a screen will txt disappear


If you like to clear a whole screen,
use HOME to jump to line 0 and press
SHIFT F1 (=F2) to move all lies up
untill the whole screen is filled with
new empty lines


If this is too much work, define:

: wipe  ( -- )   \  fill block with
 scr @ block     \  spaces
 b/blk bl fill ; \\

and use WIPE after LISTing the screen
to be cleared

FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

\ *** Block No. 88, Hexblock 58

FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv

























\ *** Block No. 89, Hexblock 59



























\ *** Block No. 90, Hexblock 5a



























\ *** Block No. 91, Hexblock 5b



























\ *** Block No. 92, Hexblock 5c



























\ *** Block No. 93, Hexblock 5d

\\ simple file system          20oct87re

A FOLDER is a single connected screen
Area, containing a Directory Screen
with File- and Folder-Names and
their relative screen numbers:

..     -&150        .            0
FILENAME  $D        FOLDERNAME &13

The ROOT-ORDNER is the whole Disk
with a Diretory in Screen 0. All Screen-
Numbers in ROOT are absolute.

All Screen-Numbers must be maintained
manually.

When moving a complete folder, only the
Screen offsets in the Parent-Folder
needs to be adjusted


.. and . are not mandatory, but without
then the user cannot access the parent
directory

\ *** Block No. 94, Hexblock 5e

\\ simple Filesystem           20oct87re

SPLIT cuts a String at first occurance
 of CHAR. The first part will be stored
 above, the remainder below.
 The Rest$ can contain CHAR again.

READs Path and Filenames. Syntax:

 cd /
     Current DIR becomes ROOT-Directory
 cd /Sub1/
            DIR becomes SUB1 from ROOT
 cd Sub2/
        DIR becomes Sub2 from current
 cd ../
        DIR becomes parent DIR
 ld /File1
        Load File1 from root
 ld File2
        Load File2 from current DIR
 ld /Sub3/File3
     Load File3 in Sub3 from ROOT
 ld Sub4/File4
     Load File4 in Sub4 from curr. DIR

\ *** Block No. 95, Hexblock 5f





  HELP  !!!










       Hummel, Hummel - Forth, Forth










\ *** Block No. 96, Hexblock 60



























\ *** Block No. 97, Hexblock 61

\ Comment to numbers          14oct85re)




alphabetic - not HEX or DECIMAL
  09 follows 08, 0A follow 09
  next 0B etc. until 0Z, 10 follows 0Z
  then 11  ...  19, 1A, 1B, 1C, ...




hex-Zahl  alphabetic display
hex-Zahl  alphabetic display
hex-Zahl  alphabetic display


The other way is also possible
 (this is how we created the hex
  numbers of "numbers" )

Do you need BINARY or OCTAL ?
 : binary  ( --)   2 base ! ;
 : octal  ( --)   8 base ! ;

\ *** Block No. 98, Hexblock 62

\\ relocating the system    bp 19 9 84 )

up@ origin -   is stacklen
r0 @ up@ -     is rstacklen

symbolic map of system

FUNKTION     TOP          BOTTOM

disk-buffer  limit        first @
 unused area
rstack       r0 @         rp@

 free area

user, warm   up@ udp @ +  up@
(heap)       up@          heap
stack        s0 @         sp@

 free area

system       here         origin 0FE +
user, cold   origin 100 + origin
screen       0800         0400
page 0-3     0400         0000

\ *** Block No. 99, Hexblock 63



























\ *** Block No. 100, Hexblock 64



























\ *** Block No. 101, Hexblock 65

\\ disassembler 6502 loadscr







+n Values will be placed inside an
Array
















\ *** Block No. 102, Hexblock 66

\\ dis shortcode0

Table for the complicated even opcodes



Table-Solution



















\ *** Block No. 103, Hexblock 67

\\ dis scode adrmode

Helptable for odd Opcodes



Helptable for odd Opcodes
 Addressmodes


calculates from Opcode 8b1
the addressingmode 8b2
and commandlength +n
for odd Opcodes












\ *** Block No. 104, Hexblock 68

\\ dis shortcode texttab

calulates Commandlength (length)
and Addressingmode (mode)
from Opcode (8b1)






defining word for Text-Tables

Datastructure:

count text1 text2 ... text+n-1 text+n










\ *** Block No. 105, Hexblock 69

\\ text-tabellen

the Mnemonic-Table








the BEFORE Table



the AFTER Table










\ *** Block No. 106, Hexblock 6a

\\ dis 4u.r 2u.r

4u.r print address with leading zeros


2u.r prints byte with leading zeros




















\ *** Block No. 107, Hexblock 6b

\\ dis shadow

"dis" is an ugly word. Better
names for this word are welcome






















\ *** Block No. 108, Hexblock 6c



























\ *** Block No. 109, Hexblock 6d



























\ *** Block No. 110, Hexblock 6e



























\ *** Block No. 111, Hexblock 6f

\\ savesystem shadow          clv04aug87


Usage: SAVESYSTEM name
Example: SAVESYSTEM MY-FORTH
        --creates a loadable File
        --with Name MEIN-FORTH.

  A complex program can contain it own
  SAVESYSTEM buildng on top of the
  generic one, preparing it's own
  Datastructures for later loading.














\ *** Block No. 112, Hexblock 70

Comment for  bamallocate, formatdisk

Creates Entry "0 blocks free"
 open Disk
 read BAM
  IF clear BAM (almost)
     writes back BAM
  THEN  closes disk
 initialize again
 and finish!

CLEAR   (formatted) Disk
 send clear command
 send name
  sucessfull?
 pretent the disk is full










\ *** Block No. 113, Hexblock 71

???

  This page unintentionaly left blank.













copies no dictionary. cannot be used
for Files, only for FORTH Screens








\ *** Block No. 114, Hexblock 72

\\ zu:2disk copy2disk..       clv04aug87


this calculates the cunmber
[0..&682] out of track and sector





















\ *** Block No. 115, Hexblock 73

\\ zu:..2disk copy2disk       clv04aug87


Checks for Disk error



Abbreviation for OPen. 40 Chars are too

copies a secotr (using adr)





 Check if there is Space at PAD
 Status messages

 Loops over allsectors
 prints out some numbers


 switches only(!!) 1551-Floppys.
 For 1541 its too complicated.


\ *** Block No. 116, Hexblock 74



























\ *** Block No. 117, Hexblock 75



























\ *** Block No. 118, Hexblock 76



























\ *** Block No. 119, Hexblock 77



























\ *** Block No. 120, Hexblock 78



























\ *** Block No. 121, Hexblock 79



























\ *** Block No. 122, Hexblock 7a

   È
h2J π B   Ô ° |   È
h2º º ı J π Ï1~

\ O 82 COPYR#º V2\ O T2 CONVEYR#ã # F ç
I
 ∏ < â ‡+ neinV2\ P Â2 LIMITø# @P
3 FIRS
T."@üP  3 ALLOTBUFFERR##3" B " I %/a   S
%/_ #3[ #3" ∏ {0 /" Ù ;  /; \ P )3 FREEBU
FFERR##3"  3%/I   Ñ . #3" !0 /∏ " #3" I Ñ
   " B Óˇ#  Â'l≤4•(E,•)Â-∞ °,Å$±,ë$lG4•,Ö
$•-Ö%lâ4lù R#% " } Ñ   ∏ P Z I K4Ô B Íˇ\
R#} # ∏ " } I Ñ ( È
" ã X Ø Ñ   ∏ " # " Ù
 ; G B   " B Nˇ~
\ R#% K4R$I   È
% " z
Ø
Ñ   / K%% ; / ˛ˇ˚ π È
¥$" z
Ø Ñ   / K%¥$;
 \ S ∫3
CUSTOM-REMOVEI(† R#Ù Î4'5M4{5Ä ã
I i L ; ° ~ ; \ T K5
R#ª7Ñ   Ö6Ö6° \ U ô
6 ?CRR#J7 .   I [ Ñ    7\ R#A!∏   a Î l@
J Ù " ˜
ª \ V ≥6 OUTPUT:R#r#Î l@ Ù ; \ V
Î6 EMITZ6 V  7 CRZ6 V  7 TYPEZ6 V  7 DELZ
6 V (7 PAGEZ6 V 37 ATZ6 V ?7 AT?Z6 V i7 R
OWR#o7Ä \ V t7 COLR#o7J
\ R#A!∏   a Î l@
J   " ˜
ª \ V D7 INPUT:R#r#Î l    +„((+R<
ñ8/ . "   ; / † „(L+X+\ † π9 ô  à ˜ ≠  I
Ö ≠! I Ö † ± Ö H± Ö † ± Ö H± Ö ¢ † äÅ ë `
Z ã8 COLD 9¢ˇö pa ≠  I Ö$≠! I Ö%† π  ë$HP
¯ æ8 Ü /aQ8\ Z ˙8 RESTART<9¢ˇö pa æ8 Ü /a
ö8\ ¢ † l  Å 09 C64KEY?C9•F ©ˇhl  Å w9 G
ETKEYZ9•F X¨W ΩX ùW Ë‰FPıFFòxI†P © l  Ç

\ *** Block No. 123, Hexblock 7b

  : CON!°:°   Á•  I Ö ê Ê ÜTÜX†  Ü Ç.\ IÄ
∞ I `I‡∞ I@` `Ñ ò: PRINTABLE?:°  æ:ê Jäl
  Ö P: C64EMIT˘:°  æ:∞ ©.l£:Ö Ì: C64CRR#
:ü:\ Ö  ; C64DELR#  ùü:ë,  ùü:\ Ö  ; C64P
AGER#  ìü:\ Ö 4; C64ATs;©  S ¶&_$  ˇ_S±Q
ÖNln9Ö i; C64AT?V;• 8È Ö ∞ F äë 8 ˇ@(òê
È(hä¢ Å Hl   ¢  Ü Ä<@.\ ¢ †  Ü è< /@.ì ‡+
 no device\ Üê ±ˇ©` ìˇ Æˇ•ê HHl°<`W<°  æ
<•  I Ö ê Ê lî<â Ü< ?DEVICER#Ä<ç.U<\  =Üù
©  S •& æ<•& ±ˇ•$ ` ìˇ¶&Üöln9ä Ï< BUSOUTR
#Ä<ç. =\ ä #= BUSOPENR#    ,=\ ä 8= BUSC
LOSER#  ‡  ,=è<\ I=Üù©  S •& æ<•& ¥ˇ•$ `
ñˇ¶&Üôln9ä o= BUSINR   ˜
d g>I    6    ˜

\ R#k># ã \ R#Ê  *t ° |,e-Ä   ,E,e-Ì,\ ø#
  ç  > READSECTORR# <   ,=ë  u1:13,0,L ª=
∞>ª=è<Ç. >S  <
í=K>Ô=è<° \ ç Q> WRITESE
CTORR#
 <   ,=ë  b-p:13,0L ª=è< <
,=K>
ª=è< <   ,=ë  u2:13,0,L ª=∞>ª=è<Ç. >\ é
? DISKOPENR# <
b=  #•=è< >\    ã I 1  7
% F Ñ-Ë ∏ % 3 3 ¶>~>P #   %#7Ô °6  Ñ   Ñ
J π ù?\ è W@ INK-POT."              h≠‚@Ö
 H@Xç·@©@h©„h ≠·@hähòh• ç‚@©6Ö ©ç
}¨
}
lR˛ Íˇ ·ˇPıl<9ë I@ INIT-SYSTEMR#/ @ˇ∏   @
D / Î@∏ / ˙ˇ; /   ; \ XX Ñˇ Åˇ äˇ©6Ö ≠U@ç
 P≠V@ç!P≠W@çÜ ©Äçä © ç P© ç P© ç P© çà x

\ *** Block No. 124, Hexblock 7c

  `ë !a C64INITöa paln9{ éac(16R#  L ô ‡+
 C) missing" / C)I Ñ „ˇ\ { ¢abC)R#\ { Mac
(64R#\ { Xa FORTH-83R#\ | ‰a ASSEMBLERv%
z zÄ%|    PUSHAø#  |  b PUSH0Aø#  |  b PU
SHø#  | (b RPø#  | 5b UPø#  | @b SPø#  |
kb IPø#  | vb Nø#$ | Ab PUTAø#  | Kb Wø#
 | Xb SETU  \ M ú1 BLOCKR#ç1}1\ N Ø1 UPDA
TER#  Ä /" z # ‰ \ N A1 SAVE-BUFFERSR# /ç
. 1} Ñ   !0B Ùˇ /@.\ N }1
EMPTY-BUFFERSR#
 /ç. /" } Ñ   ∏ {0B Úˇ /@.\ N  2 FLUSHR#Ï
1 2\ R#∏ ç1n1Ñ    /" {0,1Ñ   Ï1Z " ˜
ã ∑1
Ö Ö ; J1\ R#Ï1P Ù   ˜
Ù [ P È
  Ô 1 Ñ $
   ı Ô ° |   ì / ˛ˇı  3" " ã ; %/#3[ \ P
K3 ALL-BUFFERSR##3" 73#3" I Ñ Úˇ\ R#J
1 \ R#Í"|
#   ã î   \ R#Ä % " P Ô " } Ñ ^
 ∏ P Z I P Ô " P Ù G ∏     ã   a Í"  1 Ñ
.   î Ñ Zˇ  a |3Ñ   Ù   a Á3Ñ     a S"a 
 B ∂ˇ9 B úˇ\ M4† ± ô$ à ¯†  ± I Ö*H± I Ö+
† °$Ö,±$Ö- ,5•,E*•-Â+ê •,E&•-   CLEARR#X
 ∏ } 5L ; \ T °5 (FORGETR#∏ î ‡+ is symb
ol˘35\ T ª5 FORGETR#;'∏ / $ "   ‡+ prote
cted©"∏ î Ñ   Í"B   Ö Ö E5\ T 5 EMPTYR#/
 $ " } 5/ 2 " 0 ; \ U  6 SAVER#X } 5% "
 ∏ Ö Ö " Ù Ö ; " } ô Ñ Íˇ} ≠ /   D \ U >6
 BYER#Ï1„;\ R#Ø7P I Ñ   ì 9 \ U W6 STOP?

\ *** Block No. 125, Hexblock 7d

  @   ; \ V è7 KEY~7 V ©7 KEY?~7 V ¥7 DEC
ODE~7 V @7 EXPECT~7 W    SEALR#° / Z% #;
\ W |7$ONLYZ%W Û7%FORTHI%W ˛7%WORDS &W  8
$ALSOÂ$W  8+DEFINITIONSø%R#% " ∏ Ö " Ù Z
I ; " } ô Ñ Íˇ\ R#°  /;  3#3; H3\ X N7 'C
OLDI(F{R#18q8£%M8:7  L #7 7ö8\ X E8 'REST
ARTI(áZR#/   O9 CURON¶9_S±QÖNÜLln9Ç ú9 CU
ROFFæ9HÑLÑMÜO•N_SëQ† l  É ≥9 C64KEYR#_9Ç.
A9Ñ ˙ˇº9X9\ É R9 #BSø#  É Ô9 #CRø#
 É ˚9
C64DECODER#ı9@ Ñ   ∏ Ñ   .7G d  :@ Ñ   ∏
Q ; d P È
˜
  ã ‰ Ô
7# \ É  : C64EXPECTR
#Q ; ° ∏ Q "   Ñ   Ø7I7B Óˇ~
ë,\ É o: KEY
BOARDû7{9A9 :[:\ Ñ     Ü K; C64TYPEß;©
S † D$ ±& æ:∞ ©.  ÁHlÆ;l±:Ü õ; DISPLAY˚6
˜: ;•;";>;q;T;\ ‚¸á G; B/BLKø#   á Á; BLK
/DRVø#™ á ˆ; (DRV."  R#
<"    ˜
\ á  < DR
IVER# <$ Z ; \ á  < >DRIVER# <$ ˜
Z " I \
 á 6< DRV?R#Z " ˜
 <h \ á q< DRVINITR#† \
 à H< I/O."  à Z< BUSOFFë< Lˇ†  #Ä<ç.G=\
ã ä= BUS!ß=°  ®ˇlô ã û= BUSTYPER#£ |   %
J •=J π Ç.\ ã ±= BUS@|= •ˇl  ã S= BUSINPU
TR#£ |   Z=% ‰ J π Ç.\ ã ‰= DERROR?R# <
 í=Z=∏   0I Ñ
7Z=∏  :I Ñ Ùˇô  7ô è<\ ø
#E ø#Í ø#v R#∏ ?>  Ñ      6 d P ˜
∏ c>  Ñ
   ?>I    6    ˜
d ∏ g>  Ñ   c>I    6

\ *** Block No. 126, Hexblock 7e

  é P? DISKCLOSER# <
z=è<\ é ë? 1541R/W
R#ã ‡+ no filez
 <6 ∏
<; P [ Ñ   π-Ø  be
yond capacityJ
d {?Ñ   Ä J
d ° ã 3 3 Z £
I . Ä È
%
Ñ   ¶>~>B   ¶>#?P K>˜
Ô ∏ Ñ
 Ñ J π z
~
ù?\ è ¨? INDEXR## ã I !  7% F
Ñ-% ∑1#   %#7°6Ñ   Ñ J π \ è b@ FINDEXR#{
?Ñ   ~
d #  Pø#S | Çb NEXTø#  | êb XYNEXT
ø#n9| ùb POPTWOø#Ù | ¨b POPø#ù . ıa .BLKR
#D " } Ñ   Ø   Blk H-π6\   Gb (SEARCHˆb†
± ô$ à ¯•( )I•$ %C°&A* •$P F%F$Ê&P Ê'l
 c† π$ ë à ¯Ê&P Ê'Ê*P Ê+•$P F%F$•(P F)F(•
( )P •  I Ö ê Ê † •'ë •&àë àlâ •$ %P •  I
 Ö ê Ê † lå °&A*®†     $d CBM>SCRìd°  Md
Å l    ád SCR>CBM´d°  \dÅ l    üd EDITORv
% r r bø#( ø#  ø#Ë      SHADOW."u ."  ."
 ."o ."ˇˇ."  ."ß|  ."˜|  ."  w|."  |ø#Q
ø#S ø# Pø#Ü ø#!P•$ I)Ö$ê Ê%•& I(Ö&ê Ê'`<e
ò S Ü&≠à Ö'¢ †'±$ Mdë&à ˆ #eJPÓ≠Ü ù Xù Yù
 Zù {ËPÒludReò S Ü&≠à Ö'¢ † ±&  1 )fâ 1 ∏
 S   îü:\ R#cf} S Ád" Ñ   Gf} S [f\ R#Ôd"
 ô z7LdÖ [ 1 ∏ S    ü:\ R#cf∏ S    ü:\ R#
ÓePdq,J
PdG K Hd6 ã †c° \ R#°    ° I   Ä
Kf∏ Ñ   Ñ J π \ R#   ° I     ùü:J π ° \ R
#z7° †c-dÁd˜ Æf\ R#¯eHd?,® ° \ R#
fHdJ7I
?,® ° \ R#¯e∏ Hd˜
ã Óe/ @ ˜
∏ P Ù I D Ô

\ *** Block No. 127, Hexblock 7f


dD \ R#˝da " ô } S âg∏ S Hh\ R#ÁdÂ ° \
R#Ád˜ ° \   ∑d RVSONR#   ü:\   ∞h RVSOFFR
#  íü:\   Ch ***ULTRAFORTH83***    WhrFOR
TH-GESELLSCHAFTR#  \   Vd STAMP$."    06n
ov87re     R# i# L ÓeHd˜
Ù I ã ùe° \
i
 GETSTAMPR#   *ä: iÂ  7Ø  your stamp: ∏h
  _,z7   †   ∏ Hd# x HdI I Ô;G K Hd# 6 ã
†c\ R#-d   `j\ R#öjé+" ∑1∂eÑ   *iÄ é+" ∑1
PeJ1     \ R#•j     \ R#Jj     \ R#é+" ∑1
:eÁd˜ -dTj° \ R#•jÄ º é+[ ‡j\ R#•jÄ ì é+[
 ‡j\ R#•jÄ d" ∏ ‡iJ
[ â Ñ   _ é+[ ‡j\ R#
•jÄ „d" é+" „d; é+; ‡j\ " h DIGDECODER#ı
9@ Ñ   ∏ Ñ   .7G d     P µkW7Q " } Ñ   X
|
‰ _ Ä Ûd; †cÛd" P ò-ä:È
†c ea " HdÖ J7I
 W7Q " } Ñ    e; †c eJc˝kÈ
†c ea " HdÖ J7
I W7Q " } Ñ    e; †c eJc˝kLh∏i†c\ R#‡jÄ
eJcô+" é+" ∑1˜
Ô;ô+" I Ùbô Ñ   ° B   é+"
∑1I ô+; Tj\ R#•jÄ  l¸lô+" Ñ ' Ø7∏   rI Ñ
  Ak} Ñ   J
d P I } S B Sˇ‡iJ
  ∏ ì I Ñ
 wnô \ ( fi (PAD."  R#Ë ∏ än; Hd3 ˜
∏  ea
 ; Hd˜
∏  ea ; Hd˜
∏ ˜d; Hd3 ˜
˝d; ˜da ˜
˝da ˜ / YhL P  ea "   D Ô  e; / ÚhL    1
P  ea "   D Ô  e; \ R#Ë än" I S én\ ø##
o_S±QÖNiÄëQln9.o•N_SëQln9R#/ d  o; Lh,o\
R#D " } ô Ñ   7od ∏ " G ô+; Lh,o∏hé+" Ù

\ *** Block No. 128, Hexblock 80

  F 1 ô Ñ   Ø  not Ø  changed∏ Z 1 Ñ   Ï1
∏    1    I Ñ   Ø  , saved   1 Ñ   Ø  , l
oading 7é+" ô+" @o\ , =p LR#ô+˜ dp\ , Sp
RR#é+" dp\ , „p +LR#é+" ˜
Wp\ , Ûp VR#;'©
"} Ñ   Z I " \ ,  q VIEWR# q} Ñ   WpB   Ø

from keyboard\ , Én CURLINR# e"   UJ q,
∏   S‰ \ -  i∏  *˜ än∏  *˜ e6   F b=° Í ª
=ë  ,p,wL ª=è<   F ,=≠    I ∏ /   è ã •=•
=X Ù I +rè<   F z=°
<;  >‡+ save-error\
  ªb END-CODER#D$Ö " D$; \   Zr INDEX."
     Ä Ä
  ÄÄÄÄ    ÄÄÄÄÄ   ÄÄ,."    ır MO
DE:R#A!  Î l@ J  s; \   %s .A5s   bs #5s
5s   ls ,X5s   xs ,Y  5s   Bs X)5s   Ls )
Y5s   Vs )5s R#Ñ    s"       s; º  s"
1 } Ñ   ° I   ∏ ˜
J π Ù # " 1 ô \   Äs CP
UR#A!  Î l@ J   ss\   Gs BRKUs   ‚s CLCUs
   Ìs CLDUsX  ¯s CLIUsx   t CLVUs∏   t DE
XUsJ   t DEYUsà  $t INXUsË  /t INYUsH  :t
 NOPUsÍ  et PHAUsh  pt PHPUs    ± ô$ à ¯•
$P F%F$Ê&P Ê'l cR#d7º9\   Íb 2!±cò S † ±
ë$à ˘† lÙ   ™c 2@Lc° Ö$± Ö%• 8È Ö ∞ F † ±
$ë à ˘ln9  Ec 2VARIABLER#A!Z ˝ \   Ìc 2CO
NSTANTR#A!    Î l@ Jc\    d UNLINK/d≠à  Ä
®äîY I(ê HË‡ PÛ•SI(ê È(ÖS_S¶V  ˇ¢ † l  Ö
*)? *$*   ÄP  @`Ö*)I ê
I@ê ) $*   @`©.`

\ *** Block No. 129, Hexblock 81

   \dë$H@(êÙJ © ë$ #eleludüe©  S àD$P l
ud±( Mdë&HPÔ∏eò S Ü&≠à Ö'¢ †'±$ MdQ& ©ˇl
  à Ô #eJPÁäl  R#; Hd3 I \ eäh≠à l  ˙e•S
I(äê ©'EQh•RI l   f •SEQh•RI l   f•UI(∞ J
äl  R#Óe/ @ ˜
Hdq,J
ô Ôd"   \ R#
fÓePd˜
G
 I Ôd" ô 1 \ R#ÎdJ ü:° \ R#cf} S  f¯eHd˜

G J ?,I â   Hd?,® ° \ R#)fâ } S ¯e∏ Hd˜
Ó
ePd˜
Ù I Ú ?g\ R#Kf∏ S    ü:\ R#˜dJc˜
# ˝
d" ‚eK [ ∏ S
fJ ˜dJc˜
‰ º ˜da [ \ R#∫g}
S Kf\ R#∫g} S ´g\ R#˜da " ô } S Gf∏ S ì ˜
da [ ˜dJc˜
J
f‰ \ R#˝dJc˜
‚e[ ∏ S ¯e˝dJc
˜
HdD Hd˝da [ \ R#*h} S Æf\ R#*h} S _g\ R
#Hd_ ˝da [ ˝dJc˜
¯eH  c ia    W7LhQ "  i#
 ‰ \ R# iJ S qi\ R#ÓeË Hd3 D ÓeHd3   †® \
 R#Ë ÓeHd3 D \ R#∑1Ö " \ R#_9Ç.ª7Ñ ˙ˇº9\
R#é+" Z " ˜
 <è ã \ R#o7üi∏h° ° †c‡iØ  Sc
r # π-Ø  Drv π-é+" Fiô Ñ   Ø  not Ø  upda
tedº º †c/ YhL #7F _,/ ÚhL    1 #7Lh†cPi∏
i° \ R#o7ã Hd# $ ˜
ô+; \ R#ô+"  :@ Ñ   ∏
Q ; d ß ∏ J Ñ   Ä P È
˜
  ã ‰ Ô
7# d Ä \
 " tk DIGITSû7{9A9`k[:\ R# e" ° I   ı9ü:J
 π °  e" ° I   Gf  J π ∏ S  eJc
fã ùe•jÄ
\ R##7  <
7HdG J7I _,\ R#   *-düio7∏hº °
†cØ  replace with: o7 eJc˝k° ° †cØ  >
 search: o7 eJc˝k° F È
†cÛd" P ò-È
†cX #

\ *** Block No. 130, Hexblock 82

  Ûd" I °6ô 1 } Ñ   É Ñ   ˆjÄ B    kÄ B ß
ˇì \ R#?   @I \ ."
ç    ÖâÜäü  ãáàå
  ùë ìî      ˇ."ˆj k k8k8mph-g-göj•jJjUjâ
g_gÇh\h*iÙiËgÙg hˆf gKfÆf[f[f[fZfGf[f´g?g
mgöh_hîf n©  S _$àHπ¨mIˇ E&PÙò ®πSmhπTml
  R# e‰ Ç.  Ä° I   J π \ R# e *° @nº @n°
@nº @n\ R#  I Ñ   é+; ‡jº ° †cÌ)Tj oÄ \ *
 ¨k (LOADR#∏  *∏ ; } ô S D  *D ; Ì)˘'\ *
ço SHOWLOADR#é+ *é+˜ ô+ */ go o; ïo7o\ R#
S@˜
∏ J  e‰ ∏ # J  e‰ a J  e‰ \ R#Z |o  ì
ü:‡jÄ Ôd˜ Ø7∏ Îd‰ °  nó On} Ñ Ïˇ° ° †c_gÄ
 ÔdÂ ° |o\ + µo EDITR#é+; ìi o˛o   ° †c‡i
Ø  Scr π-Ø  Drv π-∏    kq EDIDECODER#  ç@
 Ñ   Ád˜  7d  :@ Ñ < Ád˜ tq∏ Q " [ Ñ   Ä
Q " £ |   È
˜
% J ©dã ‰ # J π ∏ Q ; d ∏ Î
d‰     nó OnÄ \ - Lq EDIEXPECTR# oQ ; ° ∏
 Q "   Ñ   Ø7I7B Óˇ~
ë,\ - ‚q EDIBOARDû7{
9A9XqÓq\ R#ób/ Ç.∏  *; Ä< *Ä<˜ ª=\    q S
AVESYSTEMR#é+ *º é+; ô+ *ô+˜     [t PLAUs
H  Ft PLPUs(  Qt RTIUs@  |t RTSUs`  át SE
CUs8  ít SEDUs¯  ùt SEIUsX  ®t TAXUs™  ≥t
 TAYUs®  æt TSXUs∫  It TXAUsä  Tt TXSUsö
 t TYAUsò  Ít M/CPUR#A!    Î l@ ∏ # "
Ä1 Ñ       s[ Ù /  ˇ1 ásásÑ   ssì ‡+ inva
lidJ  s" ˝r˜
J ˜
   s"    1 Ñ    s"    1

\ *** Block No. 131, Hexblock 83

  ñ   xv LDY u†é   Ev STY uÄå   Rv JSR u
Ä   v JMP u@ÄÑ  åv BIT u Ñ R#∏ =   [ ‡+

out of range \   ôv [[R#X \   Cv ?]R#  X
 # I _v  \   Pv ?[R#  X °   \   Áv ?[[R#Ï
vã \   ˙v ]?R#X Ù J Ñ   ã ; B   Ù # I _vã
 ‰ \    w ][R#X # º ívã X Ù # I _vã ‰ \
 3w ]]R#ív   nx 2INCR#∏ µuÛsF psÅu∏ |uXwÏ
vã #  v w\   ^x 2DECR#∏ µuòtF psOu∏ |uÉwÏ
vã # ˆu w\   áx WINCR#∏  véwÏvã #  v w\
 ∞x WDECR#∏ µuéwÏvÙ # ˆu wˆu\   Mx ;C:R#Ñ
 ÖvÂrï(° ~ ; ° \   gr ASSEMBLERR# bss\
 ye;CODER# !/ ˝ˇ˝ ö#/ ˛ˇ˝  y\    y CODER#
A!X ∏ Ö ;  y\   >y >
L È
˜
B Ô ã D \ 1
  CPUSHR#Ô z
Ù P ≥ Ù # I ∏ B n uzP P \ R#
  r˝ ä:Q;Ì)ë,Ô ˘'/ Æˇ˝ 9 \ 1 Kz RANGER#az
˜ ∏ =z; # ∏ " / \ I Ñ Úˇn az; \ ‡z©ˇçkzçl
z° Ö ± Ö •  I Ö ê Ê ≠;zÖ ≠<zÖ l  ."~z\ •
 I Ö ê Ê ≠kzÁ≠gzP • M?z• Ì@zêV• Mcz• Ìdz
∞Jla[égzékzélz• ç;z• ç<z Ü ez"  d /  0" @
 Ñ    #"  \d " / l@I Ñ   " P ˜
d ©" #Ø  c
an't be DEBUGged3+\ 5 ≤z NESTR#mz" "  \Ä
ezÂ \ 5 ù\ UNNESTR#=zÂ az˜ \ 5 ∏\ ENDLOOP
R#mz" Z ˜
=z; \ 5 O\%UNBUGC 5 Î\ (DEBUGR#
 \∫zez˜ izÂ qz˜  z¯[\ 5  z DEBUGR#;' ]\ 5
  ] TRACE'R#;'∏  ]ó C \ R#π6∏ Z ò-Ø  :\

\ *** Block No. 132, Hexblock 84

  ] RELOCATER#ã ≠ ˜
È
˜
%/˜
a  3[ ‡+ buff
ers?∏ Ë /   ˜
  ‡+ stack?Ù 0 "   @˜
  ‡+
rstack?@2$6|
˜
≠    ˜
; ∏ ,^} # " ≠ # ;
  I ≠    ˜
;  9\
 p^ BYTES.MORER#} ≠ I ˜

B " } I [^\
 }^ BUFFERSR#%/$ a  3B " I
ã I Í^\    _ DEMOSTARTR#\  ALLOT MPTY  Ïˇ
\  TEST T    ∏ Ô Ö ; ã £ |   % ; F ˚ π \
;  `àACTIVATER#° B ©ˇ; L` SLEEPR#  lã ‰ \
 ; Å` WAKER#  ,ã ‰ \ R#q+Ù_Ø
Task error
: L #7 `ô_\ < ñ` TASKR#˝ X   ˇ1   ˛I Ñ
º ˝ } X /   D X   l  } # "   ∏ } # ; P ˝
∏    I ∏     È
˜
  |
˜
X I a ˝ / ®`Ù    ˜

; ∑#\ = J` RENDEZVO  aft 7Ø  bleibt FORT
H ein ADVENTURE! 7\ a ~A >BYTE|B± häë • 8
È Ö ∞ F äë Hl  a RB HBYTER#ZBJ
\ a ñB LBY
TER#ZBÄ \ R#A!° I     J π Î °  I  ®± Å H±
 † ë l  JB  Ø ]   ∫ H   C P   H T   J S
 D L   ∏ \
  ¢ c „ Ç   º w  à   ≥ f X H
ˆ Ç
 ï   ° # _ # ü   í   { Ï   È Ö ∞ F •
'ë •&Å • 8È Ö ∞ F •%ë •$Å l  ] -D DU<R#/
 Ä˜

/  Ä˜


ô \ R#° I . P 3D3D  I ∏ É
 Ñ     ˜
Ô 3 G B   Ô 3 P ˜
J π \ R#P 3D∏
É Ñ   3D  I Ô # B   3D  È
  Ñ   Ä Ô G B
 I Ô # \ R#P ∏ É Ñ   ~
Ô # B   3D/  Ä  SD
ô _ Ô ˜
\ ^ MD SQRTR#° º    ãD
Ä    ãDA

\ *** Block No. 133, Hexblock 85

  D˚D\ _ 'E 100*sE° Ö$± Ö% $&% $&%•$Ö&•%Ö
' &&' &&' &&' •$E&Ö$•%E'Ö% &&' •$E&Å •%E'
ë l  f+F*F)F(f+F*F)F( •$E(Ö$•%E)Ö%° E*Å ±
 E+ë `` jE 100U/NEÜ$Ü(°  Ö%Ö)± *Å Ö*ä*ë Ö
+ òEf+F*F)F( òEl  @ DE GRAPHICv%2Q2Q zø#à
 ø# ‡ø# Xø# Dø# @ø#  ø#  ø#  ø#  1FX©2Ö à
¢ Ñ$Ñ&©XÖ%      U Ñ     B     ss\   ıt AD
C u`N   {u AND u N   àu CMP u@N   ïu EOR
u@N   ¢u LDA u†N   Øu ORA u N   ºu SBC u‡
N   Iu STA uÄL   Vu ASL u

  „u DEC uA
  u INC u·    ˝u LSR ua

   v ROL u!


 v ROR uA

  $v STX uÅ    1v CPX u‡Ü   >v
 CPY u@Ü   kv LDX u¢  \   vw ]]?R#ív w\
 Cw CSø#ê   Sw CCø#∞   ~w 0=ø#P   âw 0<>ø
#   îw 0<ø#    †w 0>=ø#0   ´w VSø#p   ∑w
 VCø#P   Bw NOTR#   n \   Mw BEQR#öwUv\
 ~w BMIR#±wUv\   Ów BNER#éwUv\   ˛w BPLR#
•wUv\    x BCCR#XwUv\    x BVCR#ºwUv\   .
x BCSR#ÉwUv\   >x BVSR#GwUv\    LABELR#X
H!A!˛ ã   Z i Ä º 1 i X Z I Ä Z D Ä ~ " L
    1 ˜
; L ; Î l@ " 1 " Ñ   ^ \   wy LAB
ELR#X `y y\   Ïx ROMR#X    ˜
/  †[ ‡+ not
 here  7psµuº |u\   Dy RAMR#  6psµuº |u\
/ ∞y TOOLSv% ^ ^Fd0 Ûy WCMPR#Ù µu∏ õuã #
µu# Ou\ ."  ."PX."FX."ºX."∞X."_X."òX."≥

\ *** Block No. 134, Hexblock 86

   Ñ " ez˜ Ô az *=z *∏ Ö ∫zqz *º qz[ P
 [P    *Ù  *Ö ∏ mz;  7qz" _,∏ Z ò-" ∏
ò-ë,©" #   J7I ° } _,W-1  *D  *∏  */ (+ *
/ ∫' *o  *} o " SzB  *≥ B ; / ìz„((+3+\ ˙
[©[Ö © Ö ©lÖ l  R#∏ " / L#" @ Ñ    #d / Ø
7" @ Ñ    #J   " ˜
"  \d / #7" @ Ñ    #J
Ù " ˜
"  \  R#∏ "    ò-\ R#∏ J P Ñ-\ R#
$J7I ° } _,\ 6 ˜\ SR#c]ë,`]Z _,∏ L #7∏ J
˜
# L]\ 6 ] NR#c]s]F _,∏ " ©" #a L]\ 6 £
] KR#c]s]a L]\ 7 A] DR#È
ã c]P _,ã ° I
`]# J π Z _,z
#7L]\ 7 S] CR#º W]\ 7  ^ BR
#c]s]∏ " Ù ˜
   ò-a L]\ R#} ∏ # |
" È
I Ñ

Ä B Ïˇ~
; \
 ,   Z   ∫$" x   ™$"312
,6A_  ∏ J º    Ø â -11053U_.Gnameº113Ö_J
# Û N: &_ STOPõ_• 8È Ö ∞ F • Å • ë • 8È Ö
 ∞ F • Å • ë † • ë H• ë † ò E Ö äE Ö l  .
"©,Å lõ_: í_ SINGLETASKR#   / Ç.; \ : Á_
MULTITASKR#|_/ Ç.; \ ;  `ÑPASSR#ã   ,Ù ‰
Ô z
   ˜
P   a " ã a 3   " Ù I  USR#∏ @.Ç
.ç.\ R#1 " Ñ   ^ \ = @Ab'SR#;' #J ˜
yA\ =
 IA TASKSR#Ø  MAIN  7} ∏ # " È
I Ñ 8 ∏
 ˜
"    ˜
Í"©" #∏ J   lI Ñ
 Ø  sleeping
7# " B Dˇ~
\   ~A HELPR#P Wp 7Ø  Probier'
 ruhig weiter! 7 7Ø  Aber ohne Handbuch 7
Ø  und die Hilfe der 7Ø  FORTH-Gesellsch

\ *** Block No. 135, Hexblock 87

  [ H 2 ö   B C  !|!T!*"}"N" #G#Ø#ı#8$X$µ
$Ô$'%[%ç%ª%Á% &5&x&X&ï&Ø&E&Y&Í&¯& ' ' ' '
R#∏   z- Ñ     ¥ã I B\ \ ®B SINR#/ H x ∏
 É Ñ   / H ˜
∏   ¥- Ñ     ¥I óC_ B   óC\
\ ±C COSR#/ H x   z˜
∑C\ \ ÎC TANR#∏ ∑Cã
ÒC} Ñ
   Dã } B   P $ \ ]  D D2*5D©  S
&&'&$&%• 8  ©XÖ'±$ë&HP˘Ê%Ê'JPÚ©6Ö xHl  c
   CLRSCREENR# F/ @ ó \ ."TG." ò."ˆò."(ò.
"P ." ò© ç P© ç P≠ÄFç P`©;ç P© ç P≠ÑFç P`
≠àF8È0jjj8È EV∞ `™Ë† ÑLÑM•N_SëQ† ÑO  ˇ†
±QÖNÑL`≠ç ) P l1Í üˇ≠ç P¯LXF≠ Pç P¢ JP˝•$
≠åFi çåF™P  üFl G éFΩàFç P ∞F≠
|) P∫lÅÍX©
Fç  ©‡ç  ©ç P©Åç
|x
   û(4112)    Íl
 aÍl9al Z J6 W Z{GU    VcıC®3YH




                                J ~F‰ / P
 ÜF; ÅG\ h  H NOGRAPHICR#£%@G   /  P‰
/  P‰ Z  F‰ ~FJ /  P‰ /  }J P   /  }‰ \ i
 ∑G BLKø#  i ôH WHTø#  i •H REDø#  i ±H C
YNø#  i ΩH PURø#  i IH GRNø#  i UH BLUø#
 i ·H YELø#  ii ÌH ORAø#  i ˙H BRNø#  i
I LREø#  i  I GR1ø#  i  I GR2ø#  i *I LG

\ *** Block No. 136, Hexblock 88

                                   .* ult
raFORTH-83 3.80-C16+ çˇˇ± Ö ≠ˇˇÖ  • I Ö ∞
 LˇˇÊ ∞˘     END-TRACEE ©•Ö © Ö ©IÖ © Ö l
    w  RECOVER.*HÖ HÖ • P F F lR+  Z  NOO
P    ô  ORIGINø+    _  S0<,   ≥  R0<,   Ω
  DP<,   G  OFFSET<,   Q  BASE<,     OUT
PUT<,   Î   © Ö9  8&9 8&9HE8Ö8ê Ê9 8&9 8&
9 8&9•$)  8Ö8 •&)¯E8Ö8•'E9Ö9©‡ 9Ö9•&) ™ΩÍ
I†  `l
J PLOTèJ  J¶9‡‡ê  8ë8Ê xln9l ÜJ F
LIP≠J  J¶9‡‡ê q8ë8lúJl _J UNPLOTJJ  J¶9‡‡
ê iˇ18ë8lúJm øJ LINEÁJ©  S ¢ µ$ï`J ˘©G8Â(
Ö(©G8Â$Ö$Ñ5Ñ4Ñ2àÑ7Ñ6Ñ3à•*E&•+Â'∞ 8•&Â*Ö0•
'Â+Ö1Ñ5l9K•*Â&Ö0•+Â'   INPUT<,   ˘  ERROR
HANDLER<,      VOC-LINK<,      UDP<,   *
 SP@= • Ö$• Ö%¢$• 8È Ö ∞ F µ ë µ ¢ l    5
  SP!G ° ™± Ö Ü ¢ l    _  UP@ ¢ lg   W É
UP!é ¢ ± ï à± ï ¢ † •  I Ö ê Ê l    Ü  RP
@µ ¢ lg   ≠ ÉRP!D ¢ lê   º Ç>RR • 8È Ö ∞
F ° Å ± ë lù   K ÇR>Ò • 8È Ö ∞    / ÊK‰ \
 p  L SPRBUFø# Hø# Pø# Pp aL SPRITEø# Pø#
 Pp xL 3COLOREDø# Pø# Pø#%Pø#'P."      @Ä
íL©  S à¶(ΩàL¶$P iˇ1&l™L &ë&ln9q KL SETR#
ì êL\ q ±L RESETR#° êL\ q AL GETFORMR#  @
$ jL˜
  @D \ R#/ ¯ ˜
˜
‰ \ q SL FORMSPRIT
ER#P jL/  ?1   @h ˜
∏    FÔLÔ  FÔL\ r ˇL

\ *** Block No. 137, Hexblock 89

   F ° Å ± ë ©  E Ö ê Ê l    Í  R@  • 8È
Ö ∞ F ± ë ° l      ÖRDROP    1  EXITf ° Ö
 ± Ö l    =  UNNEST^ ° Ö ± Ö l    s  ?EXI
TU °    •  I Ö ê Ê (Pæl    K  EXECUTEô °
Ö ± Ö •  I Ö ê Ê l    ç  PERFORMR+" ó \
 ±  C@L ° Ö$± Ö%© ë °$l    E  C!Ê ° Ö$± Ö
%H± Å$à•  "  XMOVER#È
3 nL˜
‰ rL
  ˇ- êL
\ r 1M YMOVER#3 # nL˜
‰ \ r tM MOVER#|
9M
\M\ r LM SPRPOSR#∏ P 3 # nL˜
J   3 nL˜
J
Ô ÜL˜
J rLJ 1 Ñ   /   ˜
\ s M HIGHR#EL∑L
\ s æM LOWR#ELIL\ s OM WIDER#ZL∑L\ s M S
LIMR#ZLIL\ s M BIGR#∏ EMÊM\ s  N SMALLR#
∏ UM˜M\ s  N BEHINDR  I Ö ê Ê l      CTO
GGLER+| J n ã ‰ \      @$ ° Ö$± Ö%±$ë °$l
       != ° Ö$± Ö%H± Å$H± † ë$lÙ   7  +!]
 ° Ö$± Ö%H±  A$Å$H± † Q$lo   v  DROPù   Y
  SWAPç ± ™† ± Ö$äë •$† ë H¢ ± Ö$° ë à•$l
    Ñ  DUP∫ • 8È Ö ∞ F † ± † ë H± àl    ≤
  ?DUP °   P l  l∫   V  OVERˆ  `bGX©Íç
©1ç  ©ç P©Åç
|xl  ©Fç  ©ıç  ©ç
|©Òç Pé|
FéåFl∞Fg ^F TEXTÉG© ç|F )G éFl  g ZG HIRE
SùG© ç|F )G üFl  ≠GX [Gxln9g ìG WINDOWR#
  $   0˜
ÜF‰ ´G\ Æ|FP
Ëé|F )G éFl1Í‡ P
Ëé
|F )G üFl1Í¢ é|F [Gl1Íh ˘E GRAPHICR# F/F/
  }J   ¸1  F  /  }‰  FûB F‰  FJ ÇF‰ /  P

\ *** Block No. 138, Hexblock 8a


 • 8È Ö ∞ F † ± Å H± † ë l    Ì  ROT  †
 ± Ö%† ± † ë † ± Ö$•%ë † •$ë H± Ö%° ë † ±
 Å •%ë † l       -ROTR+    \   s  NIPR+ã
Ä \   D  UNDERR+ã Ù \   T  PICKR+# 3 ; ˜
" \   Ü  ROLLR+∏ P ç ; ∏ a Ô # 3 Ú Ä \
ù  2SWAPR+  P   Ô \   @  2DROPÙ   V  2DUP
R+Ù Ù \     Rø#
 i 6I LBLø#  i bI GR3ø#
i nI BORDERR#∏ ~F‰ /  P‰ \ i zI SCREENR#/
 !P‰ \ i UI COLORSR#Ù ÇF‰    $    F/ ¯

® \ i äI BACKGROUNDR# FJ    h ìI\ i ∞I PE
NCOLORR# FJ    1 ã ìI\ Ä@      R#Ù `y˜
\
j NI POINTYø#` j ˛I POINTXø#B ©  S ¢ µ$ï`
J ˘©G8Â$Ö$XF •$)¯hÖ8  ‚  +˘  H° Q ë à± †
Q ë lô   Û  OR  H°   ë à± †   ë lô      A
ND3 H° 1 ë à± † 1 ë lô   +  XORp H° q ë à
± † q ë lô ! h  -K H± 8· ë H± † Ò † ë lô
! E  NOTã  ä· Å äÒ ë l  ! É  NEGATE¶ 8∞„"
 õ  DNEGATE∑ H8äÒ ë HäÒ ë ä· Å † äÒ ë l
 ™®à± ô$ à ¯ä E Ö ê Ê ¢ † `" ´  Ö1•(E$∞ 8
•$Â(Ö.Ñ4loKÂ$Ö.•1P •0E.∞ ¶.Ö.Ü0•5Ö6•4Ö7HÑ
5Ñ4•1jÖ-•0JÖ,8∞D•5 0 Ê&P Ê'lêK•&P F'F& •
$E7Ö$ •,E.Ö,ê Ê-Ê2P Ê3•0E,•1Â-∞+8•,Â0Ö,•-
Â1Ö-•6 0 Ê&P Ê'lVK•&P F'F& •$E4Ö$ /J¶9‡‡
ê  8ë8Ê x•0E2•1Â3∞Çln9o ~J DRAWTO L† π` ô
( à ˜©  S lÏJo ¸K FLIPLINER#  q/ ÊK‰ ÂJ

\ *** Block No. 139, Hexblock 8b

    D+˜ ©  S H±  E&ë H± E'ë ° E$Å † ± E%ë
 l  #   1+% ©  A ∞ l  Å ± I ë l  #    2+
c © P‡# <  3+p © PSv © PM\ © PG# i  1-I 8
° È ê l  Å ± È ë l  # B  2-á  ê‡$ Ä  TRUE
ø+ˇˇ$ å  FALSEø+  $ ô "-1ì $ ß !0° $ ∞  1
ø+  $ ∏  2ø+  $ B  3ø+  $ L  4ø+  $ V  ON
R+ì ã ; \   $ ‡  OFFR+° ã ; \ % Ò ÑCLIT
• 8È Ö ∞ F ° Å äë Ê P Ê l  %   ÉLIT1 • 8È
 Ö ∞ F ± ë ° Å •  I Ö ê Ê l  % ) GLITERAL
R+∏ /  ˇ1 Ñ   6!/  !d 6!   !\ & t  0<Ö ±
  ©ˇ$äë l  & ~  0=õ °   ËPÈ& î  UWITHIN±
 ©  S † ° E$± Â%∞ ° E&± Â'∞Ωlå ' •  <W °
Ö$± Ö%•  I Ö ê Ê •%q   0ï° E$± Â%lá ' Q
U<  ° Ö$± Ö%•  I Ö ê Ê ° E$± Â%∞ lâ lå (
˝  >R+ã U \ ( )  0>R+_ É \ ( 7  0<>R+ô â
\ ( f  U>R+ã   \ ( v  =R+I ô \ ( E  D0=R+
  ô \ ( S  D=R+µ ı Y \ ( É  D<R+  È I Ñ
 - J J B   ~   \ R+9 Ñ   ã Ä \ ) î  MINR+
È - µ \ ) E  MAXR+È U µ \ ) W   IBR+\!" \
 ; W! QUERYR+}!  pW?Q!" o!; ∏!˜ D!˜ \ < Á
! SCANR+P ∏ Ñ   Ù J   I Ñ   G ã # ã B ‰ˇ9
 \ <  " SKIPR+P ∏ Ñ   Ù J   I Ñ   G ã # ã
 B ‰ˇ9 \ < ;" /STRINGR+Ù     Ù ˜ z I \ Ia
ê I{∞ IBê )I[∞  Ä`= J" CAPITAL©"°  Ü"Å l
  > ù" CAPITALIZED"° Ö$± Ö%°$Ö&àD&P † l

\ *** Block No. 140, Hexblock 8c

   \ : Q  PADR+X   b˜ \ : ‚  ALLOTR+L [ \
 : ı  ,R+X ; F ˝ \ :  ! C,R+X ‰ º ˝ \ :
!áCOMPILER+Ô ∏ a P "  !\ ; ,! #TIB.*  ; h
! >TIB.*`!savesystem c16neu

        ; u! >IN.*  ; ≤! BLK.*  ; æ! SPAN
.*  ; J! T  d B ‡ˇJ \ R+L    1 ˜ \ n °* N
AME>R+∏ S*ã J    1 Ñ   " \ n ‚* >BODYR+a
\ n  + .NAMER+} Ñ   ∏ î(Ñ   Ø$ |L    1 #?
B   Ø$ ???ë4\ o  + CREATE:R+A)ï'¥," D,; ï
0° \ o h+ :R+r+Î(• 8È Ö ∞ F • Å • ë •  I
Ö äE Ö l  o H+A;R+° ˝ 6!\ 0¥'\ o ñ+ CONS
TANTR+A) !Î(• 8È Ö ∞  ÏÊ)Ê'lL 7 º  CMOVE>
Ù ©  S  •%E'Ö' •%E)Ö)Ê%_$ ê à±(ë&òP¯F'F)F
%P† l  7 È  MOVER+P È   Ñ   Ô Ú d Ô D \
8 %  PLACER+Ù P   Ù # Ô , ‰ \ 8 f  COUNTN
 ° Ö$ I Å ± Ö%I ë • 8È Ö ∞ F lT 8 D  ERAS
ER+° ® \ 9 è  FILL™ ©  S à•$¶' ë(HP˚Ê)JP
ˆ¶& ë(HJP˙† l  : °  HERER+L "  ¥," "  !
$J ∏ º    Ø â ‡3 invalid nameX ~'; # ˝ à)
^)" Ñ   º ^)[ ∏  ) !û(   Z' )L ; B   &)Ä
¥'°  !Î(l(3*° Ö(± Ö)•  I Ö ê Ê ° Ö&± Ö'
&P lå ±&ë Ö%°&Å Ö$ %P l  •$ I Ö$ê Ê%°$h8)
 E$Ö$ê Ê%H)  ±$h°$Ö$HÖ%•$E(P∞•%E)P™lc n
∏) >NAMER+% " ∏ Ñ   È Z I ã 1*} Ñ   z ~

\ *** Block No. 141, Hexblock 8d

  +∏ P = Ù É Ñ   | ˜ ã G   É Ñ   _ Ù Ñ
ã   ˜ ã G 9 \ 5 X  2/  ±  ± Jë ° Jl  6
 /MODR+P   Ô ‡ \ 6 /  /R+6 J \ 6 d  MODR+
6 Ä \ 6 r  */MODR+P  Ô ‡ \ 6 B  */R+J J
\ 6 X  U/MODR+° ã G \ 6 á  UD/MODR+P °
G Ô ã P G Ô \ 7 õ  CMOVEF ©  S àD$P F%  †
 l  ±(ë&HP  ë • Å  ° I Ö äQ Ö ©  E häE l
 R+Ô ~'" Í*; \ j å(EDOES>R+6!Î(  l !6!@(\
 R+∏   ˇ1   ˇI I \ R+X   ˇ1   ˇI Ñ   ∏ ∏
# X Ù I # Ú º ~'[ º ˝ \ k ˚( ?HEAD.*  k v
) |R+^)" S ì ^); \ l D) WARNING.*  R+Ñ)"
S ~'" ¥," 9.J Ñ   ë4~'"  +Ø$ exists π>\ l
 Z) CREATER+X D!"  !  %Å l  3 ò  M*R+∏ É
∏ P Ñ   _ ã ∏ É Ñ   _ Ô â P û Ô Ñ   µ \ 3
 Î  *R+û Ä \ 3    2*5 °  Å ± *ë l  R+ì ‡3
 division overflow\ 4 .  UM/MODI ° Ö)± Ö(
•  I Ö ê Ê ° Ö%± Ö$H± Ö'H± Ö&¢  F*8•%Â)®•
$Â(∞ &*ê Ö$Ñ%&'&&&%&$JP† F$F%ê  Ü b \ •&
ë H•%ë H•$ë † •'l  5 ^  M/MODR  RSIVER+¥'
\ R+Ç'Ñ   | J   Ù ‰ Ä \ h H' IMMEDIATER+
 @Z'\ h Ú' RESTRICTR+  ÄZ'\ i  ( CLEARSTA
CK.(† ± Ö H± Ö † l  i  ( HALLOTR+∏ " Ù I
ã ; a ∏   I ∏ ∏ ; F ç Ù I , ,(∏ ; \ i @(
HEAPR+∏ " z \ i Y( HEAP?R+Ä(} Ø \ R+∏ X Ù
 I ∏ i(Ä(ã D Ä(Ù I ~'[ ¥'\ • 8È Ö ∞ F •

\ *** Block No. 142, Hexblock 8e

  B Óˇ\ 0 ≥ FREPEATR+F ˝ 6!B Q \ 0 Ì EUNT
ILR+F ˝ 6!Ñ Q \ 1   BDOR+6!I ¶ P \ 1   C?
DOR+6!| ¶ P \ 1 1 DLOOPR+P ˝ 6!J 6!π Ω \
1 e E+LOOPR+P ˝ 6!˚ 6!π Ω \ 1 ` ÖLEAVER+π
 Ô Ö ∏ " ˜ P \ 2 |  UM*† ± Ö$° Ö%HÜ&Ü'¢
'&&&%&$ê  ± E'Ö'H± àE&Ö&ê Ê%P Ê$JP|•'ë H•
&ë † •$ë •  2∏ L º&;  &Â ° P ° °   Ì% &˜%
  -@ Ñ   9 ì P Ì% &˜%o&Ñ   Ê ; Ì% &˜%J%ô
 &≠%®&Ì%)&˜%J%ô Ñ ˇ &î&ô  & &˜ Ì%)&˜%B T
ˇ\ g B& 'NUMBER?I0L&g e' NUMBERR+p'} ô ‡3
 ?É Ñ     \ h v' LAST.*  R+~'" } \ h W' H
IDER+Ç'Ñ   Ö " ¥," ; \ h é' REVEALR+Ç'Ñ
 Ö ¥," ; \ h ´'IRECU  ER+X I  !\ . }  ?PA
IRSR+I ‡3 unstructured\ I   h©ˇl  h© ™• 8
È Ö ∞ F ä† ë H¢ l  / Ù  CASE?b ©  S •$A P
 •%Q P lâ äl  0 8 BIFR+6!Ñ ¶ º \ 0 \ DTHE
NR+= º ˝ Ω \ 0 O DELSER+º ˝ 6!B ¶ ã Ω ì \
 0 Ñ EBEGINR+U F \ 0 ° EWHILER+F ˝ F 6!Ñ
¶ / ˛ˇH \ R+Ë ∏ / ˛ˇI Ñ   Ä Ω   VERTR+# L
 J%Ñ   ≠%B ÙˇG \ R+º&" ô \ R+L ì º&[ \ R+
G L \ f M% DPL.*ˇˇR+Ñ   9 ~ Ä 9 ° \ R+Ñ
 9 Ä Ô Ñ   µ   Ä  &" # } S Ä ì \ R+  &@ Ñ
      ì d   $@ Ñ      ì d   H@ Ñ      ì d
   %@ Ñ   F ì d ° \ R+  ,Ù I ã   .I   \ R
+ &" ì I S º  &[ \ .*  g
& NUMBER?R+Ê

\ *** Block No. 143, Hexblock 8f

  Ö † l  + B Ü(+LOOP˝  ° A Å ± Q ë Jq  •
 I Ö ê Ê ( Dl  , Ú ÅI' † • 8È Ö ∞ F  ± HH
Q Å à± HHQ † ë l  , ! ÅJs † PR- m ÜBRANCH
D  • A Ö$• Q Ö •$Ö l  - y á?BRANCHÜ °
•  I Ö ê Ê (Kld . Z  >MARKR+X °  !\ . û
 >RESOLVER+X Ù I ã ; \ . ≤  <MARKR+X \ .
M  <RESOLV  Í#~ \ d L$b.(R+  )Í##?\ d }$a
\R+∏!"  6h #  6$ ∏!; \ d Ô$b\\R+¯c∏!; \ d
  % \NEEDSR+ $ı.J Ñ   Û$\ d  % HEXR+   Ê
; \ d 7% DECIMALR+   Ê ; \ e j% DIGIT?R+
 0I ∏    [ Ñ     áI ∏    [ Ñ   Ê " Ù [ }
S Ä ° \ e A% ACCUMULATER+ã P ã Ê " û Ä
Ê " û ı Ô \ e †% CON
 UMAXR+È   µ \ ) È
 UMINR+È [ µ \ ) ¸  EXTENDR+∏ É \ )    DA
BSR+  Ñ   µ \ ) "  ABSR+  Ñ   _ \ R+9 Ô a
 ∏ P   P ã P P \ * 7 É(DOR+Ù I i \ * C Ñ(
?DOR+Ù I } Ñ   i Ô ∏ " ˜ P Ä \ * U  BOUND
SR+Ù ˜ ã \ * ö áENDLOOPª © l  + Ø Ö(LOOPL
  © A Å ê ± I ë ê l  † ± Ö à±   DR+µ#Â"\
b Q# PARSER+P µ#∏!" T"Ù ã Ô  "P Ù I ∏ Ô l
 I ∏![ \ b ‚# NAMER+?4X#B"d \ c  $ STATE.
*  c )$eASCIIR+?4X## J 1$" Ñ   ^ \ c 7$ ,
"R+  "Í#X Ù # ˝ n \ c w$Ñ"LITR+Ô Ô | L ˜
P P \ c Q$Ç("R+X$\ c å$A"R+6!ë$\$\ d ô$É(
."R+X$L #?\ d ©$B."R+6!Ø$\$\ d ª$a(R+  )

\ *** Block No. 144, Hexblock 90

   H±$ Ü"ë$lQ"Á"Ü*† ± ô$ à ¯†  ≠∫!E&Ö&≠ª!
E'Ö'8•$Ì∫!Ö$•%Ìª!Ö%∞
° ç∫!± çª!lY#† •$ %
=°&Q P Ê&P Ê'•$P F%F$l$#•&Ö(•'Ö)°&Q  Ê&P
Ê'•$P F%F$( Ê*•$ %P‚8† •&Ò ç∫!H•'Ò çª! ©
 E Ö ê Ê † ± Å Ö$H± † ë Ö%à•*ë$±(HF* ˜© ë
$† l  b µ" SOURCER+D!" } Ñ   ∑9¯cd }!o!"
\ b ¨# WOR   F † ± Å H± † ë l  o ¨+ VARIA
BLER+A)F ˝ \ p |+ UALLOTR+∏ 0 " ˜   ˇ[ ‡3

Userarea full0 " ã 0 [ \ p Û+ USERR+A)F
¸+ !Î(• 8È Ö ∞ F † ±  E Å äHE † ë l  p ),
 ALIASR+A)~'" ∏ J    1 Ñ   / ˛ˇ˝ B      Z
'S*; \ q ], VP.*  |-K-K-ajaj    q ë, CURR
ENT.*K-q ™, CONTEXTR  +Ë Ö \ B õ4 HOLDR+ì
 ¥4[ ¥4" ‰ \ B æ4 <#R+¥4¥4; \ B W4 #>R+~
¥4" ¥4Ù I \ B Ë4 SIGNR+É Ñ     -E4\ B ˇ4
#R+Ê " _      Ù U Ñ     á˜   0˜ E4\ B  5
#SR+ 5È Y Ñ ¯ˇ\ C @5 D.RR+z | ) |4e5   5Ì
4  Ù } Ù I _4#?\ C u5 .RR+ã     [5\ C 5
U.RR+° ã [5\ C í5 D.R+° [5ë4\   C _5 .R+
 ©5\ C µ5 U.R+° ©5\ D C5 .SR+; ∏ " Ù I
   £ |   % " H5F ˚ π \ D R5 C/Lø+) D ˛5 L
/Sø+  D  6 LISTR+é3; Ø$ Scr é3" ∏  dx H5Ø
$ Dr `dπ5 6° I . °>Ñ   Ñ  ?% F Ñ5ë4é3" ∑9
%  6$ ˜  6G q4#?J π  ?\ E  6 PAUSE  E Z6
LOCKR+∏ " } I Ñ   Ä d ∏ " Ñ   Ç6B Ùˇ} ã

\ *** Block No. 145, Hexblock 91

  4π>D!" } Ñ   é3; ∏!" ô3; 33\ _ ü3á(ABOR
T"R+X$ã Ñ   P ,(Ô   ª d Ä \ R+X$ã Ñ     ª
 d Ä \ _ V3FABORT"R+6!‡3\$\ _  4FERROR"R+
6!˙3\$\ ` %4 BLø+  ` :4 -TRAILINGs4ò S °
Ö& ± E%Ö'_$ ê à±&I  HP Ê%òh•%l  òPÍF'F%
‰òl  A e4 SPACER+?4
?\ A â4 SPACESR+° |
 ë4J π \ R  ; \ E Ü6 UNLOCKR+∏ ç6˜ \ H8È
Ö HÈ Ö ©lÅ † ± Ö H± Ö † ° Ö ± Ö •  I Ö ê
Ê ¢ lê F ∑6 FILE<, F  7 PREV.*¯{.*  F  7
B/BUFø+  † ±(Y" P H@ PÙ`:7† ± ô$ à ¯†  ±
E&Ö&H± E'Ö'≠ 7Ö(≠ 7Ö) )7P •  I Ö ê Ê † ©
 E(Å •)I ë lf •(Ö*•)Ö+°*Ö(† ±*Ö) (P l   )
7P‚°(Å*† ±(ë*≠ 7Å(≠   " ≥ a I   \ ] ¨2 DE
PTHR+; ∏ " ã I   \ R+1$" Ñ   Ø$  compilin
gd Ø$  ok\ ^ G2 (QUITR+Ì1 ?Ô!˘/2B Ùˇ\ ^
 3 'QUITI0 3^  3 QUITR+B " B 0&3\ ^ ,3 S
TANDARDI/OR+/ * Ù Z D \ ^ c3 'ABORTI0† ^
A3 ABORTR+,(C J3q333\ _ P3 SCR.*  _ à3 R#
.*  _ î3 (ERRORR+q3ë4X  +L #?ë  7ë(•(ç 7•
)ç 7lC7J  7 (DISKERRR+Ø$ error !  r to re
try Ø?∏   rI ã   RI   â ‡3 aborted\ J A7
DISKERRI0L7J  8 R/WI00LR+∏ z " É Ñ j a ∏
" # Ñ 3    2Ù  2q3∏ z Ù a " F ç " °  8Ñ
 Ø$ write  8B {ˇ  ÄÙ t #   Ä \ R+a ∏ Â t
˜ \ R+∏ {8   2Ù  2q3P Ù Z " ˜ Ù      ˜ z

\ *** Block No. 146, Hexblock 92

   ± E H± Â † ê l   Ü ì ‡3 stack empty\ \
 ï1 .STATUSI0Ct.*Ô Ô ; \ \ „1ÑPUSHR+Ô ã ∏
 P " P Ò1P P \ \ ˝1 LOADR+} ô S D! 2D!; ∏
! 2∏!˜ Ì1˘/\ ]  2 +LOADR+D!" ˜ #2\ ] c2 T
HRUR+# ã I   % #2J π \ ] y2 +THRUR+# ã I
  % k2J π \ ] V2c-->R+º D![ ∏!˜ Ì1\ ] î2
RDEPTHR+B    º  8Ñ   Ø$ read  8B YˇÔ \ R+
 7∏ " Ñ   " ∏ a " ì I Ñ Íˇ 7ç6∏ !8\ R+a P
 È   ; Z " ˜   a ; Ô t ˜  7@6\ R+ 7" ∏ Ñ
  ∏ z " É Ñ Óˇ\ R+ 7" ∏ " ô Ñ ˆˇz " É \ L
  8 CORE?R+87~ ° \ M f9 (BUFFERR+87H8Ó8B
¯ˇ\ M z9 (BLOCKR+87H8â8Ó8B ˆˇ\ è9† ± hH±
l  M T9 BUFFERR+ç9D9  R+A)/ •0 !Î(† ± hH±
 Ö HÖ † l  z µ0 (ISR+Ô ∏ a P " ; \ R+" /
I/" Ù I ã / ∏/" I   â ‡3 not deferred\ z
}0bISR+;/∏ Û0 +1$" Ñ   6!„0 !d ; \ .* R+O
2   - ‡3 tight stacke1J ô Ñ   ì e1‰ ì ‡3
dictionary fullØ$ still full \ [ "1 ?STAC
K†1† 8• Ò H• Ò P †  Ü h1\ ég1†  \ M ú9 BL
OCKR+ç9}9\ N Ø9 UPDATER+  Ä 7" z # ‰ \ N
A9 SAVE-BUFFERSR+ 7ç6 9} Ñ   !8B Ùˇ 7@6\
N }9
EMPTY-BUFFERSR+ 7ç6 7" } Ñ   ∏ {8B Ú
ˇ 7@6\ N  : FLUSHR+Ï9 :\ R+∏ ç9n9Ñ    7"
{8,9Ñ   Ï9Z " ˜ ã ∑9Ö Ö ; J9\ R+Ï9P Ù   ˜
 Ù [ P È   Ô 1 Ñ $     ı Ô ° |   ì / ˛ˇı

\ *** Block No. 147, Hexblock 93

  / NOTFOUNDI0/x æ/
NO.EXTENSIONSR+˙3 Ha
eh?\ x O/ INTERPRETR+∏/\  R+û1 $ı.} Ñ   º
 1 Ñ   ó ∏/‡3 compile onlyÉ/S p'ô Ñ   I/∏
/\ R+û1 $ı.} Ñ   < Ñ   ó ∏/ !∏/É/S p'} Ñ
  < Ñ   ã ^ ^ B   I/∏/\ y Ì/a[R+/  0„0∫/1
$˜ \ y {0 ]R+/ ;0„0∫/1$Â \ R+ì ‡3 Crash\
z ë0 DEFER   È h:J π B   Ô ° |   È h:º º
ı J π Ï9~ \ O 8: COPYR+º V:\ O T: CONVEYR
+ã # F ç I ∏ < â ‡3 neinV:\ P Â: LIMITø+
ÄP
; FIRST.*¯{P  ; ALLOTBUFFERR+#;" B "
I %7a   S %7_ #;[ #;" ∏ {8 7" Ù ;  7; \ P
 ); FREEBUFFERR+#;"  ;%7I   Ñ . #;" !8 7∏
 " #;" I Ñ   " B Óˇ#  $ë • 8È Ö ∞ F •&  H
 0 òiˇ®HòÅ   ©ˇ$ä† ë l  v 1. FINDR+D,∏ "
Ù Ö " I Ñ   Ö | " 9.Ñ   J î.d Ù ñ,a [ Ñ
 ã Ö B ~ˇJ ° \ v Ó. 'R+ $ı.ô ‡3 Haeh?\ v
7/I[COMPILE]R+;/ !\ v o/C[']R+;/^ \ v E/
NULLSTRING?R+∏ J ô ∏ Ñ   J \ H ± I Ö H± I
 Ö † l  w U/ >INTERPRETï/ 0x ´  ;" " ã ;
%7#;[ \ P K; ALL-BUFFERSR+#;" 7;#;" I Ñ Ú
ˇ\ R+J    1 \ R+Í*| #   ã î(  \ R+Ä(% " P
 Ô " } Ñ ^ ∏ P Z I P Ô " P Ù G ∏     ã
a Í*  1 Ñ .   î(Ñ Zˇ  a |;Ñ   Ù   a Á;Ñ
   a S*a  B ∂ˇ9 B úˇ\ M<† ± ô$ à ¯†  ± I
 Ö*H± I Ö+† °$Ö,±$Ö- ,5•,E*•-Â+ê •,E&•-

\ *** Block No. 148, Hexblock 94


" ¥,; \ R+" Ö ©* +\ s ±- ORDERR+R,I   %
 K-/ ˛ˇ˚ π F _4¥,K-\ s Y- WORDSR+D," " ∏
°>ô 1 Ñ   π>∏ a  +ë4B ÊˇÄ \ t  . (FIND;.†
 ± ô$ à ¯°&) Ö(† ±$™H±$Ö%Ü$ $P † ¢ lå H±$
) E(P‡ © E$Ö)© E%Ö*_(±&Q)PKàP˜† •*ë à•)ë
à¢ lâ ñ.° Ö$± Ö%°$Ö&) 8E$Ö$ê Ê%•&) P •$Å
•%lB.°$Å ±  Â'l≤<•(E,•)Â-∞ °,Å$±,ë$lG<•,Ö
$•-Ö%lâ<lù R+% " } Ñ   ∏ P Z I K<Ô B Íˇ\
R+} # ∏ " } I Ñ ( È " ã X Ø Ñ   ∏ " # " Ù
 ; G B   " B Nˇ~ \ R+% K<R,I   È % " z Ø
Ñ   / K-% ; / ˛ˇ˚ π È ¥," z Ø Ñ   / K-¥,;
 \ S ∫;
CUSTOM-REMOVEI0† R+Ù Î<'=M<{=Ä(ã
I i(L ; ° ~'; \ T K=  +ñ,∏ " ˜ a \ R+ñ,a
D,\ q ∫, ALSOR+ñ,"    - ˙3 Vocabulary sta
ck fullD," F ñ,[ D,; \ q ~, TOSSR+/ ˛ˇñ,[
 \ r  - VOCABULARYR+A)°  !°  !X % "  !% ;
 Î(l@(D,; \ r /- FORTHv-,U,U  r A- ONLYÑ-
!@!@O-Î(l@(° ñ,; D,; Â,\ r S- ONLYFORTHR+
Z-I-Â,ø-\ s ó- DEFINITIONSR+D,  moÓk TAPE
INIT° 0dÓk FLOPPY ?Ø$ Type 'help' to get
help ?Ø$ Type '64kb' to use 64kb\ R+/  ê/
  î/  @çkI-ë$ CODE   P 3kI-ë$ EDITOR   P
3kI-ë$ DEBUG  /P 3kI-ë$ HELP   º 3kI-ë$ T
APEINIT   Z 3k/ † „0O@/ Xl„0ò@Ók EDITORÓk
 EDIBOARDº é3; ° ô3; e>/  Z/ {/  Äçk\

\ *** Block No. 149, Hexblock 95

   ???\ R+∏  ;I F ç ≠    ˜ " I 1 P ç ≠
 ˜ " I 1 Ñ   Ä Ä Ä d /  ; +; ≠    ˜ ; ∏
  ˜ ≠ # ; ≠    ˜ ;  a\ R+X$B"ı.Ñ   ó B
L #?Ø$  unsatisfied33\ R+6!Ók\$\   3j 64K
BR+/ 3 " /  ˝I } Ñ    ?H5Ø$ too smalld  ;
/  ˝I Ñ   /  Ä/  Ñ/  ˝çk\   #l C16DEMOR+
?Ø$ c16-De  ~j SPø+  | âj IPø+  | îj Nø+$
 | üj PUTAø+  | ©j Wø+  | ∂j SETUPø+S | @
j NEXTø+  | Nj XYNEXTø+ha| {j POPTWOø+Ù
| Íj POPø+ù R+ ?B"∏ ı.J | ô Ñ   Ø$ not Ø$
 found: L #?\ .*  R+F ç  kÑ   Ä Ä Ä d ∏ /
k" I Ñ   @:Ø$  Insert #∏ π5Ø?Ä ∏ /k; Ä Ø$
  scr#∏ π5 ?#2 kô ˙3  ≠ ˇ  ç ˇç?ˇx`ì +i C
64INIT¶i `ilha          ÖÜáâäãåàî öi C64F
KEYSMi¢ J0 Ω¨iù] lOilha{ @ic(64R+ $L ô ‡3
 C) missing" / C)I Ñ „ˇ\ { ‡ibC)R+\ {  jc
(16R+\ {  j FORTH-83R+\ | "j ASSEMBLERv-
D DÄ-|    PUSHAø+  | ij PUSH0Aø+  | wj PU
SHø+  | Fj RPø+  | Sj UPø+  |   INDEXR+#
ã I !  ?% F Ñ5% ∑9#   %#?°>Ñ   Ñ J π \ è
äh FINDEXR+CgÑ   ~ d # ã I 1  ?% F Ñ5Ë ∏
% 3 3 Óf&gP #   %#?Ô °>  Ñ   Ñ J π Âg\ ê
øh INK-POT.*ˆˆ  Óˆ  ˆˆ  í  i INIT-SYSTEM;
i¢˜ölhaç>ˇçviéxi©ih©\h∫Ω  h© ¢ l≥¸ç?ˇ@Xç>
ˇ©içˇˇ©aç˛ˇ Ñˇ äˇ≠ iç ˇ≠ iç ˇ≠ iç@ ©Äç@

\ *** Block No. 150, Hexblock 96

  g˝eód d   zeë$ u2:13,0,L ˝e¯f˝eódÇ6yf\
é ]g DISKOPENR+ d
Pe  #·eódyf\ é ∏g DIS
KCLOSER+ d
àeód\ é Yg 1541R/WR+ã ‡3 no
filez  d6 ∏  d; P [ Ñ   π5Ø$ beyond capac
ityJ d CgÑ   Ä J d ° ã 3 3 Z £ I . Ä È %
  Ñ   Óf&gB   ÓfKgP  g˜ Ô ∏ Ñ   Ñ J π z ~
 Âg\ è Ùg   v R+∏ áf  Ñ      6 d P ˜ ∏ ãf
  Ñ   áfI    6    ˜ d ∏ èf  Ñ   ãfI    6
   ˜ d èfI    6    ˜ \ R+ìf# ã \ R+Ê  2t%
° |4e5Ä   ,E4e5Ì4\ ø+  ç of READSECTORR+
d   zeë$ u1:13,0,L ˝e¯f˝eódÇ6yfS  d
Ne
g7fód° \ ç  g WRITESECTORR+   d   zeë$ b-
p:13,0L ˝eód d
ze   \ óeÜö©  S •& Ld•&ç
>ˇ ¥ˇç?ˇ•$ `Ö≠ç>ˇ ñˇç?ˇ¶&Üòlhaä }e BUSINR
+àdç6ïe\ ã Fe BUS!„e° ç>ˇ ®ˇç?ˇlô ã Ze BU
STYPER+£ |   % J ·eJ π Ç6\ ã Ûe BUS@ fç>ˇ
 •ˇç?ˇl  ã  f BUSINPUTR+£ |    f% ‰ J π Ç
6\ ã ,f DERROR?R+ d   Ne f∏   0I Ñ
? f
∏ ÚaI Ñ Ùˇô  ?ô ód\ ø+E ø+Í ø+  FFôdç>ˇ L
ˇç?ˇ† ¢  Ü àd@6\ ¢ †  Ü ód 7@6ì ‡3 no dev
ice\ ÜêÖÆç>ˇ ±ˇç?ˇ©`ç>ˇ ìˇç?ˇç>ˇ Æˇç?ˇ•ê
 HHlØd`˘d°  Ld•  I Ö ê Ê l¢dâ éd ?DEVICER
+àdç6˜d\ $eÜö©  S •& Ld•&ç>ˇ ±ˇç?ˇ•$ `ç>ˇ
 ìˇç?ˇ¶&Üôlhaä  e BUSOUTR+àdç6"e\ ä qe BU
SOPENR+    ze\ ä Fe BUSCLOSER+  ‡  zeód

\ *** Block No. 151, Hexblock 97

   Bc C64TYPE_c©  S † D$ ±& µb∞ ©.ç>ˇ lˇ
ç?ˇHl´cl®bÜ òc DISPLAY˚>Ób c¢c c5chcKc\ Ë
cç>ˇlrˇá Jc B/BLKø+  á c BLK/DRVø+™ á ˛c
 (DRV.*  R+ d"    ˜ \ á  d DRIVER+ d$ Z ;
 \ á (d >DRIVER+ d$ ˜ Z " I \ á >d DRV?R+
Z " ˜  dh \ á yd DRVINITR+† \ à Pd I/O.*
 à Çd BUSO  ˇç?ˇ•  I Ö ê Ê ÜKÜO†  Ü Ç6\ I
Ä∞ I `I‡∞ I@` `Ñ âb PRINTABLE?Vb°  µbê Jä
l  Ö Gb C64EMITb°  µb∞ ©.lîbÖ ‰b C64CRR+
Úaêb\ Ö ˛b C64DELR+  ùêbë4  ùêb\ Ö  c C64
PAGER+  ìêb\ Ö +c C64ATjc©  S ¶&_$ ç>ˇ ˇ
ç?ˇlhaÖ @c C64AT?Mc• 8È Ö ∞ F äë 8ç>ˇ ˇç
?ˇ@(òê È(hä¢ Å Hl  Ü
ˇ•II È ç ˇl  Ç ãa
CUROFF∂a©ˇç ˇç
ˇl  É ´a C64KEYR+ìaÇ6[aÑ ˙
ˇ¥aUa\ É Ca #BSø+  É ‡a #CRø+
 É Ïa C64DE
CODER+Êa@ Ñ   ∏ Ñ   .?G d Úa@ Ñ   ∏ Q!; d
 P È ˜   ã ‰ Ô
?# \ É ¯a C64EXPECTR+Q!;
° ∏ Q!"   Ñ   Ø?I?B Óˇ~ ë4\ É @b KEYBOARD
û?La[a blb\ Ñ Pb CON!íb° ç>ˇ l    3„0(3Zd
ñ@/ . "   ; / † „0L3X3\ † π9 ô  à ˜ ≠  I
Ö ≠! I Ö † ± Ö H± Ö † ± Ö H± Ö ¢ † äÅ ë `
Z ã@ COLD a `i ≠  I Ö$≠! I Ö%† π  ë$HP¯ æ
@ Ü 9iQ@\ Z ˙@ RESTART9a `i æ@ Ü 9iö@\ ¢
† l  Å -a C64KEY?]a•Ô
]  ©ˇhl  Å qa GETK
EYWaç>ˇ }Îç?ˇI†P © l  Ç La CURONïa•J EHç

\ *** Block No. 152, Hexblock 98

  @(  ; \ V è? KEY~? V ©? KEY?~? V ¥? DEC
ODE~? V @? EXPECT~? W    SEALR+° / Z- +;
\ W |?$ONLYZ-W Û?%FORTHI-W ˛?%WORDS .W  @
$ALSOÂ,W  @+DEFINITIONSø-R+% " ∏ Ö " Ù Z
I ; " } ô Ñ Íˇ\ R+°  7;  ;#;; H;\ X N? 'C
OLDI0† R+1@q@£-M@:?  L #? ?ö@\ X E@ 'REST
ARTI0XlR+/
R+ª?Ñ   Ö>Ö>° \ U ô> ?CRR+J?
6   I [ Ñ    ?\ R+A)∏  !a Î(l@(J Ù " ˜ ª
\ V ≥> OUTPUT:R+r+Î(l@(Ù ; \ V Î> EMITZ>
V  ? CRZ> V  ? TYPEZ> V  ? DELZ> V (? PAG
EZ> V 3? ATZ> V ?? AT?Z> V i? ROWR+o?Ä \
V t? COLR+o?J \ R+A)∏  !a Î(l@(J   " ˜ ª
\ V D? INPUT:R+r+Î(l   CLEARR+X ∏ } =L ;
 \ T °= (FORGETR+∏ î(‡3 is symbol˘;=\ T
ª= FORGETR+;/∏ / $ "   ‡3 protected©*∏ î(
Ñ   Í*B   Ö Ö E=\ T = EMPTYR+/ $ " } =/
 2 " 0 ; \ U  > SAVER+X } =% " ∏ Ö Ö " Ù
 Ö ; " } ô Ñ Íˇ} ≠ /   D \ U >> BYER+Ï9Êc
\ R+Ø?P I Ñ   ì 9 \ U W> STOP?   ˙j END-C
ODER+D,Ö " D,; \   Ñm INDEX.*       Ä Ä

 ÄÄÄÄ    ÄÄÄÄÄ   ÄÄ,.*    üm MODE:R+A) !Î
(l@(J Im; \   Om .Am   Ïm #m m   ˆm ,X
m    n ,Ym    n X)m    n )Ym    n )m
 R+Ñ   Im"      Im; º Im"    1 } Ñ   ° I
  ∏ ˜ J π Ù # " 1 ô \   *n CPUR+A) !Î(l@

\ *** Block No. 153, Hexblock 99

   Ô
?# d Ä \ "  ] DIGITSû?La[a&]lb\ R+V
v" ° I   ÊaêbJ π ° |v" ° I   -x  J π ∏ S
|v∑u‡wã PwK\Ä \ R+#?  <
?õvG J?I _4\ R+
 2 vE[o?~zº ° d?Ø$ replace with: o?|v∑uC]
° ° d?Ø$ >     search: o?Vv∑uC]° F È d?Fv
" P ò5È d?X # P {]W?Q!" } Ñ   X | ‰ _'Ä F
v; d?Fv" P    &\\ R+`\é3" ∑9âwÑ   zÄ é3"
 ∑9cwJ9     \ R+K\     \ R+ê\     \ R+é3"
 ∑9
w∫v˜  v:\° \ R+K\Ä º é3[ ¶\\ R+K\Ä ì
é3[ ¶\\ R+K\Ä ≤v" ∏ ¶[J [ â Ñ   _ é3[ ¶\\
 R+K\Ä ∂v" é3" ∂v; é3; ¶\\ " ∂z DIGDECODE
R+Êa@ Ñ   ∏ Ñ   .?G d Úa@ Ñ   ∏ Q!; d ß"∏
 J%Ñ   Ä P È ˜   ã ‰  AwË õv3 D Awõv3   †
® \ R+Ë Awõv3 D \ R+∑9Ö " \ R+ìaÇ6ª?Ñ ˙ˇ¥
a\ R+é3" Z " ˜  dè ã \ R+o?E[~z° ° d?¶[Ø$
 Scr # π5Ø$ Drv π5é3" å[ô Ñ   Ø$ not Ø$ u
pdatedº º d?/ üzL #?F _4/ ∏zL    1 #?ízd?
ñ[~[° \ R+o?ã õv# $ ˜ ô3; \ R+ô3" ∏ õv# x
 õvI I ¯cG K õv# 6 ã d?\ R+ v    R+∫v˜ °
\   äv RVSONR+   êb\   Vz RVSOFFR+  íêb\
  âz ***ULTRAFORTH83***    ùzrFORTH-GESEL
LSCHAFTR+ %\   ©v STAMP$.*
   R+|z# L Awõv˜ Ù I ã Pw° \   Sz GETSTAM
PR+   2{b|zÂ  ?Ø$ your stamp: ~z   _4z?
 d?|za    W?ízQ!" |z# ‰ \ R+|zJ S  [\ R+

\ *** Block No. 154, Hexblock 9a

  Ú  y\ R+ëx∏ S    êb\ R+Jv∑u˜ # Pv" µwK
[ ∏ S ‡wJ Jv∑u˜ ‰ º Jva [ \ R+Äy} S ëx\ R
+Äy} S Qy\ R+Jva " ô } S -x∏ S ì Jva [ Jv
∑u˜ J ‡w‰ \ R+Pv∑u˜ µw[ ∏ S KwPv∑u˜ õvD õ
vPva [ \ R+y} S Tx\ R+y} S %y\ R+õv_ Pv
a [ Pv∑u˜ KwõvD \ R+Pva " ô } S oy∏ S .z\
 R+∫vÂ ° \   S !x\ R+Bv" ô z?üvÖ [ 1 ∏ S
   êb\ R+ x∏ S    êb\ R+Aw£vq4J £vG K õv6
 ã d?° \ R+°    ° I   Ä ëx∏ Ñ   Ñ J π \ R
+   ° I     ùêbJ π ° \ R+z?° d? v∫v˜ Tx\
R+Kwõv?4® ° \ R+‡wõvJ?I ?4® ° \ R+Kw∏ õv˜
 ã Aw/ @ ˜ ∏ P Ù I D Ô õv?4® ° \ R+Ôwâ }
S Kw∏ õv˜ Aw£v˜ Ù I   vRw©  S àD$P l(v±(
@vë&HPÔãwò S Ü&≠> Ö'¢ †'±$ @vQ& ©ˇl  à Ô
 ˆvJPÁäl  R+; õv3 I \ Cwäh≠> l  Mw•JI(äê
©'EHh•II l  ‚w •JEHh•II l  R+Aw/ @ ˜ õvq4
J ô Bv"   \ R+‡wAw£v˜ G I Bv" ô 1 \ R+ævJ
 êb° \ R+ x} S ì Kwõv˜ G J ?4I â 1 Ôwâ 1
∏ S   îêb\ R+ x} S ∫v" Ñ   -x}   zv SCR>C
BM~v°  /vÅ l    Rv EDITORv-ËCËCejø+( ø+
ø+Ë      SHADOW.*u .*  .*  .*u .*ˇˇ.*  .*
iV  .*ôV  .*  ˘U.*  !Vø+H ø+J ø+ ˇø+; ø+
ˇ•$ I)Ö$ê Ê%•& I(Ö&ê Ê'` wò S Ü&≠> Ö'¢ †'
±$ @vë&à ˆ ˆvJPÓ≠; ù  ù  ù  ù  ËPÒl(vewò
S Ü&≠> Ö'¢ † ±& /vë$H@(êÙJ © ë$ ˆvlrwl(

\ *** Block No. 155, Hexblock 9b

   °&A*®† ± ô$ à ¯•$P F%F$Ê&P Ê'lıt  t
2!ûuò S † ± ë$à ˘† lÙ   óu 2@πu° Ö$± Ö%•
8È Ö ∞ F † ±$ë à ˘lha  ≤u 2VARIABLER+A)Z
˝ \   Zu 2CONSTANTR+A) ! !Î(l@(∑u\   Úu U
NLINKR+° ° / Ó úu\ ¢ † l  Ö*)? *$*   ÄP
@`Ö*)I ê
I@ê ) $*   @`©.`   v CBM>SCRFv°
  @vÅ l     +X    ˜ /  Ä[ ‡3 not here/ >ˇ
Üp\   Nt RAMR+/ ?ˇÜp\   òt SYSR+Tt/qût\ .
 zt .BLKR+D!" } Ñ   Ø$  Blk H5π>\   ºt (S
EARCHÎt† ± ô$ à ¯•( )I•$ %C°&A* •$P F%
F$Ê&P Ê'l˚t† π$ ë à ¯Ê&P Ê'Ê*P Ê+•$P F%F$
•(P F)F(•( )P •  I Ö ê Ê † •'ë •&àë àlâ •
$ %P •  I Ö ê Ê † lå  zs WDECR+∏ _p8rñqÙ
# †pπq†p\   Ws ;C:R+Ñ /qèmï0° ~'; ° \   N
l ASSEMBLERR+?j˝m\   ≤se;CODER+ )/ ˝ˇ˝ ö+
/ ˛ˇ˝ æs\   Hs CODER+A)X ∏ Ö ; æs\   Ës >
LABELR+X H)A)˛'ã  !Z i(Ä(º 1 i(X Z I Ä(Z
D Ä(~'" L    1 ˜ ; L ; Î(l@(" 1$" Ñ   ^ \
    t LABELR+X  tæs\   ñs ROMR   NOTR+
n \   Wr BEQR+drq\   àr BMIR+[rq\   òr
BNER+8rq\   ®r BPLR+orq\   ∏r BCCR+"rq
\   Hr BVCR+Frq\   Xr BCSR+-rq\   Ër BV
SR+Qrq\   ¯r 2INCR+∏ _pùnF ˙m+p∏ Üp"rñqã
 # ≠pπq\    s 2DECR+∏ _pboF ˙mYp∏ Üp-rñqã
 # †pπq\   1s WINCR+∏ ≠p8rñqã # ≠pπq\

\ *** Block No. 156, Hexblock 9c

  q ?]R+ !X # I nq !\   Zq ?[R+ !X °  !\
  ëq ?[[R+ñqã \   _q ]?R+X Ù J Ñ   ã ; B
  Ù # I nqã ‰ \   ¥q ][R+X # º <qã X Ù #
I nqã ‰ \   }q ]]R+<q\    r ]]?R+<qπq\

r CSø+ê    r CCø+∞   (r 0=ø+P   3r 0<>ø+
   >r 0<ø+    jr 0>=ø+0   ur VSø+p   Ar
VCø+P   Lr     Sp STA±oÄL   Äp ASL±o


çp DEC±oA    öp INC±o·    ßp LSR±oa

  ¥p
 ROL±o!

  Ap ROR±oA

  Np STX±oÅ    {p C
PX±o‡Ü   Ëp CPY±o@Ü   ıp LDX±o¢ñ    q LDY
±o†é    q STY±oÄå    q JSR±o Ä   )q JMP±o
@ÄÑ  6q BIT±o Ñ R+∏ =   [ ‡3
out of rang
e \   cq [[R+X \   M
o TXAnä  ~o TXSnö
  âo TYAnò  îo M/CPUR+A) ! !Î(l@(∏ # "
 Ä1 Ñ      Im[ Ù /  ˇ1 1n1nÑ   ˝mì ‡3 inv
alidJ Im" ßm˜ J ˜  !Im"    1 Ñ   Im"    1
    U Ñ    !B    !˝m\   üo ADC±o`N   %p A
ND±o N   2p CMP±o@N   ?p EOR±o@N   lp LDA
±o†N   yp ORA±o N   Fp SBC±o‡N  (J  !˝m\
  Qn BRKn   ån CLCn   ón CLDnX  ¢n CLI
nx  ≠n CLVn∏  ∏n DEXnJ  Cn DEYnà  Nn
INXnË  Yn INYnH  ‰n NOPnÍ  Ôn PHAnh
˙n PHPn    o PLAnH   o PLPn(   o RTIn
@  &o RTSn`  1o SECn8  <o SEDn¯  go SE
InX  ro TAXn™  ]o TAYn®  Ho TSXn∫  S

\ *** Block No. 157, Hexblock 9d

!  ò5{bÈ d?Vva " õvÖ J?I W?Q!" } Ñ   Vv;
d?Vv∑uC]È d?|va " õvÖ J?I W?Q!" } Ñ   |v;
 d?|v∑uC]íz~[d?\ R+¶\Ä Vv∑uô3" é3" ∑9˜ ¯c
ô3" I Ètô Ñ   ° B   é3" ∑9I ô3; :\\ R+K\Ä
 X]B^ô3" Ñ ' Ø?∏   rI Ñ   á]} Ñ   J d P I
 } S B Sˇ¶[J Fv" I °>ô 1 } Ñ   É Ñ   º\Ä
B   L\Ä B !  Ωˇ¶*_+`∞ © h•ê)øç?ˇl    ßM C
SAVEDN©   N©, XˇliN  zN CLOAD}N©   N©  Uˇ
 hòhäh† • 8È Ö ∞ F Hë HHë H(liN  SN .ERR±
N° 0 ™Jç>ˇ tÜàH±$ ) Rˇ( Ùç?ˇlha  ®N DERR
?R+∏ Ñ    ?∏ H5ØNØ$ error∏   ˇ1    I â  d
" ì - 1 Ñ   yf   d" ° }  d; \ .*Ô Ô ; \
 RNÖSTORER+Ô ã ∏ P "! + ; CH  \    M RDN
EWR+bMã bMã È /   I [ ‡3 range!Ù HÙ ;  M
ã I     I; (M° ° IL\ R+Ñ   Ø$ error \   p
M RDCHECKR+¢LpI∏ " ∏ ° ¯c   ˜ Ø â îM˜ ∏
?H5∏ " ∏ P ò5ë4Ñ + ∏ a "  dè º ò5Ø$ :F ò5
∏ Z ˜    #?°>S B ¨ˇîII îM\  S ç>ˇ©Ä êˇ© Ö
ê•$8È ç d∞ Jé d¶$_%©  ∫ˇ¶(_)•&! 3 no file
ã SJ/J¯cI Ñ   ô Ñ   äIB   äIã ¯cD B $ ô Ñ
   lJ JäI¯cìKÍKB   ∏ äIã /JhK L° \   'L .
RDR+„H" H5pIÄ îIH5   #IH5TIL #?\   úL ID!
R+   TI‰ TIL ?4®      TIP ˜ n ë$ RD.L TI#
 ã D \   CL ID"R+  "Í#IL\   ˚L RDUSER+„H;
 \    M RDDELR+pI" ∏ F  I; Z  I; îI˜ \ R

\ *** Block No. 158, Hexblock 9e

! DjK  K±(  )®™© ë&à ˚H8äE&Ö&ê Ê'lUK Jë
&Ê&P Ê'Ê(P Ê)F$PNF%PJl0K  ?K COMPRESSïK
K±(I P ËP Jäë&Ê&P Ê'¢ÄlLK‡Ä0
häë&Ê&P Ê'¢
H LJë&Ê&P Ê'Ê(P Ê)F$PBF%Pæ‡Ä0 äë&Ê&P Ê'l0
KR+äI| ˜ G ∏ J   [ Ñ   È [ Ñ Îˇ# ã I  J\
 R+| ˜ ¯c  I ?4® \   àK RAMR/WR+F ç ôJÑ
 ˛gB x ã ‡! ETER+YI∏ " | ˜ YIßIÙ I D _ ±I
\   cJ SEARCHR+pI∏ " ˜ ∏ " Ñ   È a " I Ñ
ËˇEIDI; \ R+ d  \   IH BINARYR+∏ Z " ˜ ôJ
S ∏ ∑9Ä J9lJ J¯c J\ IÄ  `I@ê I‡∞ I†`) `I`
∞ `È†`  £J C>7ÒJ°  LJl    ÈJ 7>C K°  Jl
 ©  S •&Ö*•'Ö+à¢•$P •%P HH© l  l/KÊ%`8•&
Â*h•'Â+l    ˚J EXPAN! /I RDR+5Iô ‡3 no Ra
mdisk\ R+˝HF  I; \   kI ADRR+F #I\   SI D
ATAR+YIZ ˜ \ R+Z #I\   ÉI BEHINDR+îIZ ˜ \
 R+Z  I[ \   ûI BLK#R+    I\   ΩI IDR+
 I\ R+îI   #I¯cI Z I [ ‡3 Ramdisk full\ R
+îIEI}I\ R+} ô S DI" îIa ; Z ˜ ∏ îI; ±IîI
˜ \ R+YI" ∏ ô S Z I \   OI DEL! ! ? ?Ø$ A
ber ohne Handbuch ?Ø$ und die Hilfe der ?
Ø$ FORTH-Gesellschaft ?Ø$ bleibt FORTH ei
n ADVENTURE! ?\   &H MEMTOPø+ ˝  ∫H RAMDI
SKv-JJJJ D     (RD.*    }H PLENø+1   ÈH A
DR>R+„H" I \   ˆH >ADRR+„H" ˜ \    I ADR@
R+ I"  I\    I RD?R+„H" ∏ ∏ " HI 1 \

\ *** Block No. 159, Hexblock 9f

! "    ò5\ R+∏ J P Ñ5\ R+  $J?I ° } _4\ 6
 ÔF SR+;Gë4xGZ _4∏ L #?∏ J ˜ # DG\ 6 WG N
R+;GkGF _4∏ " ©* +a DG\ 6 õG KR+;GkGa DG\
 7 πG DR+È ã ;GP _4ã ° I   xG# J π Z _4z
#?DG\ 7 KG CR+º OG\ 7 ˘G BR+;GkG∏ " Ù ˜
  ò5a DG\   $G HELPR+P çB ?Ø$ Probier' ru
hig weiter!  8" @ Ñ    +"  Fd " / l@I Ñ
 " P ˜ d ©* +Ø$ can't be DEBUGged33\ 5 ©D
 NESTR+dD" "  FÄ <DÂ \ 5 ïF UNNESTR+4DÂ 8
D˜ \ 5 ∞F ENDLOOPR+dD" Z ˜ 4D; \ 5 GF%UNB
UGC 5 „F (DEBUGR+ F±D<D˜ @DÂ hD˜  DE\ 5
 D DEBUGR+;/¯F\ 5  G TRACE'R+;/∏ ¯Fó C \
R+π>∏ Z ò5Ø$ :\ R+∏ !  <D˜ Ô 8D 24D 2∏ Ö
±DhD 2º hD[ P    EP    2Ù  2Ö ∏ dD;  ?hD"
 _4∏ Z ò5" ∏    ò5ë4©* +   J?I ° } _4W51$
 2D! 2∏! 2/ (3 2/ ∫/ 2o! 2}!o!" JDB  2≥ B
 ; / äD„0(333\ ÚE©EÖ © Ö ©lÖ l  R+∏ " / L
+" @ Ñ    +d / Ø?" @ Ñ    +J   " ˜ "  Fd
/ #?" @ Ñ    +J Ù " ˜ "  Fd / !  B Ô ã D
\ 1    CPUSHR+Ô z Ù P ≥ Ù # I ∏ B n lDP P
 \ R+  r˝ {bTcÌ1ë4Ô!˘// Æˇ˝ 9 \ 1 BD RANG
ER+8D˜ ∏ 4D; # ∏ " / \ I Ñ Úˇn 8D; \ WD©ˇ
çbDçcD° Ö ± Ö •  I Ö ê Ê ≠2DÖ ≠3DÖ l  2.*
UD\ •  I Ö ê Ê ≠bDÊ≠>DP • M6D• Ì7DêU• M:
D• Ì;D∞Il9Eé>DébDécD• ç2D• ç3D Ü <D" Ñ "

\ *** Block No. 160, Hexblock a0

! ∫v˜  ?d Úa@ Ñ < ∫v˜  C∏ Q!" [ Ñ   Ä Q!"
 £ |   È ˜ % J |vã ‰ # J π ∏ Q!; d ∏ æv‰
   „_ó 5`Ä \ - @C EDIEXPECTR+O`Q!; ° ∏ Q!
"   Ñ   Ø?I?B Óˇ~ ë4\ - ∂C EDIBOARDû?La[a
lCBC\ / VB TOOLSv- H Hôv0 ™t WCMPR+Ù _p∏
epã # _p# Yp\ .*  .*ˇè.*ıè.*Îè.*è.*Sè.*G
è.*≥ L È ˜!  Ø$ , saved   1 Ñ   Ø$ , load
ing ?é3" ô3" LA\ , ÒA LR+ô3˜ ¯A\ , âB RR+
é3" ¯A\ , ôB +LR+é3" ˜ çB\ , ©B VR+;/©*}
Ñ   Z I " \ , ºB VIEWR+@B} Ñ   çBB   Ø$
f
rom keyboard\ ,R+‚v" ÊvJ ˜ \ , i` CURLINR
+   êb  jêb C   êb  kêb   êb CÙ I \ -  C
EDIDECODER+  ç@ Ñ   ! ˘/\ * 9A SHOWLOADR+
é3 2é3˜ ô3 2/ Û``; aA„`\ R+ i˜ ∏ J Ív‰ ∏
 # J Úv‰ a J Óv‰ \ R+Z àA  ìêb¶\Ä Bv˜ Ø?∏
 æv‰ ° „_ó 5`} Ñ Ïˇ° ° d?%yÄ BvÂ ° àA° /
Í ‰ \ + AA EDITR+Kié3; y[O`™A   ° d?¶[Ø$
Scr π5Ø$ Drv π5∏ F 1 ô Ñ   Ø$ not Ø$ chan
ged∏ Z 1 Ñ   Ï9∏    1    I Ñ  !
 õv3 ˜ ∏
Vva ; õv˜ ∏ |va ; õv˜ ∏ Jv; õv3 ˜ Pv; Jva
 ˜ Pva ˜ / üzL P Vva "   D Ô Vv; / ∏zL
 1 P |va "   D Ô |v; \ R+Ë p`" I S t`\ ø+
#$R+/ d `; íz¥a\ R+D!" } ô Ñ   „`d ∏!" G
 ô3; íz¥a~zé3" Ù I Ñ   é3; ¶\º ° d?Ì1:\ìa
Ä \ * R] (LOADR+∏! 2∏!; } ô S D! 2D!; Ì1

\ *** Block No. 161, Hexblock a1

! ßˇì \ R+?$  @I \ .*     í
ç    ÖâÜäü  ã
áàå  ùë ìî      ˇ.*º\L\|\˛\˛^ zÛxÛx`\K\ê\
õ\oy%yhz"zz∫[Æy∫yFyºx{xëxTx!x!x!x†x-x!xQ
y y y`zJzzxÂ_©  S _$àHπR_Iˇ E&PÙò ®πô_hπ
ö_l  R+Ív‰ Ç6  Ä° I   J π \ R+Ív 2°  `º
`°  `º  `\ R+∏ ì I Ñ    `ô \ (  [ (PAD.*
 R+Ë ∏ p`;"  P  OP P \ R+Ô µ2Ñ   Ô  OI Ñ
  Ô Ô ; B ËˇP \   ,Oâ(RESTORE"R+X$ã Ñ   P
 ,(Ô jO  ª d Ä \   NOHRESTORE"R+6!ZO\$\
 òO DEVICE.*    ØO COMMODORER+º ∏O; \   æ
O FLOPPYR+   ∏O; \   VO BLOADR+∏O" {NZNZO
 load\   ÏO BSAVER+∏O" BNZNZO save\    P
N"R+  "Í#\   &P SAVE" #/  P∑L\ s 'N INFRO
NTR#/  PIL\ s <N COLOREDR#ã ÇL˜
‰ \ t rN
SPRCOLORSR#~L# ‰ ~L‰ \ t JN SETSPRITER#|

P \N  SM  |
 M N  VLILÔ AL∑L\ ."ÜN."ñï."ã
ï."ÅïR#  D} \ u ÜN HEADINGR#ºN" \ u ON SE
THEADINGR#ºN; \ u „N RIGHTR#ºN" ã I / H x
 ºN; \ u ˙N LEFTR#ºN" ˜
/ H x " "BKÏPw )Q
"TS QR#X L  .Ù I   _,#7 7\ R#6 9Q\ \ R#9Q
 Forth Gesellschaft e.V.\ R#9Q *** ultraF
ORTH-83 ***\ R#9Q (c) 1985/86/879Q!Bernd
Pennemann  Klaus Schleisiek9Q Georg Rehfe
ld  Dietrich Weineck9Q Claus Vogt\ R#9Q r
ainer mertins9Q antilopenstieg 6 79Q 200

\ *** Block No. 162, Hexblock a2

" 0  hamburg 54\ R#Ø7P I Ñ ¯ˇ\   qH .MESS
AGE1R#Ù_:7WQ 7ìQ 79Q Das Kopieren und Ver
schenken9Q"dieses Programms ist ausdrueck
lich 79Q
* erlaubt ! * 79Q Jeglichen Miss
brauch zum9Q Zwecke der Bereicherung9Q we
rden wir nach besten Kraeften9Q verfolgen
 und verhi" ndern. 79Q Die Mitglieder der
yQ `8R\   hR .MESSAGE2R#:79Q Du hast jetz
t ein9Q arbeitsfaehiges System mit9Q Edit
or, Debugger und Assembler!9Q Nach Einleg
en einer formatierten9Q Diskette kannst D
u es mit9Q SAVESYSTEM <name> (z.B. FORTH)
9Q als Programmfile " abspeichern. 7WQ 79
Q Bezug und Mitgliedschaft in deryQ9Q ueb
er: 7˚Q8R\   nS LINIENR#JFÛHÁHìIõG/ @ ° I
 % / @ ° I   %   Fq ° ÂJ  #˚ π   #˚ π \
 NT MOIRER#JF I∑HìIõG/ @ ° I   %   F/ ? %
 I ° ÂJP ˚ π   G° I   / ?   F% I ° % ÂJF
˚ π \ ° ¥T SHAPES."™™™ïuvïuvï™" ™ïÄ ïÄ ï™
†ïu`ïu`ï™†ïÄ ïÄ ïÄ ïÄ ïÄ ïÄ ïÄ ™Ä
  ™ ™Ä u`%uxïIvïÇvïÇvïÇvïÇvïÇvïÇvïÇvïÇvïÇ
vïÇvïIv%ux u` ™Ä         ™™™Äïu`ïuxï©vïÇv
ïÇvï©vïuxïu`ï•`ï•`ïâxïâxïÇvïÇvïÇvïÇv™Ç™
       ™™™™ïuvïuv™v™ v  v  v  v  v  v  v
 v  v  v  v  v  v  ™          ™™Ç™ïÇvïÇv

\ *** Block No. 163, Hexblock a3

" ïÇvïÇvïÇvï™vïuvïuvï™vïÇvïÇvïÇvïÇvïÇvïÇv
ïÇv™Ç™         ™°
U INITR# H:7ÁHCIÁHΩI
 ° I    U%   @$ ˜
% }LJ π {H´HVN   ° I
% ° ° ´H% íNJ π    ° I   %  N% EM% VL∑L%
0NJ π \ ¢ zV YPOSR#àMÄ \ ¢ MV XPOSR#àMJ
\
 ¢ ~V DISTANCEø#  ¢ ÔV 1+0-1R#∏ ô â ã É 3
 # 1 \ ¢  " W
FOLLOW-SPRITER#P   ÂV  G ÂV
˙V˜
Ù I  W˜
/ x K   9MÇ.  TV  G TVÙ I  W˜

Ô \MÇ.\ £  W
FOLLOW-CURSORR#P   ÂVJ7   $
   !˜
Ù I  W˜
  9MÇ.  TVz7   $   ;˜
Ù I
W˜
Ô \MÇ.\ £ PW FOLLOWR#Ç.∏ Ñ   .WB   ÄW\
 £ DW KILLSPRITESR#° AL‰ \ £ „W
SLIDE-SPR
ITESR#   ° I   % MW%"  NODRAWR#ÅG:7\ v +P
"LT!Ov >P"RT Ov gP$SETHNv pP"PD}Ov [P"PU
ÔOw DP TLINER#P P P LEÔ LEÔ LEÔ LEÂJ\ w M
P FORWARDR#P ¥N" ∏N" Ù ºN" ÒC  DN˜
∏ ¥N;
Ù ºN" ∑CÔ DN˜
∏ ∏N; @N" Ñ   UPB   ~
~
\ w
 ëP BACKR#_ õP\ w ÂP TURTLESTATER#@NJ  FJ
 ∏    1 ã    h \ w ˆP"FDõPw  Q#  # ° I
% MWJ π J π \  TASK   XETNEXT4X2 [X(NEXTS
TEP [ùy [[2 EXCW1-tX2 ≤z$STEP~z134,13OXLI
NEìz1 äX%CPULLuz0 òX'#SPACESqz0 _X%LAST'm
z0 ∞X%TRAP?iz0 ºX%NEST?ez0 FX#IP>az0 PX#<
IP=z0   "(W9z  ôv&RANGE?_vb b  {Ç< {ˇˇ¯Z
 ¯Z¯X {$_Dõ˙ MODE s   _ GETDISKR# 7Ø  Bi

\ *** Block No. 164, Hexblock a4

 5INITR+ ? ?Ø$ Tape2.00 ÚS/ 0L„0 8º 0d&U"
 Ñ   &U˜ BP\  •≥çd 8•ùÂ≤çe •ûÂ≥çf © çg çh
 çi çj ç ˇ© Ö`ÖB©2Ö_©kÖA©ˇÖY©XÖXç ˇ© ç ˇ©
* XR ¢P$≠  ©XÖY©4ÖXç ˇ•≤Ö_•≥Ö`•ùÖA•ûÖBç?ˇ
©E XR ¢Pl≠P2 R SUPERTAPER+   ∏O; / ∞Q/ .
 ; / =S/ 0 ; Ø$ ST2.20 \ R+∏!4O+P\ 2 ÊS B
TLR+/ ÇP/ " }R\ R+Ë    ?4®  T     Ë ã D á
RL Ë    ˜ ã D Ë    \ 2 'T STSAVSYSR+∏O4Oº
 ∏O; -T9T P   ∏O; ≠    I -TÄ  T P-TJ X +P
 P\   ITc\IFR+ $ı.ô Ñ   Û$Ä \   ™T SAVESY
STEMR+é34Oº é3; ô34O° ô3; |z4O|z˜ p`4Op`˜
 e>∏O"    I Ñ   TTd ≠    I X +P P\   DT A
UTOLOAD.*     U TAPE" .Sç>ˇ`•ÆI  8) P  l
_Ò∫ÜZ  S   „∞‰X D„ ç„ ¢P•≠çb •≤çc •≥çd 8•
ùÂ≤çe •ûÂ≥çf © çg çh çi çj ç ˇ© Ö`ÖB©2Ö_©
kÖA©ˇÖY©XÖXç ˇ© ç ˇ©* XR ¢P$≠  ©XÖY©4ÖXç
ˇ•≤Ö_•≥Ö`•ùÖA•ûÖBç?ˇ©E XR ¢Pl≠P2 R SUPER
TAPER+   ∏O; / ∞Q/ . ; / =S/ 0 ; Ø$ ST2.2
0 \ R+∏!4O+P\ 2 ÊS BTLR+/ ÇP/ "  ≠c Ö_≠d
 Ö` ≠e E_ÖA≠f E`ÖB©oçH ≠b 0 ©µçH ç?ˇ©E ˇP
l≠P øQê   øQê  l  , tP G----.*
g726e ìR†
 fwê •YçH © , ˇ˚ ëP• i Ö ∞ © , ˇ˚ ëP• i
 Ö lTR•]I Ö]•^I Ö^•XçH àPæ`h¢@© Öw êRJPˆH
Öw êR† Ñ]Ñ^±_Öw ìRÊ_P Ê` ìR•_EAPÍ•`EBP‰•]
¶^Öw êRäÖw êRlêR© † ô2 àP˙_´ç?ˇà0 ±Øô2 l

\ *** Block No. 165, Hexblock a5

"
^†  QPàP˙Öˇ•]ÖC•^ÖD QP QP•ìP •ˇë_ QP QP
•ˇQ_ Ë QP QPÊ_P Ê` QP QPÖˇ•`EBPC•_EAPΩ•ˇ
EC •ê `Öêl∞P†  QPàP˙EDPÎ‡  © Öêl∂P`Öì©
Öê•ÆI  lp∫ÜZ  „∞ËX•ìh© Öì `Ò ç„ ¢P D„ Ç
P©µçH ©* ˇP†C JÎ† π2  RˇH@ Pı†ˇHD´(ç?ˇ±Ø
ç>ˇY2 ÓI?Í≠b )  ©ÄÖêl≥P X„lRQl¸QHÖì âÒ
•¥Ö_•µÖ`•≠" RAMDISKR+pIßITIL  P\   6P LOA
DRAMDISKR+5Iô Ñ   bMCHxMpIë$ RD.L ÙOÄ \ ©
 Ö`ÖB©2Ö_©kÖA`≠H ç ˇ© ç ˇ© ç ˇ`¢ † JP˝àP˙
`© ,© ,© ,© ,© ç>ˇh ∞„ X„¶ZHö¶__`I x`© ,
ˇ˚• ) Ex  ÖxFw•w  Ê]P Ê^• ) Ex¯Öx ëP•w
`Öy¢  QPI P˘¢ †  QPàP˙I PÎJPÒ†  QPàP˙I Ù
Ey IET© Öêl∞P© Ö]Ö" ºN; \ u  O"CSJFu 7O
"PCYIu @O"BGΩIu iO*FULLSCREENõGu rO+SPLIT
SCREEN@Gv CO XCORR#¥N" LE\ v UO YCORR#∏N"
 LE\ v àO SETXR#qE¥N; \ v õO SETYR#qE∏N;
\ v ÆO SETXYR#µO¢O\ v AO PENDOWNR#@NÂ \ v
 SO PENUPR#@N˜ \ v ÁO HOMER#  †  `IO  zN
}O\ v ˘O DRAWR#JF P   @G\ v  P#
   û(2
064)    Íl 9Íl99l { J. { { £b    M;Ü:®+Â
a





\ *** Block No. 166, Hexblock a6

# tte Diskette # π-Ø  einlelDõ J..Y.Y4Z4Y
    S;!r®` F




          #  INPUT<$   ˘  ERRORHANDLER<$
     VOC-LINK<$      UDP<$   *  SP@= • Ö$
• Ö%¢$• 8È Ö ∞ F µ ë µ ¢ l    5  SP!G ° ™
± Ö Ü ¢ l    _  UP@ ¢ lg   W ÉUP!é ¢ ± ï
 à± ï ¢ † •  I Ö ê Ê l    Ü  RP@µ ¢ lg
≠ ÉRP!D ¢ lê   º Ç>RR • 8È Ö ∞ F ° Å ± ë
lù   K ÇR>Ò • 8È Ö ∞ ØÑ 3 Ä  7Ø  Kill the
 Demo? n/y Ø7ß   YI ∏ â Ñ   .7.7.7Ñ òˇáZ\
   |Z
DEMONSTRATIONR#£% H/ BZ„( 0/ áZ„(O8
mZ `Ç.Z VZ; / ‰Z„((+/ ®+  ; / † „(L+tRWTÅ
GØ7Ä ºTÅGØ7Ä Ø  helpz7G ° d7X+\

                              # I Ö ê Ê l
      CTOGGLER#|
J n ã ‰ \      @$ ° Ö$±
 Ö%±$ë °$l       != ° Ö$± Ö%H± Å$H± † ë$l
Ù   7  +!] ° Ö$± Ö%H±  A$Å$H± † Q$lo   v
 DROPù   Y  SWAPç ± ™† ± Ö$äë •$† ë H¢ ±
Ö$° ë à•$l    Ñ  DUP∫ • 8È Ö ∞ F † ± † ë
H± àl    ≤  ?DUP °   P l  l∫   V  OVERˆ

\ *** Block No. 167, Hexblock a7

#  0ï° E$± Â%lá ' Q  U<  ° Ö$± Ö%•  I Ö ê
 Ê ° E$± Â%∞ lâ lå ( ˝  >R#ã U \ ( )  0>R
#_ É \ ( 7  0<>R#ô â \ ( f  U>R#ã   \ ( v
  =R#I ô \ ( E  D0=R#  ô \ ( S  D=R#µ ı Y
 \ ( É  D<R#
È
I Ñ   - J
J
B   ~
  \ R#9
 Ñ   ã Ä \ ) î  MINR#È
- µ \ ) E  MAXR#È

U µ \ ) W # ‚
 +˘
 H° Q ë à± † Q ë lô   Û

 OR  H°   ë à± †   ë lô      AND3 H° 1 ë
 à± † 1 ë lô   +  XORp H° q ë à± † q ë lô
 ! h  -K H± 8· ë H± † Ò † ë lô ! E  NOTã
 ä· Å äÒ ë l  ! É  NEGATE¶ 8∞„" õ  DNEGAT
E∑ H8äÒ ë HäÒ ë ä· Å † äÒ ë l   ™®à± ô$ à
 ¯ä E Ö ê Ê ¢ † `" ´# Ö † l  + B Ü(+LOOP˝
  ° A Å ± Q ë Jq  •  I Ö ê Ê ( Dl  , Ú ÅI
' † • 8È Ö ∞ F  ± HHQ Å à± HHQ † ë l  , !
 ÅJs † PR- m ÜBRANCHD  • A Ö$• Q Ö •$Ö l
 - y á?BRANCHÜ °    •  I Ö ê Ê (Kld . Z
 >MARKR#X °   \ . û  >RESOLVER#X Ù I ã ;
\ . ≤  <MARKR#X \ . M  <RESOLV# $ ‡  OFFR
#° ã ; \ % Ò ÑCLIT  • 8È Ö ∞ F ° Å äë Ê P
 Ê l  %   ÉLIT1 • 8È Ö ∞ F ± ë ° Å •  I Ö
 ê Ê l  % ) GLITERALR#∏ /  ˇ1 Ñ   6 /   d
 6     \ & t  0<Ö ±   ©ˇ$äë l  & ~  0=õ °
   ËPÈ& î  UWITHIN± ©  S † ° E$± Â%∞ ° E
&± Â'∞Ωlå ' •  <W ° Ö$± Ö%•  I Ö ê Ê •%q

\ *** Block No. 168, Hexblock a8

#
ER#X I   \ . }  ?PAIRSR#I ‡+ unstructur
ed\ I   h©ˇl  h© ™• 8È Ö ∞ F ä† ë H¢ l  /
 Ù  CASE?b ©  S •$A P •%Q P lâ äl  0 8 BI
FR#6 Ñ ¶ º \ 0 \ DTHENR#= º ˝ Ω \ 0 O DEL
SER#º ˝ 6 B ¶ ã Ω ì \ 0 Ñ EBEGINR#U F \ 0
 ° EWHILER#F ˝ F 6 Ñ ¶ / ˛ˇH
\ R#Ë ∏ / ˛ˇ
I Ñ   Ä Ω #
    ." ultraFORTH-83 3.80-C64  çˇˇ± Ö ≠ˇˇ
Ö  • I Ö ∞ LˇˇÊ ∞˘     END-TRACEE ©•Ö © Ö
 ©IÖ © Ö l    w  RECOVER."HÖ HÖ • P F F l
R#  Z  NOOP    ô  ORIGINø#    _  S0<$   ≥
  R0<$   Ω  DP<$   G  OFFSET<$   Q  BASE<
$     OUTPUT<$   Î #
        MÇ_ ˝W DEMOø#4Y_ 8Z SLIDER#?ZW`AV

XB ¸ˇ\ _ eZ ENDSLIDER#ÒW?ZW`ô_\ ."_Z  _Z
 KILLDEMOR#ÒWJZÙ_zS/ ∂?„( 0/ † „(O8/ † „(
ò8/  +„((+]H/ $_E5e6    _\ R#VZ" ° } ∏  N
∑H\Nì VZ[ AL *ÒW∂?\ R#Ì) 7Ô ˘'1 " Ñ   Ø
 compilingB   Ø   uF83VZ" É ∏ #  F ° Å ±
ë ©  E Ö ê Ê l    Í  R@  • 8È Ö ∞ F ± ë °
 l      ÖRDROP    1  EXITf ° Ö ± Ö l    =
  UNNEST^ ° Ö ± Ö l    s  ?EXITU °    •
I Ö ê Ê (Pæl    K  EXECUTEô ° Ö ± Ö •  I
Ö ê Ê l    ç  PERFORMR#" ó \   ±  C@L ° Ö
$± Ö%© ë °$l    E  C!Ê ° Ö$± Ö%H± Å$à•

\ *** Block No. 169, Hexblock a9

# g˝eód d   zeë$ u2:13,0,L ˝e¯f˝eódÇ6yf\
é ]g DISKOPENR+ d
Pe  #·eódyf\ é ∏g DIS
KCLOSER+ d
àeód\ é Yg 1541R/WR+ã ‡3 no
filez  d6 ∏  d; P [ Ñ   π5Ø$ beyond capac
ityJ d CgÑ   Ä J d ° ã 3 3 Z £ I . Ä È %
  Ñ   Óf&gB   ÓfKgP  g˜ Ô ∏ Ñ   Ñ J π z ~
 Âg\ è Ùg #  • 8È Ö ∞ F † ± Å H± † ë l
 Ì  ROT
† ± Ö%† ± † ë † ± Ö$•%ë † •$ë H±
 Ö%° ë † ± Å •%ë † l
 -ROTR#

\
s
 NIPR#ã Ä \   D
 UNDERR#ã Ù \   T
 PICK
R## 3 ; ˜
" \   Ü
 ROLLR#∏ P ç
; ∏ a Ô #
3 Ú Ä \   ù
 2SWAPR#
P
Ô \   @
 2DROPÙ
   V
 2DUPR#Ù Ù \   #  UMAXR#È
  µ \ ) È
 UMINR#È
[ µ \ ) ¸  EXTENDR#∏ É \ )    DA
BSR#  Ñ   µ \ ) "  ABSR#  Ñ   _ \ R#9 Ô a
 ∏ P
P ã P P \ * 7 É(DOR#Ù I i \ * C Ñ(
?DOR#Ù I } Ñ   i Ô ∏ " ˜
P Ä \ * U  BOUND
SR#Ù ˜
ã \ * ö áENDLOOPª © l  + Ø Ö(LOOPL
  © A Å ê ± I ë ê l  † ± Ö à± #   D+˜ ©
S H±  E&ë H± E'ë ° E$Å † ± E%ë l  #   1+
% ©  A ∞ l  Å ± I ë l  #    2+c © P‡# <
3+p © PSv © PM\ © PG# i  1-I 8° È ê l  Å
± È ë l  # B  2-á  ê‡$ Ä  TRUEø#ˇˇ$ å  FA
LSEø#  $ ô "-1ì $ ß !0° $ ∞  1ø#  $ ∏  2ø
#  $ B  3ø#  $ L  4ø#  $ V  ONR#ì ã ; \
