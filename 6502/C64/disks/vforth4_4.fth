
\ *** Block No. 0, Hexblock 0

\\ Directory volksFORTH 4of4   26oct87re

.                          0
..                         0
C16-Tape-Demo              2
C64-Grafic-Demo            6
cload/csave              &13
Tape-Version:LoadScreen  &16
Ramdisk                  &21
Supertape                &32
auto-Decompiler          &51
Screenswitch             &61
Grafic                   &64
Math                     &90
Sieve Benchmark         &138
Grafic-Demo             &144
Sprite-Demo             &160
Sprite-Data             &165
Sprite-Editor           &166







\ *** Block No. 1, Hexblock 1

\\ Content volksFORTH 4of4     26oct87re

Directory                       0
Content                         1

C16-Tape  -Demo           &2-  &5
C64-Grafic-Demo           &6- &12
cload csave              &13- &15
Tape-Version:LoadScreen  &16- &20
Ramdisk                  &21- &30
                              &31 free
Supertape                &32- &50
automatic Decompiler     &51- &60
Screens via UserPort C64 &61- &63
Grafic  C64 only!!       &64- &88
                              &89 free
Math                     &90- &96
                         &97-&100 free
Tape Ramdisk Supertape  &101-&135 shadow

Sieve Benchmark              &138
Grafic-Demo C64 only!!  &144-&155
Sprite-Demo C64 only!!  &160-&164
Sprite-Data                  &165
Sprite Editor           &166-&168

\ *** Block No. 2, Hexblock 2

\ DemoL:C16Tape-Demo ?dload   clv10oct87

\ Demo: 80 Screens in total !!!
\ checks if a word is defined:

| : exists? ( string--flag)
      cr capitalize dup find nip under
      0= IF ." not " THEN ." found: "
      count type ;

\ last accessed diskf:

| Variable LastDisk   -1 LastDisk !

\ load SCR from DISK, if WORD named
\ STRING is not in Forth Dictionary
| : ?dload  ( string scr disk--)
  2 pick exists?
  IF drop drop drop exit THEN
  dup LastDisk @ -
  IF flush ."  Insert #" dup .
     key drop  dup LastDisk ! THEN
  drop ."  scr#" dup . cr
  load exists? 0= error" ???" ;
-->

\ *** Block No. 3, Hexblock 3

\ DemoL:?reloc                clv10oct87

\ relocates system call COLD if necces.
| : ?reloc  ( s0 r0 limit --)
 dup           limit =
 2 pick origin $a + @ = and
 3 pick origin  8 + @ = and

 IF drop drop drop exit THEN
        ['] limit >body ! \ limit
            origin $A + ! \ r0
    dup 6 + origin   1+ ! \ task
            origin  8 + ! \ s0
 cold ;

\ compiles forward references that will
\ be loaded later
| : (forward"    "lit capitalize find
 IF execute
 ELSE count type ."  unsatisfied" quit
 THEN ; restrict
| : forward"  compile (forward" ," ;
 immediate  restrict
-->


\ *** Block No. 4, Hexblock 4

\ DemoL:64kb C16Demo          clv10oct87

\ configures system for 64K if possible

: 64kb $533 @ $fd00 -  ?dup
 IF cr u. ." too small" exit THEN
 limit $fd00 -
 IF $8000 $8400 $fd00 ?reloc THEN ;

\ will be installed as 'RESTART:

: c16demo  cr ." c16-Demo"
  forward" tapeinit"
  0 drive   forward" floppy"
  cr ." Type 'help' to get help"
  cr ." Type '64kb' to use 64kb" ;







-->


\ *** Block No. 5, Hexblock 5

\ DemoL:C16DemoLoad          cclv14oct87

\ This word load the complete
\ Demo-Version. Will be installed as
\ 'COLD and later as C16DEMO

| : c16DemoLoad
  $9000 $9400 $c000 ?reloc
  Forth     " Code"       5 3 ?dload
  Forth     " Editor"   $13 3 ?dload
  Forth     " debug"    $2f 3 ?dload
  Forth     " help"      $a 1 ?dload
  Forth     " Tapeinit" $10 4 ?dload
  ['] noop    Is 'cold
  ['] c16demo Is 'restart
  forward" Editor" forward" Ediboard"
  1 scr !  0 r# !       save
  $7a00 $7bf0 $8000 ?reloc ;

' c16DemoLoad Is 'cold  save

cr .( Type : cold)
cr .( after all: savesystem!!!)



\ *** Block No. 6, Hexblock 6

\ Graphic-Demo for C64        23oct87re

(16 .( Not for C16!) \\ C)

Onlyforth

\needs buffers      .( Buffers?!)    \\
\needs demostart    .( Demostart?!)  \\
\needs tasks        .( Tasker??!)    \\
\needs help         .( help??!)      \\
\needs graphic      &58 +load
\needs .message2    1 2 +thru
Graphic also
\needs moire        6 +load

\needs slide  &154 +load  \ the Demo

 3 5 +thru

1 Scr !  0 R# !

save




\ *** Block No. 7, Hexblock 7

\ demo-version                 06nov87re

| : (center."  "lit count
 C/L over - 2/ spaces type cr ;
restrict

| : c."  compile (center." ," ;
immediate restrict

| : .FGes c." Forth Gesellschaft e.V." ;

| : .vF83  c." *** volksFORTH-83 ***" ;

| : .(c)   c." (c) 1985-2006"
c." Bernd Pennemann  Klaus Schleisiek"
c." Georg Rehfeld  Dietrich Weineck"
c." Claus Vogt  Ewald Rieger "
c." Carsten  Strotmann  " ;

| : .source  c." www.forth-ev.de"
      cr     c." volksforth.sf.net" ;


| : wait   BEGIN  key 3 -  UNTIL ;


\ *** Block No. 8, Hexblock 8

\ demo-version                 20oct87re

: .message1  ( -- )   singletask
 page .vF83 cr .(c) cr
 c." volksForth is free software"
 c." see file COPYING in the"
 c." distribution package"
 multitask wait ;

: .message2  ( -- )
 page c." You now have created a"
 c." worksystem with Editor,"
 c." Debugger and Assembler!"
 c." Please insert an empty, formatted"
 c." Disk and save the new system with"
 c." SAVESYSTEM <name> (eg. FORTH)"
 c." as a loadable program file"
 cr .vF83 cr
 c." Information on volksForth from"
 .FGes c." on:" cr .source wait ;






\ *** Block No. 9, Hexblock 9

\ demo-version                 20oct87re

graphic  also

| Variable end?

: killdemo  ( -)
 killsprites endslide
 singletask  .message2
 ['] 1541r/w Is r/w
 ['] noop Is 'cold
 ['] noop Is 'restart
 ['] (quit   Is 'quit
 nographic
 [ ' demostart >name 4 - ] Literal
 (forget  save  &16 buffers ;










\ *** Block No. 10, Hexblock a

\ demo-version                 06nov87re

| : demor/w  ( adr blk r/wf - f)
 end? @  0 max  dup small  red colored
 -1 end? +!  sprite push  killsprites
 1541r/w ;

| : demoquit
  BEGIN .status cr query interpret
   state @ IF   ."  compiling"
           ELSE ."  vF83" THEN
   end? @ 0< dup
   IF drop
    cr ." Kill the Demo? n/y "
    key capital Ascii Y =
    dup not IF  del del del  THEN
   THEN
  UNTIL  killdemo ;








\ *** Block No. 11, Hexblock b

\ demo-version                 20oct87re

: demonstration
 Onlyforth graphic
 ['] demor/w Is r/w
 ['] killdemo Is 'cold
 slide multitask pause   4 end? !
 ['] demoquit Is 'quit
 ['] (error errorhandler !
 ['] noop Is 'abort
 .message1  linien text
 key drop  moire text  key drop
 ." help" row 1- 0 at abort ;

' demonstration Is 'cold
' killdemo Is 'restart










\ *** Block No. 12, Hexblock c

\ hires demo words             06nov87re

: linien
 clrscreen yel blu colors hires
 &320 0 DO
    &320 0 DO I &198 J 0 line &35 +LOOP
 &35 +LOOP ;

: moire
 clrscreen ora red colors hires
 &320 0 DO
  I &198 &319 I - 0 line
 3 +LOOP
 &199 0 DO
  &319 &198 I - 0 I line
 2 +LOOP ;










\ *** Block No. 13, Hexblock d

\ cSave cLoad..               clv10oct87

Onlyforth
\needs Code   .( need Assembler!) quit

$ff90 >label setMsg   $90 >label status
$ffba >label setlfs $ffbd >label setNam
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
 status lda $bf # and
 (16 ram C) push jmp end-code
-->


FORTH-GESELLSCHAFT (c) bp/ks/re/we/clv

\ *** Block No. 14, Hexblock e

\ ..cSave cLoad               clv10oct87

Code cSave ( f t+1 Name Nlen dev--err)
 5 # lda    SLPars jsr
 N 8 + # lda bsave jsr
 slErr jmp end-code

Code cLoad ( f Name Nlen dev--t+1 err)
 4 # lda    SLPars jsr
 0 # lda     bload jsr
 php pha tya pha txa pha 0 # ldy
 SP 2dec pla SP )y sta iny pla SP )Y sta
 pla plp slErr jmp  end-code

-->

\\ possible errors
 AR CF ST                  Basic  Forth
 xx  L 00 no error            0     0
 00  H 00 stop-key           1e    1e
 00  L 60 end-of-tape        04    00
 00  L 10 load/verify-error  1d/1c 1d
 00  L 60 Checksumerror      1d    1d
0-8  H 00 Kernal-Error       0-8  0-8
FORTH-GESELLSCHAFT (c) bp/ks/re/we/clv

\ *** Block No. 15, Hexblock f

\ ..cSave cLoad Luxus         clv10oct87


Code .err ( err#-err# ) \ prints message
 SP x) lda 0>=
 ?[ (16 tax dex rom $8654 jsr C)
    (64 .A asl tax rom
    $a326 ,x lda $24 sta
    $a327 ,x lda $25 sta dey C) dey
    [[ iny $24 )y lda php  $7f # and
       $ffd2 jsr plp 0< ?]
    (16 ram C) (64 ram C)
  ]? xyNext jmp end-code


: derr?  ( err# -- flag)
 dup IF cr dup u. .err ." error" THEN
 dup $ff and 5 = not
 (drv @ -1 > and
 IF derror? or THEN
 (drv @ 0 max (drv ! ;

\\ for usage after CSAVE and CLOAD.
   The last line is only for
   Compatibility with old version.

\ *** Block No. 16, Hexblock 10

\ TapeVersion:LoadScreen      clv12oct87

Onlyforth

\needs Code  .( ?! Code !?)   quit

              5 +load    \ Ramdisk
             -3 +load    \ csave/load
           1  3 +thru    \ Tape
        (16 $10 +load C) \ superTape
              4 +load    \ savesys

Onlyforth
Variable autoload   autoload off

: tapeInit  cr cr ." Tape2.00 "
 \if supertape supertape
 ['] ramr/w Is r/w  1 drive
 autoload @
 IF autoload off loadramdisk THEN ;

save
' tapeInit Is 'restart
\ restart


\ *** Block No. 17, Hexblock 11

\ store restore               clv24jul87

\ wie push pull abort"

| Create restore 0  ] r> r> ! ;

: store ( addr -- )
 r> swap dup >r @ >r  restore >r >r  ;
 restrict
\ rstack:   restore date address ....

| : back \ -- \ rewinds rstack
 r> BEGIN rdepth WHILE
      r> restore =
      IF r> r> ! THEN REPEAT >r ;

: (restore"    "lit swap IF
   >r clearstack r> back
   errorhandler perform
 exit THEN  drop ;  restrict


: restore"  compile (restore" ," ;
 immediate  restrict


\ *** Block No. 18, Hexblock 12

\ tape-interface              clv01aug87

\needs cload   .( ?! cload ?!)   quit
\needs restore .( ?! restore ?!) quit

Variable device      0 device !
: commodore     1 device ! ; \ device..
: floppy        8 device ! ;


: bload  ( [from name count -- ]to)
  device @ cload derr? restore" load" ;

: bsave  ( [from ]to name count--)
  device @ csave derr? restore" save" ;

: n" ( -- adr count) Ascii " parse ;









\ *** Block No. 19, Hexblock 13

\ Ramdisk TapeInterface       clv29jul87

Onlyforth Ramdisk also

: saveRamDisk
  rd behind id count bsave ;


: loadRamDisk
 rd? 0=
 IF range memtop  rdnew rd THEN
 " RD." count bload drop ;














\ *** Block No. 20, Hexblock 14

\ \if savesystem"             clv01aug87

\needs restore" .( ?! restore" ?!) quit

Onlyforth

: \if name find 0=
 IF [compile] \ THEN drop ; immediate

: savesystem \ -- name must follow
 \ Forth-Kernal a la boot:
   scr store 1 scr ! r#  store 0 r# !
 \ Editor  a la boot
  \if Editor  [ Editor ]
  \if Editor stamp$ store stamp$ off
  \if Editor (pad   store (pad   off
   save
 \ Supertape? if then other routine
  \if supertape device @ 7 =
  \if supertape IF stSavSys exit THEN
 \ now we save
   origin $17 - here n" bsave ;




\ *** Block No. 21, Hexblock 15

\ RD: loadscreen              clv01aug87

Onlyforth

(16 $fd00 C) (64 $c000 C)
Constant memtop

Vocabulary Ramdisk
Ramdisk also definitions

      1   9 +thru

Onlyforth













\ *** Block No. 22, Hexblock 16

\ RD: basics                  clv01aug87

Variable (rd     (rd off
$31 constant plen

: adr>   ( adr--ofs) (rd @ -          ;
: >adr   ( ofs--adr) (rd @ +          ;
: adr@   ( ofs--adr) >adr @ >adr      ;
: rd?    ( -- adr flag )
   (rd @ dup   dup   @ plen =     and ;
: rd     ( -- adr)
   rd? 0= abort" no Ramdisk" ;

| : take   ( adr--   ) adr> 2 >adr !  ;

: adr    ( --adr   ) 2   adr@         ;
: data   ( --adr   ) adr 4 +          ;

| : end    ( --adr   ) 4   adr@       ;
: behind ( --adr   ) end 4 +          ;
| : end+   ( len--   ) 4   >adr +!    ;

: blk#   ( --adr   ) 8   >adr         ;
: id     ( --adr   ) $10 >adr         ;


\ *** Block No. 23, Hexblock 17

\ RD: new delete len@ len!    clv01aug87

| : ?full      end 6 adr@ b/blk - 4 -
             u> abort" Ramdisk full" ;

| : new ( --)  end take ?full ;

| : len! ( len--) \ end new block
 ?dup 0= ?exit
 blk# @   end 2+ !  4 + dup end !
 end+  end off ;

| : len@ ( --len) \ gen length
 adr @ dup 0= ?exit 4 - ;


: delete  ( --)
 adr dup @ under + adr behind over -
 cmove
 negate end+ ;






\ *** Block No. 24, Hexblock 18

\ RD: search binary           clv01aug87

: search ( blk --) \ set current Block
 rd BEGIN dup @ + dup @ WHILE
  ( blk adr ) 2dup 2+ @ = UNTIL
 take  blk# ! ;

| : notRD? ( blk--flag) blk/drv u< ;








Onlyforth Ramdisk also

: binary ( blk--blk) \ no ComPand
 dup offset @ + notRD? ?exit
 dup block drop update
 delete new b/blk len! ;




\ *** Block No. 25, Hexblock 19

\ RD: cbm>7bit 7bit>cbm       clv01aug87

Label cbm>7b \ AR=char -- 7bitChar
 $80 # cmp 0< ?[ rts ]?
 $c0 # cmp CS ?[ $e0  # cmp CC ?[
       $a0 # adc rts ]? ]?
 $1f # and       rts end-code
Label 7b>cbm \ AR=7bitChar -- char
 $60 # cmp CC ?[ rts ]?
 $a0 # sbc rts end-code

Code c>7 sp x) lda cbm>7b jsr putA jmp
Code 7>c sp x) lda 7b>cbm jsr putA jmp
end-code












\ *** Block No. 26, Hexblock 1a

\ RD: cp1 cp2                 clv01aug87

Label cp1 ( from to count--tocount)
 3 # lda setup jsr N 2+ lda N 6 + sta
 N 3+ lda N 7 + sta dey  $7f # ldx
 N lda 0=
 ?[ N 1+ lda 0= ?[ pla pla 0 # lda
   push0a jmp ]? ][ N 1+ inc ]? rts

Label cp2
 sec N 2+ lda N 6 + sbc pha
     N 3+ lda N 7 + sbc push jmp














\ *** Block No. 27, Hexblock 1b

\ RD: expand compress         clv01aug87

Code expand  cp1  jsr
 [[ [[ N 4 + )y lda 0<
  ?[ $7f # and tay tax bl # lda
   [[ N 2+ )y sta dey 0< ?] iny
   sec txa
   N 2+ adc N 2+ sta CS  ?[ N 3+ inc ]?
  ][ 7b>cbm jsr N 2+ )y sta N 2+ winc ]?
  N 4 + winc  N dec 0= ?] N 1+ dec 0= ?]
  cp2 jmp end-code

Code compress  cp1 jsr
 [[ [[ N 4 + )y lda bl # cmp 0=
  ?[ inx 0=
   ?[ dex txa  N 2+ )y sta N 2+ winc
    $80 # ldx ]?
   ][ $80 # cpx 0>=
    ?[ pha txa N 2+ )y sta N 2+ winc
     $7f # ldx pla ]?
    cbm>7b jsr N 2+ )y sta N 2+ winc ]?
   N 4 + winc N dec 0= ?] N 1+ dec 0= ?]
 $80 # cpx 0>=
 ?[ txa        N 2+ )y sta N 2+ winc ]?
 cp2 jmp end-code

\ *** Block No. 28, Hexblock 1c

\ RD: ramR/W                  clv01aug87

| : endwrite ( compLen--)
 data under + ( [from ]to )
 BEGIN 1- dup c@ $7f u> WHILE
   2dup u> UNTIL 1+ swap - len! ;

| : endread  ( toAdr expLen--)
 under + b/blk rot - bl fill ;

: ramR/W ( adr blk file R/NotW -- error)
 2 pick notRD?
 IF 1541r/w
 ELSE swap abort" no file"
  swap search len@ b/blk = ( adr r? b?)
  IF   0= IF data ELSE data swap THEN
          b/blk cmove
  ELSE 0= IF   delete new data b/blk
               compress endwrite
          ELSE dup data swap len@
               expand endread

 THEN THEN false THEN ;



\ *** Block No. 29, Hexblock 1d

\ RD: id rduse/del/new        clv01aug87

: .rd  ( --)     (rd @ u. rd drop
  end u. 6 adr@ u. id count type ;

: id! ( adr count--)
  $20 id c! id count bl fill
  $1a umin id 3 + place
  " RD." count id 1+ swap cmove ;

: id" Ascii " parse id! ; \ id" name

: rduse ( from --) (rd ! ;
: rddel  ( --)
  rd @ dup 2 >adr ! 4 >adr ! end off ;
| : range ( adr--adr)
  limit umax memtop umin ;
: rdnew ( from to--)
  range swap range swap
  2dup $500 - u> abort" range!"
  over plen over ! rduse
  swap - 6 >adr !
  rddel 0 0 id! ;



\ *** Block No. 30, Hexblock 1e

\ RD: rdcheck                 clv01aug87

| : ?error IF ." error " THEN ;

: rdcheck
 .rd
 rd BEGIN
  dup @   dup 0 b/blk 5 + uwithin
                            not ?error
  +       dup cr u.
  dup @   dup 3 u.r space
  WHILE   dup 2+ @ blk/drv u/mod
          1 u.r ." :" 2 u.r
          dup 4 + &26 type
          stop? ?exit
  REPEAT  end -                 ?error ;










\ *** Block No. 31, Hexblock 1f



























\ *** Block No. 32, Hexblock 20

\ ST:Supertape LoadScreen     clv01aug87

(64 .( not for C64!! ) quit C)

\needs Code .( needs Assembler!) quit

Assembler
\needs rom  .( ??! rom  !??) quit
Onlyforth

   1  $12 +thru \ load supertape


\\ Supertape was developed by german
   magazin c't ( www.heise.de )
   We thank the publisher for
   permission to adapt SuperTape
   for volksForth








\ *** Block No. 33, Hexblock 21

\ ST:Labels..                 clv16jun87

\ ------ hardware-Addresses -----------
$0001 >Label pCass
$ff02 >Label pTimerB
$ff09 >Label pTimerBCtrl
$ff3f >Label pRamOn
$ff3e >Label pRamOff

\ ------ System-Vectors --------------
$0330 >Label vSave
$032e >Label vLoad

\ --- Input-Params Load/Save ---------
$ae >Label zDeviceNr
$ad >Label zSecadd
$af >Label zFilenameZ
$ab >Label zFileNameC
$b4 >Label zBasLoadAdd
$b2 >Label zIOStartZ
$9d >Label zProgEndZ

\ --- Output Params for Load/Save ----
$90 >Label zStatus


\ *** Block No. 34, Hexblock 22

\ ST:..Labels                 clv16jun87

\ ------ used System Routines ---------
$e38d >Label xCassMotorOn
$e3b0 >Label xCassMotorOff
$e364 >Label xCassPrtOn
$e378 >Label xCassPrtOff
$f050 >Label xLoad
$f1a4 >Label xSave
$f189 >Label xMsgLoadVerify
$e31b >Label xPressplay
$e319 >Label xPressRec
$ebca >Label xFoundFile
$f160 >Label xSearching
$ffd2 >Label kOutput

\ ------ used Zeropage Addresses ------

$5f >Label zBeginZ
$61 >Label zEndZ
$93 >Label zVerifyFlag
$59 >Label ZBlockKind
$58 >Label zBit
$57 >Label zByte
$ff >Label zTmp

\ *** Block No. 35, Hexblock 23

\ ST:..Labels                 clv16jun87


$d8 >Label zReservAA
$5d >Label zCheckSum
$63 >Label zCheckSumB
$da >Label zTmpSP


\ --- other Systemadressen ----------
$07c8          >Label sTime
$0332  dup     >Label sCassBuffer
$19 + $100 mod >Label cCassBufferEnd

\ --------- Konstanten --------------
$07 >Label cDeviceST
$2a >Label cHeaderMark
$c5 >Label cDataMark
$4f >Label chsl
$b5 >Label clsl
$78 >Label chssh    $34 >Label chssl
$ff >Label clssh    $78 >Label clssl
$16 >Label cSyncByte
$0b >Label cSyncBytesLoad
$40 >Label cSyncByteCount

\ *** Block No. 36, Hexblock 24

\ ST:verschiedenes            clv28jul87

Label btlBeg
Label puffinit \ Load Pointer to Buffer
 sCassbuffer $100 u/mod
 # lda    zBeginZ 1+ sta zEndZ 1+ sta
 # lda    zBeginZ    sta
 cCassbufferEnd # lda     zEndZ    sta
 rts end-code

Label timerBStart
 sTime lda          pTimerB      sta
 0   #  lda         pTimerB   1+ sta
 $10 #  lda         pTimerBCtrl  sta
 rts end-code

Label delayMotor \ Motor start Delay
 0 # ldx 0 # ldy
 [[ [[ dex 0= ?] dey 0= ?]
 rts end-code






\ *** Block No. 37, Hexblock 25

\ ST:stEnde etc.              clv23jul87

Label stEnd         0 # lda  $2c c,
Label loadError   $1d # lda  $2c c,
Label eot         $04 # lda  $2c c,
Label verError    $1c # lda  $2c c,
Label brkError    $1e # lda
 pRamOff sta  pha
 xCassMotorOff jsr  xCassPrtOff jsr
 zTmpSP ldx   pla  txs
 zBeginZ ldx  zBeginZ 1+ ldy
 01 # cmp cli rts end-code


\\  cbm: stop: ar=0  cf=1
        normal ar=0  cf=0  st=0
        eot                 $80
 load/vererr                $10
    checksum                $60
                            ...
kernal-errors ar=0..8 cf=1

 s.ROM:$a803



\ *** Block No. 38, Hexblock 26

\ ST:bitRead                  clv18jun87

Label bitRead  \ cur.Byte in AR
 $10 # lda [[ ptimerBctrl bit 0<> ?]
 pCass lda  $10 # and  zBit cmp
 0<> ?[ clc ]?        zBit sta
 zByte ror zByte lda
 0< ?[ zCheckSum wInc ]?
 [[ pCass lda $10 # and  zBit cmp 0<> ?]
 zBit sta timerBStart jsr
 zByte lda rts end-code















\ *** Block No. 39, Hexblock 27

\ ST:stRead..                 clv05aug87

Label stRead \ reads a block
 zBlockKind sta  0 # ldx
Label syncron
 [[ bitRead jsr cSyncByte # cmp 0= ?]
 cSyncBytesLoad # ldx
 [[ $08 # ldy
    [[ bitRead jsr dey 0= ?]
    cSyncByte # cmp         syncron bne
    dex 0= ?]
 [[ $08 # ldy
    [[ bitRead jsr dey 0= ?]
    cSyncByte # cmp 0<> ?]
 zBlockKind cmp 0<>
 ?[ cDataMark # cmp        syncron beq
 $10 # lda zStatus sta loadError jmp ]?
 0 # lda zCheckSum sta zCheckSum 1+ sta
 $08 # ldy
    [[ bitRead jsr dey 0= ?] zTmp sta






\ *** Block No. 40, Hexblock 28

\ ST:..stRead                 clv28jul87

 [[ [[
  zCheckSum    lda  zCheckSumB    sta
  zCheckSum 1+ lda  zCheckSumB 1+ sta
  bitRead jsr      bitRead jsr
  zVerifyFlag lda 0=
  ?[ zTmp lda   zBeginZ )Y sta ]?
  bitRead jsr      bitRead jsr
  zTmp lda      zBeginZ )Y cmp
  0<> ?[ inx ]?
  bitRead jsr      bitRead jsr
  zBeginZ wInc
  bitRead jsr      bitRead jsr
  zTmp sta
 zBeginZ 1+ lda  zEndZ 1+ cmp 0= ?]
 zbeginZ    lda  zEndZ    cmp 0= ?]









\ *** Block No. 41, Hexblock 29

\ ST:..stRead                 clv05aug87

 zTmp lda ZCheckSumB cmp 0<> ?[
Label SError zStatus lda  $60 # ora
         zStatus sta loadError jmp ]?
 $08 # ldy
    [[ BitRead Jsr dey 0= ?]
 zCheckSumB 1+ cmp        SError bne
 0 # cpx  0<> ?[ $10 # lda zStatus sta
                       verError jmp ]?
Label ldRTS rts end-code















\ *** Block No. 42, Hexblock 2a

\ ST:stLoad..                 clv23jul87

Label stLoad
 zVerifyFlag sta  0 # lda zStatus sta
 zDeviceNr  lda  cDeviceST # cmp 0<>
 ?[ xLoad jmp ]? \ CBM-Routine
Label loadNext
 tsx  zTmpSP stx
 xPressplay jsr ldRTS bcs
 sei     zVerifyFlag lda pha
 0 # lda zVerifyFlag sta
 xSearching jsr
Label ldWrongFile
 xCassMotorOn jsr delayMotor jsr
 xCassPrtOn   jsr puffInit   jsr
 clsl # lda   sTime sta
 cHeaderMark # lda  stRead jsr
 $63 # ldy  xFoundFile jsr
 0 # ldy [[ sCassBuffer ,Y lda
    kOutput jsr  iny  $10 # cpy  0= ?]
 $ff # ldy





\ *** Block No. 43, Hexblock 2b

\ ST:..stLoad                 clv23jul87


Label ldComp
 [[ iny zFileNameC cpy 0<>
  ?[[ pRamOn sta zFilenameZ )Y lda
  pRamOff sta
  sCassBuffer ,Y cmp         ldComp beq
  Ascii ?      # cmp         ldComp beq
  sCassBuffer $10 + lda  $02 # and 0<>
  ?[ $80 # lda zStatus sta eot jmp ]?
  xCassPrtOff jsr       ldWrongFile jmp
 ]]? pla  zVerifyFlag sta
 xMsgLoadVerify jsr
 zBasLoadAdd    lda  zBeginZ    sta
 zBasLoadAdd 1+ lda  zBeginZ 1+ sta
 zSecAdd lda 0<>
 ?[ sCassBuffer $11 + lda zBeginZ   sta
   sCassBuffer $12 + lda zBeginZ 1+ sta
 ]?
 clc sCassBuffer $13 + lda
 zBeginZ    adc zEndZ    sta
 sCassBuffer $14 +  lda
 zBeginZ 1+ adc zEndZ 1+ sta


\ *** Block No. 44, Hexblock 2c

\ ST:..stLoad                 clv14oct87


 chsl # lda    sTime sta
 sCassBuffer $10 + lda 0>=
 ?[ clsl # lda sTime sta ]?
 pRamOn sta  cDataMark # lda stRead jsr
 stEnd jmp end-code

Label loadsys \ load and start
 loadnext jsr CS ?[ brk ]?
 loadnext jsr CS ?[ brk ]?
 origin 8 - jmp \ Forth-Cold vector
Label btlEnd
base @ hex
Create g----    7 allot
loadsys 0 <# #s Ascii g hold #cr hold #>
g---- place
base !
: >lower ( str--) count bounds
 DO I c@ $7f and I c! LOOP ;

g---- >lower  forget >lower



\ *** Block No. 45, Hexblock 2d

\ ST:wByte w4bits             clv16jun87

Label wByte here 3 + Jsr \ write byte
Label w4bits             \ upper 4 Bits
 $04 # ldy
 [[ zByte lsr CS
    ?[ zReservAA 1+ lda  sTime sta ]?
    $10 # lda [[ pTimerBCtrl bit 0<> ?]
    timerBStart jsr
    pCass lda  $02 # eor pCass sta
    CC ?[ $10 # lda
       [[ pTimerBCtrl bit 0<> ?]
       timerBStart jsr
       pCass lda  $02 # eor pCass sta
    ][ zCheckSum    lda 0 # adc
       zCheckSum    sta
       zCheckSum 1+ lda 0 # adc
       zCheckSum 1+ sta
       zReservAA lda sTime sta ]?
    dey 0= ?] rts  end-code






\ *** Block No. 46, Hexblock 2e

\ ST:stWrite                  clv18jun87

Label stWrite \ writes a block
 pha cSyncByteCount # ldx
 [[ cSyncByte # lda  zByte sta
    wByte Jsr dex 0= ?]
 pla  zbyte sta  wByte Jsr
 0 # ldy zCheckSum sty zCheckSum 1+ sty
 [[ [[
  zBeginZ  )Y lda  zByte sta w4bits jsr
  zBeginZ  wInc              w4bits jsr
  zBeginZ     lda   zEndZ    cmp 0= ?]
  zBeginZ  1+ lda   zEndZ 1+ cmp 0= ?]
 zCheckSum lda  zCheckSum 1+ ldx
 zByte sta        wByte jsr
 txa   zByte sta  wByte jsr
 wByte jmp end-code









\ *** Block No. 47, Hexblock 2f

\ ST:saveName                 clv26jul87

Label saveName \ no error checking!
 bl # lda  $0f # ldy
 [[ sCassBuffer ,Y sta  dey 0= ?]
 zFileNameC ldy ram
 [[ dey 0>= ?[[ zFileNameZ )Y lda
    sCassBuffer ,Y sta ]]? rom
Label rsRTS rts end-code

















\ *** Block No. 48, Hexblock 30

\ ST:stSave..                 clv16jun87

Label stSave
 zDeviceNr lda cDeviceST # cmp 0<>
 ?[ sec $0e # and 0= ?[ clc ]?
                            xSave jmp ]?
 tsx  zTmpSP stx
 saveName jsr
 clc  xPressRec  jsr          rsRTS bcs
 sei  xCassPrtOn jsr xCassMotorOn jsr
 delayMotor jsr
 zSecAdd lda  sCassbuffer &16 + sta
 zIOStartZ    lda sCassBuffer &17 + sta
 zIOStartZ 1+ lda sCassBuffer &18 + sta
 sec zProgEndZ lda  zIOStartZ    sbc
 sCassBuffer &19 + sta
 zProgEndZ 1+ lda   zIOStartZ 1+ sbc
 sCassBuffer &20 + sta
 0 # lda sCassBuffer &21 +
 dup sta 1+ dup sta 1+ dup sta 1+ sta
 pTimerB 1+ sta
 sCassBuffer $100 u/mod
 # lda zBeginZ  1+ sta  zEndZ 1+ sta
 # lda zBeginZ     sta
 cCassBufferEnd # lda   zEndZ    sta

\ *** Block No. 49, Hexblock 31

\ ST:..stSave                 clv16jun87


 clssh # lda  zReservAA 1+ sta
 clssl # lda  zReservAA    sta
 pTimerB sta
 $10 # lda  pTimerBCtrl sta
 cHeaderMark # lda  stWrite jsr
 delayMotor jsr
 zSecAdd bit 0<
 ?[ chssh # lda  zReservAA 1+ sta
    chssl # lda  zReservAA    sta
    pTimerB sta ]?
 zIOStartZ     lda  zBeginZ     sta
 zIOStartZ  1+ lda  zBeginZ  1+ sta
 zProgEndZ     lda  zEndZ       sta
 zProgEndZ  1+ lda  zEndZ    1+ sta
 pRamOn sta cDataMark # lda stWrite jsr
 delayMotor jsr stEnd jmp end-code







\ *** Block No. 50, Hexblock 32

\ ST:supertape savebooter     clv10oct87

: supertape  \ --
 7 device !
 stLoad vLoad !  stSave vSave !
 ." ST2.20 " ;

| : (n" >in store n" ;

: btl ( --[from ]to )
 [ BtlBeg ] Literal [ BtlEnd ] Literal ;

| : btlName ( --adr count)
 pad $16 bl fill
 (n" $10 umin pad     swap cmove
 g---- count pad $a + swap cmove
 pad $10 ;

: stSavSys ( --)  \ Name" follows
  device store 1 device !
  btl btlName               bsave
  7 device !
  origin $17 - btl drop (n" bsave
  btl nip here           n" bsave ;


\ *** Block No. 51, Hexblock 33

\ Loadscreen for Decompiler    20oct87re
\ based on F83 by H. Laxen / M. Perry

\needs Tools  Vocabulary Tools

.( Decompiler loading...)

Onlyforth
Tools also definitions

\needs dis     ' drop | Alias dis
          \ Disassemble if possible

&1 &9 +thru

\\

clear








\ *** Block No. 52, Hexblock 34

\ case defining words        20aug85mawe

| : case:  ( n -)
 Create , 0 ]
 Does> 2+ swap 2* + perform ;

| : associative:
 Create ,   ( n -)
 Does>      ( n - index)
 dup @ -rot dup @ 0
   DO 2+ 2dup @ =
     IF 2drop drop I 0 0 LEAVE THEN
   LOOP 2drop ;


Defer (see
| Variable maxbranch
| Variable thenbranch








\ *** Block No. 53, Hexblock 35

\ decompile each type of word 29nov85re

| : .word   ( IP - IP')
 dup @ >name .name 2+ ;

| : .lit    ( IP - IP')
 .word dup @ . 2+ ;

| : .clit   ( IP - IP')
 .word dup c@ . 1+ ;

| : .string ( IP - IP')
 cr .word count 2dup type ascii " emit
 space + ;

| : .do  ( IP - IP')   ." DO " 4 + ;

| : .loop  ( IP - IP')   ." LOOP " 4 + ;

| : .exit    ( IP - IP' f)
 dup maxbranch @ u< IF  .word exit  THEN
 dup @ [ Forth ] ['] unnest =
 IF  ." ; "  ELSE  .word ." ; -2 allot "
 THEN  0= ;


\ *** Block No. 54, Hexblock 36

\ branch, ?branch             29nov85re

| : .to
 ." back to " .word drop ;

| : .branch ( IP - IP')
 2+ dup @ 2dup +  swap 0<
 IF   cr ." REPEAT to " .exit
   0<> swap 2+  and  exit
 THEN cr ." ELSE " dup thenbranch !
 dup maxbranch @ u>
 IF  maxbranch !  ELSE  drop  THEN 2+ ;

| : .?branch  ( IP - IP')
 2+ dup @ 2dup +
 swap 0<
 IF cr ." UNTIL " .to 2+ exit THEN
 cr dup 4 - @ [ ' branch ] literal =
 over 2-  @ 0< and
 IF   ." WHILE "
 ELSE ." IF "  dup thenbranch !
 THEN  dup maxbranch @ u>
    IF maxbranch ! ELSE drop THEN  2+ ;



\ *** Block No. 55, Hexblock 37

\ decompile does> ;code ;      20oct87re

| : does?    ( IP - IP' f)
 dup 3 + swap
 dup c@ $4C  =  swap      \ jmp-opcode
 1+ @  ['] Forth @ 1+ @ = \ (dodoes>
 and ;

| : .(;code  ( IP - IP' f)
  2+ does?
   IF cr ." Does> "
   ELSE  ." ;Code " 3 - dis 0 THEN ;

| : .compile  ( IP -- IP' )
  .word .word ;











\ *** Block No. 56, Hexblock 38

\ classify each word           20oct87re

&18 associative: execution-class
Forth
' lit ,     ' clit ,     ' ?branch ,
' branch ,  ' (DO ,      ' (." ,
' (abort" , ' Does> 4 + @ , \ (;code
' exit ,    ' abort ,    ' quit ,
' 'quit ,   ' (quit ,    ' unnest ,
' (" ,      ' (?DO ,     ' (LOOP ,
' compile ,

&19 case: .execution-class
.lit        .clit        .?branch
.branch     .do          .string
.string     .(;code
.exit       .exit        .exit
.exit       .exit        .exit
.string     .do          .loop
.compile    .word    ;






\ *** Block No. 57, Hexblock 39

\ decompile a :-definition   20aug85mawe

: .pfa  ( cfa -)
 >body
 BEGIN ?cr dup
   dup thenbranch @ =
   IF ." THEN " ?cr THEN
   @ execution-class .execution-class
   dup 0= stop? or UNTIL
 drop ;

: .immediate  ( cfa -)
 >name c@ dup
 ?cr $40 and IF ." Immediate " THEN
 ?cr $80 and IF ." restrict" THEN ;

: .constant   ( cfa -)
 dup >body @ . ." Constant "
 >name .name ;

: .variable   ( cfa -)
 dup >body . ." Variable "
 dup >name .name
 cr ." Value = "  >body @ . ;


\ *** Block No. 58, Hexblock 3a

\ display category of word     20oct87re

: .:      ( cfa -)
 ." : " dup >name .name cr .pfa ;

: .does>  ( cfa -)
 cr ." Does> " 2- .pfa ;

: .user-variable  ( cfa -)
 dup >body c@ . ." User-Variable "
 dup >name .name
 cr ." Value = "  execute @ . ;

: .defer  ( cfa -)
 ." deferred " dup >name .name
 ." Is " >body @ (see ;

: .other  ( cfa -)
 dup >name .name
 dup @ over >body =
  IF ." is Code" @ dis exit THEN
 dup @ does? IF .does> drop exit THEN
 drop ." maybe Code" @ dis ;



\ *** Block No. 59, Hexblock 3b

\ Classify a word              22jul85we

5 associative: definition-class
' quit @ ,        ' 0 @ ,
' scr @ ,         ' base @ ,
' 'cold @ ,

6 case: .definition-class
.:                .constant
.variable         .user-variable
.defer            .other  ;















\ *** Block No. 60, Hexblock 3c

\ Top level of Decompiler    20aug85mawe

: ((see    ( cfa -)
 maxbranch off  thenbranch off
 cr dup dup @
 definition-class .definition-class
 .immediate ;

' ((see Is (see

Forth definitions

: see   ' (see ;













\ *** Block No. 61, Hexblock 3d

\ Commodore hole Screens       20oct87re

Onlyforth

: <init   0 $DD03 c! ;

: get  ( -- 8b)
 BEGIN  $DD0D c@ $10 and  UNTIL
 $DD01 c@ dup $DD01 c! ;

: <sync  ( --)
 <init  BEGIN  get $55 =  UNTIL
 BEGIN  get dup $55 =
 WHILE  drop  REPEAT  abort" SyncErr" ;

: sum  ( oldsum n -- newsum n)
 swap over + swap ;

: check  ( sum.int  8b.sum.read --)
 swap $FF and - abort" ChSumError" ;

-->




\ *** Block No. 62, Hexblock 3e

\ Commodore hole Screens       20oct87re

: download  ( n --)
 <sync  0 swap buffer b/blk bounds
 DO  get  sum  I c!  LOOP
 get check update ;

: downthru  ( start count --)
 bounds DO  I download  LOOP ;

-->

\\ sync needs: xx $55 $55 00 data













\ *** Block No. 63, Hexblock 3f

\ Commodore sendscreens        20oct87re

: >init  $FF $DD03 c! ;

: put ( 8b -)
 $DD01 c!  BEGIN  stop?
  IF  <init true abort" terminated" THEN
  $DD0D c@ $10 and  UNTIL ;

: >sync  ( --)
 >init $10 0 DO $55 put  LOOP  0 put ;

: upload  ( n --)
 >sync  0 swap  block b/blk bounds
 DO  I c@ sum  put  LOOP
 $FF and  put  <init  ;

: upthru  ( from to -- )
 1+ swap DO  I . cr  I upload  LOOP ;







\ *** Block No. 64, Hexblock 40

\ Graphic Load-Screen          20oct87re

(16 .( C64 Only ) \\ C)

Onlyforth

\needs Code         .( Assembler??!) \\

\needs lbyte         1 +load

\needs 100u/       &26 +load

Vocabulary graphic

' graphic | Alias Graphics

Graphics also definitions

  2 &15 +thru  \ hires graphic
&16 &20 +thru  \ sprites
&21 &23 +thru  \ turtle graphic

Onlyforth



\ *** Block No. 65, Hexblock 41

\ >byte hbyte lbyte            20oct87re

Code >byte   ( 16b - 8bl 8bh)
 SP )Y lda pha txa SP )Y sta SP 2dec
 txa SP )Y sta pla Puta jmp   end-code

: hbyte >byte nip ;
: lbyte >byte drop ;


















\ *** Block No. 66, Hexblock 42

\ Graphics constants           20oct87re

| $0288 Constant scrpage
| $E000 Constant bitmap
| $D800 Constant charset
| $C400 Constant colram
| $C000 Constant vidram
\ $C800 Constant sprbuf

   bitmap hbyte $40 /mod  3 swap -
| Constant bank
   $20 / 8 *
   colram  hbyte $3F and 4 / $10 * or
| Constant bmoffs
   vidram  hbyte $3F and 4 / $10 *
   charset hbyte $3F and 4 / or
| Constant tmoffs

$0314 | Constant irqvec
$EA31 >label oldirq
$EA81 >label irqend
$FF6E >label settimer




\ *** Block No. 67, Hexblock 43

\ Gr movecharset clrscreen     20oct87re

| Code movecharset
 sei  $32 # lda  1 sta   dey  8 # ldx
 N sty  N 2+ sty  $D8 # lda  N 1+ sta
 charset hbyte # lda  N 3 + sta
  [[
   [[  N )Y lda  N 2+ )Y sta  iny  0= ?]
  N 1+ inc  N 3 + inc  dex  0= ?]
 $36 # lda   1 sta  cli  iny
 Next jmp  end-code


: clrscreen ( -- ) bitmap &8000 erase ;












\ *** Block No. 68, Hexblock 44

\ Gr Variables (text (hires    20oct87re

| Variable cbmkey
| Variable switchflag
| Variable textborder
| Variable hiresborder
| Variable switchline
| Variable chflag

Label (text
 $1B # lda  $D011 sta
 tmoffs # lda  $D018 sta
 textborder lda  $D020 sta  rts

Label (hires
 $3B # lda  $D011 sta
 bmoffs # lda  $D018 sta
 hiresborder lda  $D020 sta  rts








\ *** Block No. 69, Hexblock 45

\ Gr rasterirq graphicirq      20oct87re

Label windowhome
 switchline lda sec $30 # sbc
 .A lsr .A lsr .A lsr  sec 1 # sbc
  $D6 cmp CC ?[ rts ]?
  tax inx 2 # ldy $CC sty $CD sty
  $CE lda $D3 ldy $D1 )Y sta
  0 # ldy $CF sty clc $FFF0 jsr
  0 # ldy $D1 )y lda $CE sta $CC sty
  rts

Label graphicirq
$28D lda  2 # and 0= ?[ oldirq jmp ]?
[[ $FF9F jsr $28D lda 0= ?] cbmkey ) jmp

Label rasterirq
 $D019 lda $D019 sta
 $15 # ldx [[ dex 0= ?] N lda ( Blind!!)
 chflag lda  1 # eor chflag sta tax
 0= ?[ (hires jsr ][ (text jsr ]?
 switchline ,x lda $D012 sta
 windowhome jsr
 $DC0D lda  1 # and graphicirq bne
 irqend jmp

\ *** Block No. 70, Hexblock 46

\ Gr IRQ-handling   (window    20oct87re

Label setirq
 sei graphicirq >byte
 # lda irqvec 1+ sta    # lda irqvec sta
 $F0 # lda $D01A sta $81 # lda $DC0D sta
 cli rts

| Code resetirq
 sei oldirq >byte
 # lda irqvec 1+ sta    # lda irqvec sta
 $F0 # lda $D01A sta $81 # lda $DC0D sta
 cli Next jmp end-code

Label (window
 rasterirq >byte
 # lda irqvec 1+ sta  # lda irqvec sta
 $7F # lda $DC0D sta $F1 # lda $D01A sta
 switchflag stx chflag stx
 windowhome jmp






\ *** Block No. 71, Hexblock 47

\ Gr text hires window switch  20oct87re

Code text    1 # lda switchflag sta
 setirq jsr  (text jsr  Next jmp
 end-code

Code hires   2 # lda switchflag sta
 setirq jsr  (hires jsr  Next jmp
 end-code

| Code setwindow  ( row -)
 sei (window jsr cli xyNext jmp
 end-code

: window   ( row -)
 8 * $30 + switchline c! setwindow ;

Label switch    switch cbmkey !
 switchflag ldx
         0= ?[ inx switchflag stx
     setirq jsr (text  jsr oldirq jmp ]?
 1 # cpx 0= ?[ inx switchflag stx
     setirq jsr (hires jsr oldirq jmp ]?
 0 # ldx  switchflag stx
 (window jsr  oldirq jmp  end-code

\ *** Block No. 72, Hexblock 48

\ Gr graphic forth             20oct87re

Forth definitions

: graphic
 Graphics movecharset
 $DD00 c@  $FC and  bank or  $DD00 c!
 vidram hbyte scrpage c!
 colram c@ hiresborder c!
 $D020 c@ textborder c!
 $10D0 switchline !
 text ;

: nographic
 Onlyforth resetirq
 $1B $D011 c!  $17 $D018 c! 4 scrpage c!
 textborder c@   $D020 c!
 $DD00 c@  3 or  $DD00 c! ;

Graphics definitions






\ *** Block No. 73, Hexblock 49

\ Gr Colors                    20oct87re

0  Constant blk      1 Constant wht
2  Constant red      3 Constant cyn
4  Constant pur      5 Constant grn
6  Constant blu      7 Constant yel
8  Constant ora      9 Constant brn
$A Constant lre     $B Constant gr1
$C Constant gr2     $D Constant lgr
$E Constant lbl     $F Constant gr3

: border      ( color -)
 dup textborder c! $D020 c! ;

: screen      ( color -)   $D021 c! ;

: colors      ( bkgrnd  foregrnd -)
 over hiresborder c!
 $10 *  or  colram $03F8  rot fill ;

: background  ( color -)
 colram c@ $10 /  colors ;

: pencolor    ( color -)
 colram c@ $F and swap  colors ;

\ *** Block No. 74, Hexblock 4a

\ Gr Bittab Labels             20oct87re

Label bittab
 $80 c, $40 c, $20 c, $10 c,
 $08 c, $04 c, $02 c, $01 c,

| : >laballot  ( adr n - adr+n)
 over >label + ;

$60 Constant pointy  $62 Constant pointx

Assembler

N
 2 >laballot y0      2 >laballot x0
 2 >laballot y1      2 >laballot x1
 2 >laballot offset  2 >laballot dy
 2 >laballot dx      2 >laballot ct
 1 >laballot iy      1 >laballot ix
 1 >laballot ay      1 >laballot ax
 2 >laballot bytnr
drop




\ *** Block No. 75, Hexblock 4b

\ Gr (plot compute             20oct87re

Label (plot  ( x y -)
 2 # lda  setup jsr      3 # ldx
 [[ y0 ,X lda  pointy ,X sta  dex  0< ?]
 $C7 # lda sec y0 sbc y0 sta
Label compute  sei  1 dec
 y0 lda  $F8 # and  pha
 bytnr sta  0 # lda  bytnr 1+ sta  clc
 bytnr asl  bytnr 1+ rol
 bytnr asl  bytnr 1+ rol
 pla  bytnr adc  bytnr sta
   CS ?[  bytnr 1+ inc  ]?
 bytnr asl  bytnr 1+ rol
 bytnr asl  bytnr 1+ rol
 bytnr asl  bytnr 1+ rol
 y0 lda  7 # and  bytnr ora  bytnr sta

 clc  x0 lda  $F8 # and  bytnr adc
 bytnr sta
   x0 1+ lda  bytnr 1+ adc  bytnr 1+ sta
 bitmap hbyte # lda
 bytnr 1+ ora  bytnr 1+ sta
 x0 lda  7 # and  tax  bittab ,X lda
 0 # ldy  clc rts

\ *** Block No. 76, Hexblock 4c

\ Gr plot flip clpx            20oct87re

Code plot  ( x y -)
 (plot jsr
 bytnr 1+ ldx bitmap hbyte # cpx
 cs ?[ bytnr )Y ora  bytnr )Y sta ]?
Label  romon
 1 inc  cli  xyNext jmp  end-code

Code flip  ( x y -)
 (plot jsr
 bytnr 1+ ldx bitmap hbyte # cpx
 cs ?[ bytnr )Y eor  bytnr )Y sta ]?
 romon jmp  end-code

Code unplot ( x y -)
 (plot jsr
 bytnr 1+ ldx bitmap hbyte # cpx cs ?[
 $FF # eor  bytnr )Y and bytnr )Y sta ]?
 romon jmp  end-code

\\ compute disables IRQ, the words
plot, flip, unplot and line enable the
IRQ again. Not nice, but the only was
because of the branch in 'line'.

\ *** Block No. 77, Hexblock 4d

\ Gr line 1                    20oct87re

Code line  ( x1 y1 x0 y0 -)
 4 # lda  setup jsr
Label (drawto
 3 # ldx
 [[ y0 ,X lda  pointy ,X sta  dex  0< ?]
 $C7 # lda sec y1 sbc y1 sta
 $C7 # lda sec y0 sbc y0 sta

 ix sty  iy sty  ct sty  dey
 ax sty  ay sty  ct 1+ sty  dey
 x1 lda  x0 cmp  x1 1+ lda  x0 1+ sbc
 CC ?[  sec  x0 lda  x1 sbc  dx sta
        x0 1+ lda  x1 1+ sbc  dx 1+ sta
        ix sty
    ][  x1 lda  x0 sbc  dx sta
        x1 1+ lda  x0 1+ sbc  dx 1+ sta
    ]?  y1 lda  y0 cmp
 CC ?[  sec  y0 lda  y1 sbc  dy sta
        iy sty
    ][               y0 sbc  dy sta
    ]?  dx 1+ lda



\ *** Block No. 78, Hexblock 4e

\ Gr line 2                    20oct87re

 0= ?[  dx lda  dy cmp
  CC ?[  dy ldx  dy sta  dx stx
         ix lda  ay sta  iy lda  ax sta
         iny  ix sty  iy sty  ]?  ]?
 dx 1+ lda  .A lsr  offset 1+ sta
 dx lda     .A ror  offset sta
sec  CC ?[  .( Trick!! )
 [[ ix lda
    0<> ?[ 0>= ?[  x0 winc
               ][  x0 wdec  ]?  ]?
    clc  y0 lda  ax adc  y0 sta
    clc  offset lda  dy adc  offset sta
    CS ?[  offset 1+ inc  ]?   ct winc
    dx lda  offset cmp  dx 1+ lda
    offset 1+ sbc
    CC ?[  sec  offset lda  dx sbc
           offset sta  offset 1+ lda
           dx 1+ sbc  offset 1+ sta
           ay lda
           0<> ?[  0>= ?[  x0 winc
                       ][  x0 wdec ]? ]?
           clc  y0 lda  iy adc  y0 sta
    ]?

\ *** Block No. 79, Hexblock 4f

\ Gr line 3 flipline           20oct87re

 swap  ]?  .( Part 2 of trick! )
 compute jsr
 bytnr 1+ ldx bitmap hbyte # cpx cs ?[
Label mode
 bytnr )Y ora  bytnr )Y sta ]?
 1 inc  cli
    dx lda  ct cmp
    dx 1+ lda  ct 1+ sbc  CC ?]
 xyNext jmp  end-code

Code drawto  ( x1 y1 -)
 3 # ldy
 [[ pointy ,Y lda  y1 ,Y sta  dey  0< ?]
 2 # lda setup jsr  (drawto jmp
end-code

: flipline   ( x1 y1 x0 y0 -)
 $51 ( eor )  mode c! line
 $11 ( ora )  mode c! ;

\ bad self-modifying code



\ *** Block No. 80, Hexblock 50

\ Sprite constants             20oct87re

  $C800 Constant sprbuf
| $D000 Constant sprbase
| $D010 Constant xposhi
  $D015 Constant sprite
| $D017 Constant yexpand
  $D01C Constant 3colored
| $D01D Constant xexpand
| $D025 Constant sprmcol
| $D027 Constant sprcol

| Create sbittab
 $01 c, $02 c, $04 c, $08 c,
 $10 c, $20 c, $40 c, $80 c,











\ *** Block No. 81, Hexblock 51

\ Spr setbit set formsprite    20oct87re

| Code setbit   ( bitnr adr fl  -)
 3 # lda  setup jsr  dey
 N 4 + ldx  sbittab ,X lda
 N ldx
 0= ?[ $FF # eor  N 2+ )Y and
    ][  N 2+ )Y ora  ]?
 N 2+ )Y sta  xyNext jmp  end-code

: set   ( bitnr adr  -)   True  setbit ;

: reset ( bitnr adr  -)   False setbit ;

: getform  ( adr mem#  -)
 $40 * sprbuf + $40 cmove ;

| : sprite!   ( mem# spr# adr -)
 $3F8 + + c! ;

: formsprite  ( mem# spr# -)
 >r sprbuf $3F00 and $40 / + dup
 r@ vidram sprite!  r> colram sprite! ;



\ *** Block No. 82, Hexblock 52

\ Spr move sprpos              20oct87re

: xmove   ( x spr#  -)
 2dup 2* sprbase + c!
 xposhi rot $FF > setbit ;

: ymove   ( y spr#  -)
 2* 1+ sprbase + c! ;

: move    ( y x spr#  -)
 under xmove ymove ;


: sprpos  ( spr# - y x)
 dup >r 2* 1+ sprbase + c@
 r@ 2* sprbase + c@
 r> sbittab + c@ xposhi c@ and
 IF $100 + THEN ;








\ *** Block No. 83, Hexblock 53

\ Sprite Qualities             20oct87re

: high    ( spr# -)   yexpand set ;

: low     ( spr# -)   yexpand reset ;

: wide    ( spr# -)   xexpand set ;

: slim    ( spr# -)   xexpand reset ;

: big     ( spr# -)   dup high wide ;

: small   ( spr# -)   dup low slim ;

: behind  ( spr# -)   $D01B set ;

: infront ( spr# -)   $D01B reset ;

: colored ( spr# col  -)
 swap sprcol + c! ;






\ *** Block No. 84, Hexblock 54

\ Spr sprcolors setsprite      20oct87re

: sprcolors  ( col# col#  -)
 sprmcol 1+ c! sprmcol c! ;

: setsprite  ( mem# y x color spr# -)
 under >r colored   r@ move
 r@ under formsprite small
 r@ 3colored reset  r> sprite set ;

















\ *** Block No. 85, Hexblock 55

\ Tu heading left right        20oct87re

| Variable xpos     | Variable ypos
| Variable deg      | Variable pen

| : 100*/  ( n1 n2 n3 - n4)  &100 */ ;

: heading     ( - deg)    deg @ ;
: setheading  ( deg -)    deg ! ;

: right  ( deg -)
 deg @ swap - &360 mod deg ! ;

: left   ( deg -)
 deg @ + &360 mod deg ! ;


' clrscreen  Alias cs
' pencolor   Alias pc
' background Alias bg
' hires      Alias fullscreen
' window     Alias splitscreen




\ *** Block No. 86, Hexblock 56

\ Tu positions pen home        20oct87re

: xcor    ( - x)     xpos @ 100u/ ;
: ycor    ( - y)     ypos @ 100u/ ;

: setx    ( x -)     100* xpos ! ;
: sety    ( y -)     100* ypos ! ;
: setxy   ( x y -)   sety setx ;

: pendown  pen on ;
: penup    pen off ;

: home
 &160 &96 setxy &90 setheading pendown ;

: draw     clrscreen home &20 window ;
: nodraw   text page ;


' left       Alias lt
' right      Alias rt
' setheading Alias seth
' pendown    Alias pd
' penup      Alias pu


\ *** Block No. 87, Hexblock 57

\ Tu forward back              20oct87re

: tline   ( x1 y1 x2 y2 -)
 >r >r >r  100u/  r> 100u/
        r> 100u/  r> 100u/  line ;

: forward  ( distance -)
 >r xpos @ ypos @
 over deg @ cos r@ 100*/ + dup xpos !
 over deg @ sin r> 100*/ + dup ypos !
 pen @ IF tline ELSE 2drop 2drop THEN ;

: back     ( distance -)
 negate forward ;

: turtlestate ( - pen bg pc)
 pen c@ colram c@ dup
 &15 and swap &16 / ;

' forward     Alias fd
' back        Alias bk
' turtlestate Alias ts




\ *** Block No. 88, Hexblock 58

\ Gr arc ellipse circle        20oct87re

: arc     ( hr vr strt end -)
 >r >r 2dup max &360 swap /
 r> 2* 2* r> 1+ 2* 2* swap rot >r
  DO over I 2/ 2/ cos &10005 */
     over I 2/ 2/ sin &10005 */
     plot
  r@ +LOOP
 r> 2drop drop ;

: ellipse ( x y hr vr -)
 2swap c-y ! c-x ! m-flag on
 0 &90 arc m-flag off ;

: circle  ( x y r -)
 dup 3 4 */ ellipse ;









\ *** Block No. 89, Hexblock 59



























\ *** Block No. 90, Hexblock 5a

\ Math Load-Screen            20oct87re

Onlyforth

base @  decimal

   1  2 +thru  \ Trigonometry
   3  4 +thru  \ roots
   5  6 +thru  \ 100* 100u/

base !















\ *** Block No. 91, Hexblock 5b

\ Ma sinus-table               20oct87re
\    Sinus-Table from FD Vol IV/1

| : table    ( values n -)
 Create 0 DO , LOOP
 ;code       ( n - value)
 SP X) lda  clc  1 # adc  .A asl  tay
 W )Y lda  SP X) sta
 iny  W )Y lda  1 # ldy  SP )Y sta
 Next jmp  end-code

10000 9998 9994 9986 9976 9962 9945 9925
 9903 9877 9848 9816 9781 9744 9703 9659
 9613 9563 9511 9455 9397 9336 9272 9205
 9135 9063 8988 8910 8829 8746 8660 8572
 8480 8387 8290 8192 8090 7986 7880 7771
 7660 7547 7431 7314 7193 7071 6947 6820
 6691 6561 6428 6293 6157 6018 5878 5736
 5592 5446 5299 5150 5000 4848 4695 4540
 4384 4226 4067 3907 3746 3584 3420 3256
 3090 2924 2756 2588 2419 2250 2079 1908
 1736 1564 1392 1219 1045 0872 0698 0523
 0349 0175 0000

&91 | table sintable

\ *** Block No. 92, Hexblock 5c

\ Ma sin, cos, tan             20oct87re

| : s180   ( deg -- sin*10000:sin 0-180)
 dup &90 >
   IF &180 swap - THEN
 sintable ;

: sin     ( deg -- sin*10000)
 &360 mod dup 0< IF &360 + THEN
 dup &180 >
    IF &180 - s180 negate
    ELSE s180 THEN ;

: cos     ( deg -- cos*10000)
 &360 mod &90 + sin ;

: tan     ( deg -- tan*10000)
 dup sin swap cos ?dup
   IF &100 swap */ ELSE 3 * THEN ;







\ *** Block No. 93, Hexblock 5d

\ Ma sqrt 1                    20oct87re

Code d2*  ( d1 - d2)
 2 # lda setup jsr
 N 2+ asl N 3 + rol  N rol N 1+ rol
 SP 2dec N 3 + lda SP )y sta
 N 2+ lda SP x) sta
 SP 2dec N 1+ lda SP )y sta
 N lda SP x) sta
 Next jmp end-code

: du< &32768 + rot &32768 + rot rot d< ;
| : easy-bits  ( n1 -- n2)
 0 DO
  >r d2* d2*  r@ -  dup 0<
    IF   r@ +   r> 2* 1-
    ELSE        r> 2* 3 +
    THEN LOOP ;

| : 2's-bit
 >r d2* dup 0<
  IF    d2* r@ - r> 1+
  ELSE  d2* r@ 2dup u<
   IF drop r> 1-  ELSE -  r> 1+  THEN
  THEN ;

\ *** Block No. 94, Hexblock 5e

\ Ma sqrt 2                    20oct87re

| : 1's-bit
 >r dup 0<
  IF 2drop r> 1+
  ELSE d2* &32768 r@  du< 0=
    negate R> +
  THEN ;

: sqrt    ( ud1 - u2)
 0 1  8 easy-bits
 rot drop 6 easy-bits
 2's-bit 1's-bit ;

\\

: xx
 &16 * &62500 um*
 sqrt 0 <# # # # ascii . hold #s #>
 type space ;






\ *** Block No. 95, Hexblock 5f

\ 100*                         20oct87re

Code 100*  ( n1 - n2)
 SP X) lda  N sta  SP )Y lda  N 1+ sta
 N asl N 1+ rol  N asl N 1+ rol

 N lda N 2+ sta  N 1+ lda N 3 + sta

 N 2+ asl N 3 + rol  N 2+ asl N 3 + rol
 N 2+ asl N 3 + rol

 clc N lda N  2+ adc N sta
  N 1+ lda N 3 + adc N 1+ sta

 N 2+  asl N 3 + rol

 clc N lda N  2+ adc  SP X) sta
  N 1+ lda N 3 + adc  SP )Y sta

 Next jmp end-code






\ *** Block No. 96, Hexblock 60

\ 100/                         20oct87re

Label 4/+
 N 7 + lsr N 6 + ror N 5 + ror N 4 + ror
 N 7 + lsr N 6 + ror N 5 + ror N 4 + ror
 clc N  lda N 4 + adc N     sta
  N 1+  lda N 5 + adc N 1+  sta
  SP X) lda N 6 + adc SP X) sta
  SP )Y lda N 7 + adc SP )Y sta  rts

Code  100u/  ( u - n)
 N stx  N 4 + stx
 SP X) lda  .A asl N 1+  sta  N 5 + sta
 SP )Y lda  .A rol SP X) sta  N 6 + sta
 txa .A rol        SP )Y sta  N 7 + sta
 4/+ jsr
 N 7 + lsr N 6 + ror N 5 + ror N 4 + ror
 4/+ jsr
 Next jmp end-code







\ *** Block No. 97, Hexblock 61



























\ *** Block No. 98, Hexblock 62

\\ for csave cload            clv10oct87


The Assembler must be loaded


set Labels


 Save parameter starting at N
 Enable SysMessages    Status to 0
 (set drv for derror?
 Device#, Sec.Address, File#
 Address-of-Filename Length
 Address in XY


 One of 8 Kernalerrors?
 check Status/destroy EOI-Bit
 send both back as Error Number






\ *** Block No. 99, Hexblock 63

\\ for ..csave cload          clv10oct87


 prepare Parameter     (XR=to+1)
 Pointer to from in AR and BSAVE
 Error?


 prepare Parametr      (XR=from)
 Load (no Verify) BLOAD
 to+1 will be given back
 place on the Forth Stack
 Error?



Errorsources for CBM-Routinen:
(1) Kernal-Result
(2) Status-Register
(3) Disk-Errorchannel






\ *** Block No. 100, Hexblock 64

\\ for ..csave cload Luxus    clv10oct87



This routine is using the BASIC
Basic-Errormessages, so that the
messages doesn need to be defined
again. This is using the BASIC ROM.
The BASIC Rom should only be used if
no Site Effects occur, which is the
case here.





creates an Errormessage from Error-
number
If not "device not presen"
if is querying the serial bus for
device error message





\ *** Block No. 101, Hexblock 65

\\ for TapeVersion            clv01aug87

The Tapeversion was developed for C16
with 64kB, but also works on the C64


It conists of 3 parts
   A virtual floppy in memory (Ramdisk)
   An Interface to the external Device
     Tape Recorder
   Supertape loader
     (only for C16)




Initializing:
 init Supertape if possible
 redefine and activate R/W
 if AUTOLOAD enabled, load Ramdisk






\ *** Block No. 102, Hexblock 66



























\ *** Block No. 103, Hexblock 67



























\ *** Block No. 104, Hexblock 68



























\ *** Block No. 105, Hexblock 69



























\ *** Block No. 106, Hexblock 6a

\\ for RD: loadscreen         clv05aug87

This Ramdisk is using a compressed
format

To allow switching of ramdisks, the
code contains one variable (RD that
contains a pointer to the ramdisk.
All other variables are stored in the
Ramdisk Memory area


Binaerblocks must be marked with BINARY
this Ramdisk support all Block Forth-
Words that use R/W











\ *** Block No. 107, Hexblock 6b

\\ For RD:                    clv01aug87

\ All Pointers are offsets from First

rd ==0 ==>   no Ramdisk

rd -->Length of Parametrblock
+2 -->current Block
+4 -->End of last Block+1
+6 -->End of Ramdisk-Area+1
+8 -->Number of current Block
+16-->Name
End of Parameterblock
     1.RD-Block
     2.RD-Block
       .
     0000

adr-->current RD-Block (absolute Addr)
   -->Length (incl. 4 bytes RD-Data)
 2+-->Blocknumber
 2+-->..Data..




\ *** Block No. 108, Hexblock 6c

\\ for RD:                    clv01aug87

NEW checks for enough space and
    set current block to free space



LEN! stores the length of new block
    and patches END
    If length is=0 nothing happends
    Creates 0000 at the End of Ramdisk

LEN@ returns length of current Block
    If not available, returns 0


DEL deletes current block and patches
    END








\ *** Block No. 109, Hexblock 6d

\\ for RD:                    clv01aug87

SEARCH set current block to searched
  Block, if not found, to END
  Blocknumber will be stored in BLK#













BINARY disables compression of Block
  for example for Binary-Data
  A binary block will be detected if
  length is $400




\ *** Block No. 110, Hexblock 6e

\\ for RD: c>7 7 >c           clv01aug87

Convert CBM-Chars to 7bit
All chars $c0..$e0 will be $60..80
All other >=$80 will be $00..20





















\ *** Block No. 111, Hexblock 6f

\\ for RD: cp1 cp2            clv01aug87

Start routine for COMPRESS & EXPAND






Endroutine for COMPRESS & EXPAND
















\ *** Block No. 112, Hexblock 70

\\ for RD: expand compress    clv01aug87

























\ *** Block No. 113, Hexblock 71

\\ for RD:ramR/W              clv01aug87

ENDWRITE removes Blanks at end of Block
   and set LEN!



ENDREAD fills Reminder of block with
        Blank

RAMR/W  replaces the R/W-Routine
  (binary) Blocks in full length will
  copied by CMOVE, shorter blocks will
  be copied with COMPRESS (write) and
  EXPAND (read).











\ *** Block No. 114, Hexblock 72

\\ for RD:id rduse..          clv01aug87

.RD  print information about current RD


ID!  set name of Ramdisk




ID"  reads name of Ramdisk

RDUSE switches (without checks) to RD
RDDEL clears Ramdisk



RDNEW creates a new Ramdisk and
      checks (almost) everything







\ *** Block No. 115, Hexblock 73

\\ for RD: rdcheck            clv01aug87



RDCHECK checks pointer of Ramdisk and
        prints table of contents




















\ *** Block No. 116, Hexblock 74



























\ *** Block No. 117, Hexblock 75

\\ for ST:LoadScreen          clv01aug87

Supertape is a fast loader using
3600 Bd or 7200 Bd approx. 10 times
fster then the original CBM-Routines

 Usage:
  DeviceNumber  =  7 ==> Supertape
  SecAddress   >=$80 ==> 7200 Bd
                <$80 ==> 3600 Bd
                ..everything else like
                  CBM
StorageFormat 8Bit per Byte, Lowbit 1st
Selfregulating, on each Bitborder is
a edge-switch
If there is anotherone in the middle
the bit is  Bit=0, else=1.

Format: sync #$2a 25b:Header 2b:checksum
        sync #$c5 len:Data   2b:checksum
Sync  = 64b:#$16
Header=16b:Name
       1b:SecAdd 2b:from 2b:len 4b:#$00



\ *** Block No. 118, Hexblock 76

\\ for ST:Labels.             clv16jun87

-------- hardware-Addresses-----------
1  Cassettenport
2  Time for Timer2
1  controllregister for Timer2
1  Writeaccess switches to RAM
1  Writeaccess switches to ROM

-------- System-Vectors  -------------
2  Save-Vector of System will be patched
2  Load-Vector of System will be patched

----- Input-Parameter Load/Save-----
1  Device-Number
1  Secundaryaddress (controls Device)
2  Pointer to filenames
1  Number of Chars in Filename
2  Startaddress for LOAD
2  Startaddress for SAVE
2  Endaddress+1 for SAVE

----- Outpute-Parameter for Load/Save --
1  Status Flags of OS


\ *** Block No. 119, Hexblock 77

\\ zu ST:..Labels             clv16jun87

-------- benutzte System-Routinen -----
   start Cassette Motor
   stop  Cassette Motor
   init Cassette Port
   init Cassette Port
   normal  Load-Routine
   normal  Save-Routine
   print 'Loading' or 'Verifying'
   print 'Press play..'
   print 'Press Record.. '
   print 'Found'
   print 'Searching'
   print one char

-------- used Zeropage-Addresses -----

2  Address of current I/O  Byte
2  Address of last    I/O  Byte +1
1  next Block: Verify/-Load
1  next Block: Header/Data
1  last State of Cassetteport
1  already loaded part of current Byte
1  last loaded byte

\ *** Block No. 120, Hexblock 78

\\ for ST:..Labels            clv16jun87


2  short/long Impuls for Save
2  Checksum

1  Stackpointer for Error Exit


----- additional Systemaddresses-----
1  Time for next TimerBStart
c0 Buffer for Cassetteoperations
-  End of Buffer, Low-Byte

----------- Constants  --------------
   DeviceNumber of Supertape
   1.Byte of Headerblock
   1.Byte of Datablock
   Time 7600 Baud loading
   Time 3600 Baud loading
   Time 7600 Bd save long/short Impuls
   Time 7600 Bd save long/short Impuls
   Byte for Sync-Header
   min. Number of SyncBytes for Loading
   Number of SyncBytes for Saving

\ *** Block No. 121, Hexblock 79

\\ for ST:misc                clv28jul87

Start of Bootstraploader








 (1)
Start Timer Number 2
 (1)
with time in STIME




Wait-Loop

(1) the Sequence 'brk brk bit brk brk'
    stops overwriting data at boottime
    if a read-error occurs


\ *** Block No. 122, Hexblock 7a

\\ for ST:stEnd etc.         clv18jun87

                no     Error (Bit--)
                Load-     "  (Bit--)
 AR := ErrorNr  EOT -     "  (Bit--)
                Verify-   "  (Bit--)
                Stop-     "
 Switch to ROM, push Error
 Port exit
 repair Stack
 xr-yr := Load-EndAddress
 CF    := Error, enable Interrupt














\ *** Block No. 123, Hexblock 7b

\\ for ST:bitRead             clv16jun87


wait for timer                    (?)
Carry := 1 , if level equal == Bit=1
 save bit
 rotate in byte
if Bit=1: increment checksum
wait for edge
save portstate, set timer
return current byte















\ *** Block No. 124, Hexblock 7c

\\ zu ST:stRead..             clv28jul87

 Data/or Header,Verifyerror := 0


 syncronizing

 read Byte

 no Sync Byte? search for it

 Header detected
    read Byte
    until Header ends AR=Block-kind
 searched Block Kind? yes, read it
 searched for Header, data found, cont.
 othr Kund? Error
 Checksum := 0
 Read byte







\ *** Block No. 125, Hexblock 7d

\\ for ST:..stRead            clv28jul87

--- Loop from Load-Start till end
 Checksum
             := Checksum
 read 2 Bit
 only Verify?
 else: load Byte
 read 2 Bit
 compare Byte
 increment verify error
 read 2 bit
 pointer to next byte
 new byte
 new byte
--- end of loop
--- end of loop









\ *** Block No. 126, Hexblock 7e

\\ for ST:..stRead            c2v27jul87

 Checksum-Error?
   else Status
   and  LoadError-Exit
 read byte

 Checksum-Error?
 Verifyerror?

















\ *** Block No. 127, Hexblock 7f

\\ for ST:stLoad..            clv16jun87

will be load-vector of system
 save Verify and Load, clear status
 for Supertape?
 if not -> CBM-Routine
 save stack for error handling
 'Press play on Tape' Stop?,then return
 disable IRQ
 set to load
 'Searching...'

 Initializing

 3600 Baud/Load
 Search Header and Load
 'Found ..'
 print Filename








\ *** Block No. 128, Hexblock 80

\\ for ST:..stLoad            clv16jun87



 compare all chars

  same as in entred filename?
                           continue
  entered Char   '?'  ?    continue
  End-Of-Tape         ?
                           then NotFound
  else: enable Screen, cont. search
 repair Verify Flag
 'loading'/'verifying'
 LoadAddress  := from System

 SecAdd.=1?

   then loadaddress from header

 LoadEnd
         :=
             LoadAddress
            +FileLength


\ *** Block No. 129, Hexblock 81

\\ for ST:..stLoad            c2v27jul87


 7200 Baud/Load
 saved with 3600 Bd (==Secadd>$80)?
 then 3600 Bd/Load
 switch to RAM, load Datablock
 End

Will be used for Bootstraploader




Creates a string of Form 'g78b5',
with address LOADSYS
will be used as Monitor-Command,
with address of Bootstraploader
s.a. SAVEBOOTER

This String cannot contain capital
Letters




\ *** Block No. 130, Hexblock 82

\\ for ST:wByte w4bits        clv16jun87



 4 Bits
 --- Loop over 4 Bits
    bit=1?, set full timer
    wait for timer
    start new
    write edge
    bit=0?
       wait for timer
       and start new
       write edge (Bit-border)
    bit=1?
       increment Checksum


       set half time
 --- End of loop






\ *** Block No. 131, Hexblock 83

\\ for Rd:stWrite             clv18jun87

 AR=BlockKind
 save
 SynchronisationBytes
 ..write
 write BlockKind
 Checksum:= 0
 --- Loop for 1st till last Byte
                   upper 4 Bits
                   lower 4 Bits write
 --- Loop...
 --- ..End
 Checksum..
 ..write Low Byte
 ..write High Byte
 few Extrabits, ensures that loading
 will end








\ *** Block No. 132, Hexblock 84

\\ for ST:saveName            clv01aug87

writes FileName in Cassettebuffer
 CassetteBuffer  [0..$10]
              := <blanks>
 CassetteBuffer  [0..FileNameLength]
              :=
                 FileName


















\ *** Block No. 133, Hexblock 85

\\ for ST:stSave..            clv01aug87

will by pacthed into OS Vector
 DeviceNr = Supertape?
 else: use
    CBM-Save-Routine
 StackPointer saved for Errorhandling
 FileName in Buffer
 ' Press Play & Record on Tape'  STOP?
 Initializing


 Startaddress in Buffer -- change???
                        -- for COPY?
 FileLength
 ..calculate
 ..and
 ..write into buffer
 CassBuffer [$21..$24]
                           := 0
 Time-HighByte := 0
 SaveStartAdresse
               := CassetteBuffer
 SaveEndAddress
               := Cassett.Buffer-End

\ *** Block No. 134, Hexblock 86

\\ for ST:..STSave            clv01aug87


 3600Baud/short  SaveImpuls (==Bit=0)
         /long   SaveImpuls (==Bit=1)
 set
 TimerNummer2   enabled
 Header  (==Buffer) write
 write Pause
 7200Bd requested  (==SecAdd>=$80) ?
 then 7200Bd/short  SaveImpuls
            /long   SaveImpuls
      set
 SaveBeginAddress
           := from System
 SaveEndAddress
           := from System
 enable RAM, write Data Block
 write Pause, finish







\ *** Block No. 135, Hexblock 87

\\ for ST:supertape stSavSys  clv10oct87

SUPERTAPE
.. set current device
.. patches OS vectors
.. prints message

!! A Supertape-System must be saved in
!! 3 Parts:
!!  1. Mini-Supertape
!!  2. Part of System before
!!  3. Part of System aftr
!! Part 1 will be saved in CBM-Format
!! and is loading Part 2,3 in ST-Format

  Attache Filename to gLOADSYS



  1. from BUFFINIT to excl. BTL save in
  CBM-Format
  use ST-Format
  2. store
  3.   "


\ *** Block No. 136, Hexblock 88



























\ *** Block No. 137, Hexblock 89



























\ *** Block No. 138, Hexblock 8a

\ Sieve benchmark              20oct87re

Onlyforth

: allot  ( u --)
 dup sp@ here - $180 -  u>
 abort" no room" allot ;

&8192 Constant size
Create flags   size allot
: do-prime  ( -- #primes )
 flags size 1 fill    0
 size    0 DO flags I + c@
           IF  I 2* 3+ dup I +
             BEGIN  dup size <
             WHILE  0 over flags + c!
                    over +
             REPEAT  2drop 1+
           THEN
        LOOP ;
: benchmark   9 0 DO do-prime drop  LOOP
 do-prime . ." Primes" ;
: .primes   size 0 DO  flags I + c@
 IF  I 2* 3+ .  THEN ?cr
 stop? IF  LEAVE  THEN  LOOP ;

\ *** Block No. 139, Hexblock 8b



























\ *** Block No. 140, Hexblock 8c



























\ *** Block No. 141, Hexblock 8d



























\ *** Block No. 142, Hexblock 8e



























\ *** Block No. 143, Hexblock 8f



























\ *** Block No. 144, Hexblock 90

\ Graphic-Demos Loadscreen     20oct87re

Only Forth also definitions

\needs Graphic -&80 +load

Graphic also definitions
 page  .( Loading .....)

 1   4 +thru   \ Demo1,2,3,4 Demo
 5     +load   \ Sinplot
 6 &11 +thru   \ Turtle demos


wave wave1 triangle lines moire
sinplot
ornament circles worm coil
town

&20 window






\ *** Block No. 145, Hexblock 91

\ Plot wave                    20oct87re

&100 | Constant &100
&160 | Constant &160
: wave
 cs red cyn colors hires
 &100 0 DO
 &99 0 DO
   I dup * J dup * + &150 / 1 and
    IF &160 J + &100 I + plot
       &160 J - &100 I + plot
       &160 J - &100 I - plot
       &160 J + &100 I - plot THEN
  LOOP LOOP ;

: wave1
 cs blu yel colors hires
 &160 0 DO
 &99 0 DO
   I dup * J dup * + 100u/ 1 and 0=
    IF &160 J + &100 I + plot
       &160 J - &100 I + plot
       &160 J - &100 I - plot
       &160 J + &100 I - plot THEN
  LOOP LOOP ;

\ *** Block No. 146, Hexblock 92

\ lineplot triangle            20oct87re

| : grinit
 clrscreen
 yel blu colors hires ;

: triangle
 grinit
 0 2 DO
   &160 0 DO
      I &199 &160 I 2/     J + flipline
   &320 I - &199 &160 I 2/ J + flipline
   2 +LOOP
 -1 +LOOP text ;












\ *** Block No. 147, Hexblock 93

\ lineplot linies moire        20oct87re

: linies
 grinit
 &320 0 DO
    &320 0 DO I &198 J 0 line &35 +LOOP
 &35 +LOOP ;

: moire
 clrscreen ora red colors hires
 &320 0 DO
  I &198 &319 I - 0 line
 3 +LOOP
 &199 0 DO
  &319 &198 I - 0 I line
 2 +LOOP ;










\ *** Block No. 148, Hexblock 94

\ lineplot boxes               20oct87re

Variable x0       Variable y0
Variable x1       Variable y1

: box   ( x1 y1 x0 y0 -)
 y0 ! x0 ! y1 ! x1 !
 x1 @ y0 @ x0 @ over flipline
 x1 @ y1 @ over y0 @ flipline
 x0 @ y1 @ x1 @ over flipline
 x0 @ y0 @ over y1 @ flipline ;

Create colortab
 blk c, lbl c, red c, lre c,
 pur c, grn c, blu c,

: boxes
 grinit
 &10 3 DO
  &160 0 DO I dup &318 I - &198 I - box
        J +LOOP
   I 3 - colortab + c@  pencolor
   LOOP ;



\ *** Block No. 149, Hexblock 95

\ Graphic sinplot              20oct87re

&10000 Constant 10k

: sinplot
 grinit
 &319  &96   0 &96 line
 &160 &197 &160  0 line
 &152 &160 negate DO
  I &160 + &96 I sin &96 10k */ +
  I &168 + &96 I 8 + sin &96 10k */ +
  line
 8 +LOOP
 &152 &160 negate DO
  I &160 + &96 I cos &96 10k */ +
  I &168 + &96 I 8 + cos &96 10k */ +
  line
 8 +LOOP  ;








\ *** Block No. 150, Hexblock 96

\ Turtle demos                 20oct87re

| : tinit    ( -- )
 clrscreen  hires   \ showturtle
 red cyn colors ;

| : shome  ( -- )
 tinit &65 0 setxy &90 seth pendown ;

: polygon   ( length edges -- )
 &360 over /
 swap 0 DO  over forward
            dup right     LOOP 2drop ;

| : ring  ( edges -- )
 &200 over / swap
 &18 0 DO  2dup vieleck
          &20 right  LOOP  2drop ;

: ornament  ( -- )
 tinit home
 &10 3 DO  clrscreen  I dup 7 -
          IF  ring  ELSE  drop  THEN
          LOOP ;


\ *** Block No. 151, Hexblock 97

\ Turtle demos 1               20oct87re

: circles  ( -- )
 tinit
  2 -1 DO home
   &10 0 DO
    &20 I 2* - &20 polygon
    xcor &10 I 2/  - - setx
    ycor &10 I - J * - sety
   LOOP
  2 +LOOP ;















\ *** Block No. 152, Hexblock 98

\ Turtle demos 2               20oct87re

| : (worm      ( length -- )  recursive
 dup 5 < IF  drop exit  THEN
 dup forward &90 right
 2- (worm ;

: worm  ( -- )
 shome &190 (schnecke ;

| : (coil  ( length -- )  recursive
 dup 5 < IF drop exit THEN
 dup forward &91 right
 2- (coil ;

: coil ( -- )
 shome 5 forward &190 (coil ;









\ *** Block No. 153, Hexblock 99

\ Turtle demos 3               20oct87re

| : startpos
 &15 0 setxy &90 setheading ;

| : continue ( -- )
 &90 right penup &55 forward
 pendown &90 left ;

| : chimney
 xcor ycor
 &50 fd &30 rt &15 fd &30 lt
 &30 fd &90 rt &12 fd &90 rt  8 fd
 setxy &90 setheading ;

| : house
 &50 4 polygon &50 forward  &30 right
 &50 3 polygon &30 left  &50 back
 &90 right &15 forward &90 left
 &20 4 polygon
 &90 left &15 forward &90 right
 chimney ;




\ *** Block No. 154, Hexblock 9a

\ Turtle demos 4               20oct87re

| : rowofhouses
 tinit startpos
 4 0 DO house continue LOOP house ;

| : housewindow
 xcor ycor
 penup &30 fd &90 rt &10 fd &90 lt
 pendown
 &10 4 polygon &90 rt
 penup &20 fd &90 lt
 pendown &10 4 polygon
 setxy ;

: town       houserow
 startpos 4 0 DO housewindow continue
 LOOP
 housewindow ;







\ *** Block No. 155, Hexblock 9b

\ Turtle demos 5               20oct87re

| : (medal ( len grad -- ) recursive
 stop? 0= and ?dup
 IF  over 3 / swap 1-
  4 0 DO 2dup (medal 2 pick forward
         &90 right  LOOP 2drop
  THEN drop ;

: medal
 tinit shome &192 5 (medal ;

\\

: (6medals   ( len grad -)  recursive
 ?dup IF  over 3 / swap 1-
  6 0 DO 2dup (6medals 2 pick forward
         &60 right  LOOP 2drop
  THEN drop ;

: 6medals
 tinit shome &80 &55 setxy
 &85 3 (6medals ;



\ *** Block No. 156, Hexblock 9c



























\ *** Block No. 157, Hexblock 9d



























\ *** Block No. 158, Hexblock 9e



























\ *** Block No. 159, Hexblock 9f



























\ *** Block No. 160, Hexblock a0

\ Sprite-Demo                  23oct87re

\needs graphic   -&96 +load

Onlyforth graphic also  Forth

.( Loading...)

  1   4 +thru

















\ *** Block No. 161, Hexblock a1

\ Sprite-Demo                  20oct87re

Create Shapes  5 $40 * allot
 blk @ 4 +  block
 Shapes  5 $40 * cmove

: init  ( -)
 graphic page
 blu border  blu background
 5 0 DO
     Shapes  I $40 * +  I getform LOOP
 grn wht sprcolors
 5 0 DO  I 0 0 wht I setsprite  LOOP
 5 0 DO  I small  I high  I 3colored set
         I behind LOOP  ;











\ *** Block No. 162, Hexblock a2

\ Sprite-Demo                  20oct87re

: ypos  ( spr# - y)  sprpos drop ;

: xpos  ( spr# - x)  sprpos nip ;

&26 Constant Distance

: 1+0-1  ( n - +1/0/-1)
 dup 0= not swap 0< 2* 1+ and ;

: follow-sprite ( spr# -)
>r r@ xpos  r@ 1- xpos  Distance +
   over -  1+0-1 + &344 min  r@ xmove
   pause
   r@ ypos  r@ 1- ypos
   over -  1+0-1 +           r> ymove
   pause ;








\ *** Block No. 163, Hexblock a3

\ Sprite-Demo                  20oct87re

: follow-cursor  ( spr# -)
>r r@ xpos  Col 8 * &33 +
   over -  1+0-1 +  r@ xmove  pause
   r@ ypos  Row 8 * &59 +
   over -  1+0-1 +  r> ymove  pause ;

: follow  ( spr# -)
pause  dup IF   follow-sprite
           ELSE follow-cursor THEN ;

: killsprites  ( -)  0 sprite c! ;

: slide-sprites  ( -)
5 0 DO  I follow  I 1+ 0 DO  I follow
LOOP LOOP ;

\\

: testslide ( -)
init BEGIN  slide-sprites
            key dup con!  3 = UNTIL ;



\ *** Block No. 164, Hexblock a4

\ Sprite-Demo                  20oct87re

\needs tasks   .( Tasker? ) \\

$100 $100 Task Demo

: slide  ( -)
 Demo activate
 init BEGIN  slide-sprites  REPEAT ;

: endslide  ( -)
 killsprites  Demo activate  stop ;














\ *** Block No. 165, Hexblock a5

ªªª•uv•uv•ªª•€ •€ •ª •u`•u`•ª •€ •€ •€ •€
 •€ •€ •€ ª€          ª ª€ u`%ux•Iv•‚v•‚v
•‚v•‚v•‚v•‚v•‚v•‚v•‚v•‚v•Iv%ux u` ª€
    ªªª€•u`•ux•©v•‚v•‚v•©v•ux•u`•¥`•¥`•‰x
•‰x•‚v•‚v•‚v•‚vª‚ª         ªªªª•uv•uvªvª
v  v  v  v  v  v  v  v  v  v  v  v  v  ª
         ªª‚ª•‚v•‚v•‚v•‚v•‚v•ªv•uv•uv•ªv•
‚v•‚v•‚v•‚v•‚v•‚v•‚vª‚ª         ª


















\ *** Block No. 166, Hexblock a6

\ tiny sprite editor           06nov87re

Onlyforth Graphic also definitions

\needs sprbuf  Create sprbuf $100 allot
\needs >byte : >byte $100 /mod ;

| Variable cbase  2 cbase !

| : width ( -- n )  &16 cbase @ / ;

| : (l: ( -- )
 base push  cbase @ base !
 name number  name number drop
 >r  >byte drop  r@ c!
 >byte r@ 1+ c!  r> 2+ c! ;

: l: (l: quit ;

: #.r  ( n width -- )
 >r 0 <#  r> 0 DO  #  LOOP  #> type ;

: arguments  ( n -- )
 depth < not abort" Arguments?" ;
                                    -->

\ *** Block No. 167, Hexblock a7

\ tiny sprite editor           06nov87re

| Create savearea $1A allot
| Variable xsave    | Variable ysave
| Variable saved    saved off

| : savesprites  ( -- )
 saved @ ?exit
 sprite savearea $1A cmove  0 sprite c!
 7 sprpos xsave ! ysave ! saved on ;

: fertig  ( -- )
 saved @ not ?exit
 ysave @ xsave @ 7 move
 savearea sprite $1A cmove saved off ;

| : sprline  ( adr line -- )
 base push  dup 2* + +  cr
 ." l: "  cbase @ base !
 dup count width #.r  count width #.r
 c@ width #.r  ." . $" hex 4 #.r ;



                                    -->

\ *** Block No. 168, Hexblock a8

\ tiny sprite editor           06nov87re

| : slist  ( mem# -- )
 $40 * sprbuf +
 &21 0 DO  dup I sprline
           stop? IF  LEAVE  THEN  LOOP
 drop cr  ." fertig"  0 0 at quit ;

: sed  ( mem# -- )
 1 arguments  &32 min
 page dup . ." sed \ 1 color"
 savesprites  2 cbase !
 dup $40 $128 yel 7 setsprite
 7 3colored reset  7 big  slist ;

: ced  ( mem# -- )
 1 arguments  &32 min
 page dup . ." ced \ 3 colors"
 savesprites  4 cbase !
 blk gr2 sprcolors
 dup $40 $128 yel 7 setsprite
 7 3colored set  7 high  slist ;




\ *** Block No. 169, Hexblock a9


























