\ Block No. 0
\\ Directory ultraFORTH 3of4   11apr91pz

rom-ram-sys         &2      :::
Transÿent-Asÿembler &4             àà
Assembler-6502      &5:   :
2words             &14
unlink             &15
scr<>cbm           &16
(search            æ17 ààà
Editor           à æ19
.blk               &46
Tracer+Tools       &47                 à
Multi-Tasker       æ57
Printer            &63
Printer-driver     &64
Printer-controls   &70
Printer-tools      &78

scriptor           &90

savesystem        &143
formatdisk        &144
copydisk          &145


\ Block No. 1
\\ Inhalt ultraFORTH 3of4      11apr91pz

rom ram sys          2 - 3
Transient Assembler  4
Assembler-6502       5 - 12
2words              14
unlink              15
scr<>cbm            16
(search             17
Editor              19
.blk                46
Tracer Tools        47
Multi-Tasker        57
Printer:Auswahl     63
Printer:Allgemein   64 - 69
Druckeranpassungen  70 - 77
Printer:Tools       78 - 84

Shadows             85 ff., dazwischen

Scriptor            90 - 131
utilities          143 - 145



\ Block No. 2
\ rom ram sys              clv/re20aug87
\              Shadow mit Ctrl+W--->

\ wird gebraucht, wenn
\ Spruenge ins ROM gehen.

Assembler also definitions
(16 \ Umschalten des Bereichs 8000-FFFF
: rom here 9 + $8000 u> abort" not here"
       $ff3e sta ;
: ram  $ff3f sta ;
: sys rom jsr ram ;
\  wer unter diesem abort" not here"
\  leidet: s.naechster Screen --> C)


(64 \ Umschalten des Bereichs A000-BFFF
: rom here 9 + $A000 u> abort" not here"
      $37 # lda 1 sta ;
: ram $36 # lda 1 sta ;
C)




\ Block No. 3
\ sysMacro Long             clv20aug87re

(64  .( Nicht fuer C64 !) \\ C)

\ Mit Makro: fuer Fortgeschrittene

here $8000 $20 - u> ?exit \ geht nicht!

' 0 | Alias ???

Label long   ROM
Label long1  ??? jsr  RAM  rts end-code

| : sysMacro ( adr -- )
 $100 u/mod  pha  # lda  long1 2+ sta
 # lda  long1 1+ sta  pla  long jsr ;

: sys ( adr -- ) \ fuer Jsr ins ROM
 here 9 + $8000 u>
 IF  sysMacro  ELSE  sys  THEN ;





\ Block No. 4
\ transient Assembler         clv10oct87

\ Basis: Forth Dimensions VOL III No. 5)

\ internal loading         04may85BP/re)

here   $800 hallot  heap dp !

         1  +load

dp !

Onlyforth












\ Block No. 5
\ Forth-6502 Assembler        clv10oct87

\ Basis: Forth Dimensions VOL III No. 5)

Onlyforth  Assembler also definitions

1 7  +thru
 -3  +load \ Makros: rom ram sys

Onlyforth















\ Block No. 6
\ Forth-83 6502-Assembler      20oct87re

: end-code   context 2- @  context ! ;

Create index
$0909 , $1505 , $0115 , $8011 ,
$8009 , $1D0D , $8019 , $8080 ,
$0080 , $1404 , $8014 , $8080 ,
$8080 , $1C0C , $801C , $2C80 ,

| Variable mode

: Mode:  ( n -)   Create c,
  Does>  ( -)     c@ mode ! ;

0   Mode: .A        1    Mode: #
2 | Mode: mem       3    Mode: ,X
4   Mode: ,Y        5    Mode: X)
6   Mode: )Y       $F    Mode: )






\ Block No. 7
\ upmode  cpu                  20oct87re

| : upmode ( addr0 f0 - addr1 f1)
 IF mode @  8 or mode !   THEN
 1 mode @  $F and ?dup IF
 0 DO  dup +  LOOP THEN
 over 1+ @ and 0= ;

: cpu  ( 8b -)   Create  c,
  Does>  ( -)    c@ c, mem ;

 00 cpu brk $18 cpu clc $D8 cpu cld
$58 cpu cli $B8 cpu clv $CA cpu dex
$88 cpu dey $E8 cpu inx $C8 cpu iny
$EA cpu nop $48 cpu pha $08 cpu php
$68 cpu pla $28 cpu plp $40 cpu rti
$60 cpu rts $38 cpu sec $F8 cpu sed
$78 cpu sei $AA cpu tax $A8 cpu tay
$BA cpu tsx $8A cpu txa $9A cpu txs
$98 cpu tya





\ Block No. 8
\ m/cpu                        20oct87re

: m/cpu  ( mode opcode -)  Create c, ,
 Does>
 dup 1+ @ $80 and IF $10 mode +! THEN
 over $FF00 and upmode upmode
 IF mem true Abort" invalid" THEN
 c@ mode @ index + c@ + c, mode @ 7 and
 IF mode @  $F and 7 <
  IF c, ELSE , THEN THEN mem ;

$1C6E $60 m/cpu adc $1C6E $20 m/cpu and
$1C6E $C0 m/cpu cmp $1C6E $40 m/cpu eor
$1C6E $A0 m/cpu lda $1C6E $00 m/cpu ora
$1C6E $E0 m/cpu sbc $1C6C $80 m/cpu sta
$0D0D $01 m/cpu asl $0C0C $C1 m/cpu dec
$0C0C $E1 m/cpu inc $0D0D $41 m/cpu lsr
$0D0D $21 m/cpu rol $0D0D $61 m/cpu ror
$0414 $81 m/cpu stx $0486 $E0 m/cpu cpx
$0486 $C0 m/cpu cpy $1496 $A2 m/cpu ldx
$0C8E $A0 m/cpu ldy $048C $80 m/cpu sty
$0480 $14 m/cpu jsr $8480 $40 m/cpu jmp
$0484 $20 m/cpu bit


\ Block No. 9
\ Assembler conditionals       20oct87re

| : range?   ( branch -- branch )
 dup abs  $7F u> Abort" out of range " ;

: [[  ( BEGIN)  here ;

: ?]  ( UNTIL)  c, here 1+ - range? c, ;

: ?[  ( IF)     c,  here 0 c, ;

: ?[[ ( WHILE)  ?[ swap ;

: ]?  ( THEN)   here over c@  IF swap !
 ELSE over 1+ - range? swap c! THEN ;

: ][  ( ELSE)   here 1+   1 jmp
 swap here over 1+ - range?  swap c! ;

: ]]  ( AGAIN)  jmp ;

: ]]? ( REPEAT) jmp ]? ;



\ Block No. 10
\ Assembler conditionals       20oct87re

$90 Constant CS     $B0 Constant CC
$D0 Constant 0=     $F0 Constant 0<>
$10 Constant 0<     $30 Constant 0>=
$50 Constant VS     $70 Constant VC

: not    $20 [ Forth ] xor ;

: beq    0<> ?] ;   : bmi   0>= ?] ;
: bne    0=  ?] ;   : bpl   0<  ?] ;
: bcc    CS  ?] ;   : bvc   VS  ?] ;
: bcs    CC  ?] ;   : bvs   VC  ?] ;












\ Block No. 11
\ 2inc/2dec   winc/wdec        20oct87re

: 2inc  ( adr -- )
 dup lda  clc  2 # adc
 dup sta  CS ?[  swap 1+ inc  ]?  ;

: 2dec  ( adr -- )
 dup lda  sec  2 # sbc
 dup sta  CC ?[  swap 1+ dec  ]?  ;

: winc  ( adr -- )
 dup inc  0= ?[  swap 1+ inc  ]?  ;

: wdec  ( adr -- )
 dup lda  0= ?[  over 1+ dec  ]?  dec  ;

: ;c:
 recover jsr  end-code ]  0 last !  0 ;







\ Block No. 12
\ ;code Code code>          bp/re03feb85

Onlyforth

: Assembler
 Assembler   [ Assembler ] mem ;

: ;Code
 [compile] Does>  -3 allot
 [compile] ;      -2 allot   Assembler ;
immediate

: Code  Create here dup 2- ! Assembler ;

: >label  ( adr -)
 here | Create  immediate  swap ,
 4 hallot heap 1 and hallot ( 6502-alig)
 here 4 - heap  4  cmove
 heap last @ count $1F and + !  dp !
  Does>  ( - adr)   @
  state @ IF  [compile] Literal  THEN ;

: Label
 [ Assembler ]  here >label Assembler ;

\ Block No. 13
\ ein case-of-konstrukt        07apr91pz
| variable caselink

  : case ( -- ) ( comp: -- n 10 )
     caselink @ caselink off  10 ;
    immediate restrict

  : of ( n1 n2 -- n1/-- )
     compile case?
     compile ?branch >mark 11 ;
    immediate restrict

  : endof ( -- )
     compile branch  here
     caselink @ ,  caselink !
     11 ?pairs  >resolve ;
    immediate restrict

  : elsecase ;  immediate restrict

  : endcase   10 ?pairs
     caselink @ BEGIN ?dup WHILE
     dup @ swap >resolve REPEAT
     caselink ! ;
    immediate restrict
\ Block No. 14
\ 2! 2@ 2variable 2constant clv20aug87re

Code 2!  ( d adr --)
 tya  setup jsr  3 # ldy
 [[  SP )Y lda  N )Y sta  dey  0< ?]
 1 # ldy  Poptwo jmp  end-code

Code 2@  ( adr -- d)
 SP X) lda  N sta  SP )Y lda  N 1+ sta
 SP 2dec  3 # ldy
 [[  N )Y lda  SP )Y sta  dey  0< ?]
 xyNext jmp  end-code

: 2Variable  ( --)   Create 4 allot ;
             ( -- adr)

: 2Constant  ( d --)   Create , ,
  Does> ( -- d)   2@ ;

\ 2dup  exists
\ 2swap exists
\ 2drop exists



\ Block No. 15
\ unlink                    clv20aug87re

$FFF0 >label plot

(64

Code unlink  ( -- )
  $288 lda  $80 # ora  tay  txa
  [[  $D9 ,X sty  clc  $28 # adc
   CS ?[  iny  ]?  inx  $1A # cpx  0= ?]
  $D3 lda  $28 # cmp
  CS ?[  $28 # sbc  $D3 sta  ]?
  $D3 ldy  $D6 ldx  clc  plot jsr C)

(16 : unlink  0 0  $7EE 2! ; C)

Label setptrs
 0 # ldx  1 # ldy  Next jmp  end-code







\ Block No. 16
( changing codes              18may85we)
( Wie gut, dass commodore ...          )
( ... besondere screen-codes hat.      )

Label (scr>cbm
 N 6 + sta $3F # and  N 6 + asl
 N 6 + bit  0< ?[ $80 # ora  ]?
            VC ?[ $40 # ora  ]?  rts

Label (cbm>scr
 N 6 + sta $7F # and $20 # cmp
 CS ?[ $40 # cmp
    CS ?[ $1F # and  N 6 + bit
       0< ?[ $40 # ora  ]?  ]?  rts  ]?
 Ascii . # lda  rts

Code cbm>scr  ( 8b1 -- 8b2)
 SP X) lda  (cbm>scr jsr  SP X) sta
 Next jmp  end-code

Code scr>cbm  ( 8b1 -- 8b2)
 SP X) lda  (scr>cbm jsr  SP X) sta
 Next jmp  end-code


\ Block No. 17
\ schnelles search        bp   17jun85re

\needs Code -$D +load \ Trans Assembler

Onlyforth

 ' 0< @ 4 +  >label puttrue
puttrue 3 +  >label putfalse

Code (search
( text tlen buffer blen -- adr tf / ff)
 7 # ldy
 [[  SP )Y lda  N ,Y sta dey  0< ?]
 [[ N 4 + lda  N 5 + ora  0<> ?[
 [[ N     lda   N 1+ ora  0<> ?[
    N 2+ X) lda  N 6 + X) cmp  swap
    0<> ?[[  N wdec  N 2+ winc  ]]?

-->






\ Block No. 18
\ Edi schnelles search    bp   17jun85re

 7 # ldy
 [[  N ,Y lda  SP )Y sta  dey  0< ?]
 [[  N 2+ winc  N 6 + winc  N wdec
 N 4 + wdec  N 4 + lda  N 5 + ora
 0= ?[  SP lda  clc  4 # adc  SP sta
        CS ?[  SP 1+ inc  ]?
        3 # ldy  N 3 + lda  SP )Y sta
        N 2+ lda  dey  SP )Y sta  dey
        puttrue jmp  ]?
 N lda  N 1+ ora  0= ?[
 3 roll  3 roll ]? ]?
 SP lda  clc  6 # adc  SP sta
 CS ?[  SP 1+ inc ]?   1 # ldy
 putfalse jmp  ]?
 N 2+ X) lda  N 6 + X) cmp  0= not ?]
 7 # ldy
 [[ SP )Y lda  N ,Y sta  dey  0< ?]
 N wdec  N 2+ winc
 ( next char as first )  ]]  end-code




\ Block No. 19
\ Editor loadscreen           clv13jul87
\ Idea and first implementation:  WE/re

Onlyforth
\needs .blk       $1B +load \ .blk
\needs Code       -$F +load \ Assembl
\needs (search     -2 +load \ (search

Onlyforth
(64 | : at  at curoff ; C) \ sorry

\needs 2variable  -5 +load
\needs unlink     -4 +load  \ unlink
\needs scr>cbm    -3 +load  \ cbm><scr

Vocabulary Editor
Editor also definitions

                1 $17 +thru  \ Editor
              $18 $19 +thru  \ edit-view
                  $1A +load  \ Ediboard

Onlyforth  1 scr !  0 r# !

save
\ Block No. 20
\ Edi Constants Variables     clv15jul87

$28 | Constant #col $19 | Constant #row
#col  #row  *           | Constant b/scr
  Variable shadow   $55 shadow !
| Variable ascr     1 ascr !
|  Variable imode   imode off
| Variable char     #cr char !
| Variable scroll   scroll on
| Variable send     1 send !
| 2variable chars   | 2variable lines
| 2variable fbuf    | 2variable rbuf

(64 $288 C)  (16 $53e C)  >Label scradr
(64 $d800 C) (16 $800 C)  >Label coladr

$d1  (16 drop $c8 C) | Constant linptr
$d3  (16 drop $ca C) | Constant curofs

(64 $D020 C) (16 $ff19 C)
 | Constant border
(64 $286  C) (16 $53b C) | Constant pen
(64 $d021 C) (16 $ff15 C)
 | Constant bkgrnd

\ Block No. 21
( Edi special cmoves         clv21.3.87)
( Dank an commodore ...                )

Label incpointer
 N    lda  clc  #col 1+ # adc
 N    sta  CS ?[  N 1+  inc  ]?
 N 2+ lda  clc  #col    # adc
 N 2+ sta  CS ?[  N 3 + inc  ]?  rts

| Code b>sc   ( blkadr --)
 tya  setup jsr
 N 2+ stx  scradr lda  N 3 + sta
 #row # ldx
 [[  #col 1- # ldy
     [[  N    )Y lda  (cbm>scr jsr
         N 2+ )Y sta  dey  0< ?]
     incpointer jsr  dex
 0= ?]
 pen lda
 [[ coladr        ,X sta
    coladr $100 + ,X sta
    coladr $200 + ,X sta
    coladr $300 + ,X sta
    inx  0= ?]  setptrs jmp   end-code

\ Block No. 22
( Edi special cmoves cont.   clv21.3.87)
( ... fuer dies Bildschirmformat.      )

| Code sc>b   ( blkadr --)
 tya  setup jsr
 N 2+ stx  scradr lda  N 3 + sta
 #row # ldx
 [[  0 # ldy
     [[  N 2+ )Y lda  (scr>cbm jsr
         N )Y sta  iny  #col # cpy CS ?]
     dex
 0<> ?[[
     bl # lda  N )Y sta
     incpointer jsr
 ]]?  setptrs jmp  end-code

| Code >scrmove  ( from to 8bquan --)
 3 # lda  setup jsr  dey
 [[  N cpy  0= ?[  setptrs jmp  ]?
     N 4 + )Y lda  (cbm>scr jsr
     N 2+  )Y sta  iny  0= ?]  end-code




\ Block No. 23
( Edi changed?               clv21.3.87)

| Code changed?   ( blkadr -- f)
 tya  setup jsr
 N 2+ stx  scradr lda  N 3 + sta
 #row # ldx
 [[  #col 1- # ldy
     [[  N )Y lda  (cbm>scr jsr
         N 2+ )Y cmp
         0<> ?[  $FF # lda  PushA jmp ]?
         dey 0<  ?]
     incpointer jsr  dex
 0= ?]
 txa  PushA jmp  end-code

| : memtop  sp@ #col 2* - ;









\ Block No. 24
\ Edi c64-specials           clv2:jull87

| Code scrstart  ( -- adr)
 txa pha scradr lda  Push jmp end-code


| Code rowadr  ( -- adr)
 curofs lda  #col # cmp  txa
 CS ?[  #col 1- # lda  ]?
 linptr adc pha linptr 1 + lda  0 # adc
 Push jmp  end-code

| Code curadr  ( -- adr)
 clc curofs lda linptr adc  pha
 linptr 1 + lda 0 # adc Push jmp
 end-code
(64
| Code unlinked?     \ -- f
 $D5 lda  #col # cmp  CC ?[  dex  ]?
 txa  PushA jmp  end-code C)





\ Block No. 25
\ Edi scroll? put/insert/do  clv2:jull87

| : blank.end?  ( -- f)
 scrstart [ b/scr #col - ] Literal +
 #col -trailing nip  0=  scroll @ or ;

| : atlast?  ( -- f)
 curadr  scrstart b/scr + 1-  =
 scroll @ 0=  and ;

| : putchar  ( -- f)
 char c@ con! false ;

| : insert  ( -- f)
 atlast?  ?dup ?exit
(64  unlinked? C) (16 true C)
 rowadr #col + 1- c@  bl = not  and
 blank.end? not  and  dup ?exit
 $94 con! ;

| : dochar  ( -- f)
 atlast?  ?dup ?exit
 imode @ IF insert ?dup ?exit
 THEN putchar ;

\ Block No. 26
( Edi cursor control          15may85re)

| : curdown  ( -- f)
 scroll @ 0=  row  #row 2-  u>  and
 dup ?exit $11 con! ;

| : currite  ( -- f)
 atlast? dup ?exit $1D con! ;

' putchar | Alias curup
' putchar | Alias curleft
' putchar | Alias home
' putchar | Alias delete

| : >""end  ( -- ff)
 scrstart b/scr -trailing nip
 b/scr 1- min #col /mod swap at false ;

| : +tab  ( -- f)
 0  $a 0 DO  drop currite dup
            IF LEAVE THEN  LOOP ;

| : -tab  ( -- f)
 5 0 DO $9D con!  LOOP  false ;

\ Block No. 27
( Edi cr, clear/newline       12jun85re)

| : <cr>  ( -- f)
 row 0 at  unlink  imode off  curdown ;

| : clrline  ( -- ff)
 rowadr #col bl fill false ;

| : clrright  ( -- ff)
 curadr #col col - bl fill false ;

| : killine  ( -- f)
 rowadr dup #col + swap
 scrstart $3C0 + dup >r
 over - cmove
 r> #col bl fill false ;

| : newline  ( -- f)
 blank.end? not  ?dup ?exit
 rowadr dup #col + scrstart b/scr +
 over - cmove>  clrline ;




\ Block No. 28
( Edi character handling      18jun85re)

| : dchar  ( -- f)
 currite  dup ?exit $14 con! ;

| : @char  ( -- f)
 chars 2@ + 1+  lines @ memtop min
 u>  dup ?exit
 curadr c@  chars 2@ +  c!
 1 chars 2+ +! ;

| : copychar  ( -- f)
 @char  ?dup ?exit  currite ;

| : char>buf  ( -- f)
 @char  ?dup ?exit  dchar ;

| : buf>char  ( -- f)
 chars 2+ @ 0=  ?dup ?exit
 insert        dup ?exit
 -1 chars 2+ +!
 chars 2@ +  c@  curadr c! ;



\ Block No. 29
( Edi line handling, imode    18jun85re)

| : @line  ( -- f)
 lines 2@ +  memtop  u>  dup ?exit
 rowadr  lines 2@ +  #col  cmove
 #col lines 2+ +! ;

| : copyline  ( -- f)
 @line  ?dup ?exit  curdown ;

| : line>buf  ( -- f)
 @line  ?dup ?exit  killine ;

| : !line  ( --)
 #col negate lines 2+ +!
 lines 2@ +  rowadr  #col  cmove  ;

| : buf>line  ( -- f)
 lines 2+ @ 0=  ?dup ?exit
 newline  dup ?exit  !line ;

| : setimd  ( -- f)   imode on false ;

| : clrimd  ( -- f)   imode off false ;

\ Block No. 30
( Edi the stamp               17jun85re)

Forth definitions
: rvson $12 con! ;  : rvsoff $92 con! ;

Code ***ultraFORTH83***
     Next here 2- !  end-code
: Forth-Gesellschaft   [compile] \\ ;
immediate

Editor definitions
Create stamp$ $12 allot stamp$ $12 erase

| : .stamp  ( -- ff)
 stamp$ 1+ count  scrstart #col +
 over -   swap >scrmove false ;

: getstamp  ( --)
 input push  keyboard  stamp$ on
 cr ." your stamp: "  rvson $10 spaces
 row $C at  stamp$ 2+ $10 expect
 rvsoff  span @ stamp$ 1+ c! ;

| : stamp?  ( --)
 stamp$ c@ ?exit getstamp ;
\ Block No. 31
\ Edi the screen#             clv01aug87

| : savetop  ( --)
 scrstart pad #col 2* cmove
 scrstart #col 2* $A0 fill ;
| : resttop  ( --)
 pad scrstart #col 2* cmove ;
| : updated?  ( scr# -- n)
 block 2- @ ;
| : special  ( --)
 curon BEGIN pause key? UNTIL curoff ;

| : drvScr ( --drv scr')
 scr @ offset @ + blk/drv u/mod swap ;

| : .scr#  ( -- ff) at? savetop  rvson
 0 0 at drvScr ." Scr # " . ." Drv " .
 scr @ updated? 0=
 IF ." not " THEN ." updated"  1 1 at
 [ ' ***ultraFORTH83*** >name ] Literal
 count type 2 spaces
 [ ' Forth-Gesellschaft >name ] Literal
 count $1F and type
 rvsoff at special resttop false ;

\ Block No. 32
( Edi exits                   20may85re)

| : at?>r#  ( --)
 at? swap #col 1+ * + r# ! ;

| : r#>at  ( --)
 r# @  dup  #col 1+  mod  #col =  -
 b/blk 1- min  #col 1+  /mod  swap at ;

| : cancel  ( -- n)
 unlink  %0001  at?>r# ;

| : eupdate ( -- n)
 cancel  scr @ block changed?
 IF .stamp drop  scr @ block sc>b
    update %0010 or THEN ;

| : esave   ( -- n)   eupdate %0100 or ;

| : eload   ( -- n)   esave   %1000 or ;





\ Block No. 33
\ leaf thru Edi               clv01aug87

| : elist  ( -- ff)
 scr @ block b>sc  imode off  unlink
 r#>at  false ;

| : next    ( -- ff)
 eupdate drop  1 scr +!  elist ;

| : back    ( -- ff)
 eupdate drop -1 scr +!  elist ;

| : >shadow  ( -- ff)
 eupdate drop  shadow @ dup drvScr nip
 u> not IF negate THEN  scr +!  elist ;

| : alter  ( -- ff)
 eupdate drop  ascr @  scr @
 ascr !  scr !  elist ;






\ Block No. 34
\ Edi digits                    2oct87re

Forth definitions

: digdecode  ( adr cnt1 key -- adr cnt2)
 #bs case?   IF  dup  IF
                 del 1- THEN exit THEN
 #cr case?   IF  dup span !  exit THEN
 capital dup digit?
 IF  drop >r 2dup + r@ swap c!
     r> emit  1+  exit  THEN  drop ;

Input: digits
 c64key c64key? digdecode c64expect ;

Editor definitions

| : replace  ( -- f)
 fbuf @ 0 DO  #bs con!  LOOP
 false rbuf @ 0 DO insert or LOOP
 dup ?exit
 rbuf 2@ curadr swap >scrmove
 eupdate drop ;


\ Block No. 35
( Edi >bufs                   20nov85re)

| : .buf  ( adr count --)
 type Ascii < emit
 #col 1- col - spaces ;

| : >bufs  ( --)
 input push
 unlink savetop at?  rvson
 1 0 at ." replace with: "
 at? rbuf 2@ .buf
 0 0 at ." >     search: "
 at? fbuf 2@ .buf
 0 2  2dup at  send @ 3 u.r  2dup at
 here 1+ 3 digits expect  span @ ?dup
 IF  here under c!  number drop send !
     THEN  at  send @ 3 u.r  keyboard
 2dup at fbuf 2+ @  #col 2- col - expect
 span @ ?dup IF  fbuf !  THEN
 at fbuf 2@ .buf
 2dup at rbuf 2+ @  #col 2- col - expect
 span @ ?dup IF  rbuf !  THEN
 at rbuf 2@ .buf
 rvsoff resttop at ;

\ Block No. 36
\ Edi esearch                 clv06aug87

| : (f      elist drop
 fbuf 2@  r# @  scr @ block  +
 b/blk r# @ - (search 0=
 IF  0  ELSE  scr @ block -  THEN
 r# !  r#>at ;

| : esearch  ( -- f)
 eupdate drop  >bufs
 BEGIN BEGIN  (f  r# @
       WHILE  key  dup Ascii r =
              IF replace ?dup
                 IF nip exit THEN THEN
              3 = ?dup ?exit
       REPEAT  drvScr nip send @ -
       stop? 0= and ?dup
 WHILE 0< IF   next drop
          ELSE back drop THEN
 REPEAT true ;





\ Block No. 37
\ Edi keytable               clv2:jull87
| : Ctrl  ( -- 8b)
 [compile] Ascii $40 - ; immediate
| Create keytable
Ctrl n c, Ctrl b c, Ctrl w c, Ctrl a c,
$1F c, (64 Ctrl ^ C)      (16 $92 C) c,
$0D c,   $8D c,
Ctrl c c, Ctrl x c, Ctrl f c, Ctrl l c,
$85 c,   $89 c,    $86 c,    $8A c,
$9F c,   $1C c, (64 00 C) (16 $1e C) c,
$8B c,   $87 c,    $88 c,    $8C c,
$1D c,   $11 c,    $9D c,    $91 c,
$13 c,   $93 c,    $94 c,
$14 c,    Ctrl d c, Ctrl e c, Ctrl r c,
Ctrl i c, Ctrl o c,
                             $ff c,









\ Block No. 38
( Edi actiontable             clv9.4.87)


| Create actiontable ]
next      back      >shadow   alter
esearch   copyline
<cr>      <cr>
cancel    eupdate   esave     eload
newline   killine   buf>line  line>buf
.stamp    .scr#           copychar
char>buf  buf>char  +tab      -tab
currite   curdown   curleft   curup
home      >""end    insert
delete    dchar     clrline   clrright
setimd    clrimd
                              dochar  [
| Code findkey  ( key n -- adr)
 2 # lda  setup jsr  N ldy  dey
 [[  iny  keytable ,Y lda  $FF # cmp
     0<> ?[  N 2+ cmp  ]?  0= ?]
 tya  .A asl  tay
 actiontable    ,Y lda  pha
 actiontable 1+ ,Y lda  Push jmp
end-code

\ Block No. 39
( Edi show errors            clv21.3.87)


' 0   | Alias dark

' 1   | Alias light

| : half  ( n --)
 border c!  pause $80 0 DO LOOP ;

| : blink ( --)
 border push  dark half light half
              dark half light half ;

| : ?blink ( f1 -- f2)
 dup true = IF  blink 0=  THEN ;









\ Block No. 40
( Edi init                    18jun85re)

' Literal | Alias Li  immediate

Variable (pad       0 (pad !

| : clearbuffer  ( --)
 pad       dup  (pad  !
 #col 2* + dup  fbuf  2+ !
 #col    + dup  rbuf  2+ !
 #col    + dup  chars !
 #col 2* +      lines !
 chars 2+ off  lines 2+ off
 [ ' ***ultraFORTH83*** >name ] Li
 count >r fbuf 2+ @ r@ cmove r> fbuf !
 [ ' Forth-Gesellschaft >name ] Li
 count $1F and >r
 rbuf 2+ @ r@ cmove r> rbuf ! ;

| : initptr ( --)
 pad (pad @ = ?exit clearbuffer ;




\ Block No. 41
\ Edi show                    clv15jul87

' name >body 6 +  | Constant 'name
(16 \ c16 benutzt standard C)

(64
| Code curon
 $D3 ldy    $D1 )Y lda  $CE sta
 $80 # eor  $D1 )Y sta
 xyNext jmp  end-code

| Code curoff
 $CE lda  $D3 ldy  $D1 )Y sta
 xyNext jmp  end-code

C)









\ Block No. 42
( Edi show                    17jun85re)

| : showoff
 ['] exit 'name !  rvsoff  curoff ;

| : show  ( --)
 blk @ ?dup 0= IF  showoff exit  THEN
 >in @ 1-  r# !  rvsoff curoff rvson
 scr @  over - IF  scr !  elist
 1 0 at .status THEN r#>at curon drop ;

Forth definitions

: (load  ( blk pos --)
 >in push  >in !  ?dup 0= ?exit
 blk push  blk !  .status interpret ;

: showload  ( blk pos -)
 scr push  scr off  r# push
 ['] show 'name ! (load showoff ;

Editor definitions



\ Block No. 43
\ Edi edit                    clv01aug87
| : setcol ( 0 / 4 / 8 --)
 ink-pot +
 dup c@ border c! dup 1+ c@ bkgrnd c!
  2+ c@ pen c! ;
| : (edit  ( -- n)
 4 setcol $93 con!
 elist drop  scroll off
 BEGIN key dup char c!
   0 findkey execute ?blink ?dup UNTIL
 0 0 at killine drop  scroll on
 0 setcol (16 0 $7ea c! C) \ Append-Mode
;
Forth definitions
: edit ( scr# -) (16 c64fkeys C)
 scr !  stamp?  initptr  (edit
 $18 0 at  drvScr ." Scr " . ." Drv " .
 dup 2 and 0=  IF ." not "     THEN
                  ." changed"
 dup 4 and     IF save-buffers THEN
 dup 6 and 6 = IF ." , saved"  THEN
     8 and     IF ." , loading" cr
       scr @  r# @  showload   THEN ;


\ Block No. 44
\ Editor Forth83             clv2:jull87

: l  ( scr -)   r# off  edit ;
: r  ( -)       scr @ edit ;
: +l ( n -)     scr @ + l ;

: v  ( -) ( text)
 '  >name  ?dup IF  4 - @  THEN  ;

: view  ( -) ( text)
 v ?dup
 IF  l  ELSE  ." from keyboard"  THEN ;

Editor definitions

(16 | : curaddr \ --Addr
     linptr @ curofs c@ + ; C)

: curlin  ( --curAddr linLen) \ & EOLn
(64 linptr @ $D5 c@ -trailing
     dup $d3 c! C)
(16 $1b con! ascii j con! curaddr
    $1b con! ascii k con! $1d con!
     curaddr  over - C) ;

\ Block No. 45
( Edidecode                  clv26.3.87)

: edidecode  ( adr cnt1 key -- adr cnt2)
 $8D case? IF  imode off cr exit  THEN
 #cr case? IF  imode off
curlin dup span @ u> IF drop span @ THEN
  bounds ?DO
  2dup +  I c@ scr>cbm  swap c!  1+ LOOP
  dup span !  exit  THEN
 dup char c!
 $12 findkey execute ?blink drop ;


: ediexpect ( addr len1 -- )
 initptr  span !
 0 BEGIN  dup span @  u<
   WHILE  key decode  REPEAT
 2drop space ;

Input: ediboard
 c64key c64key? edidecode ediexpect ;

ediboard


\ Block No. 46
( .status                     15jun85re)

' noop Is .status

: .blk  ( -)
 blk @ ?dup IF  ."  Blk " u. ?cr  THEN ;

' .blk Is .status

















\ Block No. 47
\ tracer: loadscreen          clv12oct87

Onlyforth

\needs Code -$2B +load \ Trans Assembler

\needs Tools   Vocabulary Tools

Tools also definitions

   1 6  +thru  \ Tracer
   7 8  +thru  \ Tools for decompiling

Onlyforth

\\

Dieser wundervolle Tracer wurde
von Bernd Pennemann und Co fuer
den Atari entwickelt. Ich liess mir
aufschwatzen, ihn an C64/C16 anzupassen
und muss sagen, es ging erstaunlich
einfach. /clv


\ Block No. 48
\ tracer: wcmp variables      clv04aug87

Assembler also definitions

: wcmp ( adr1 adr2--) \ Assembler-Macro
 over lda dup cmp swap  \ compares word
 1+   lda 1+  sbc ;


Only Forth also Tools also definitions

| Variable (W
| Variable <ip      | Variable ip>
| Variable nest?    | Variable trap?
| Variable last'    | Variable #spaces










\ Block No. 49
\ tracer:cpush oneline        clv12oct87

| Create cpull    0  ]
 rp@ count 2dup + rp! r> swap cmove ;

: cpush  ( addr len -)
 r> -rot   over  >r
 rp@ over 1+ - dup rp!  place
 cpull >r  >r ;

| : oneline  &82 allot keyboard display
 .status  space  query  interpret
 -&82 allot  rdrop
 ( delete quit from tnext )  ;

: range ( adr--) \ ermittelt <ip ip>
 ip> off  dup <ip !
 BEGIN 1+ dup @
   [ Forth ] ['] unnest = UNTIL
 3+ ip> ! ;





\ Block No. 50
\ tracer:step tnext           clv04aug87

| Code step
 $ff # lda trap? sta trap? 1+ sta
           RP X) lda  IP sta
 RP )Y lda  IP 1+ sta  RP 2inc
 (W lda  W sta   (W 1+ lda   W 1+ sta
Label W1-  W 1- jmp  end-code

| Create: nextstep step ;

Label  tnext IP 2inc
 trap? lda  W1- beq
 nest? lda 0=  \ low(!)Byte test
 ?[ IP <ip wcmp W1- bcc
    IP ip> wcmp W1- bcs
 ][ nest? stx  \ low(!)Byte clear
 ]?
  trap? dup stx 1+ stx \ disable tracer
  W lda  (W sta    W 1+ lda   (W 1+ sta





\ Block No. 51
\ tracer:..tnext              clv12oct87

 ;c: nest? @
 IF nest? off r> ip> push <ip push
    dup 2- range
    #spaces push 1 #spaces +! >r THEN
 r@  nextstep >r
 input push    output push
 2- dup last' !
 cr #spaces @ spaces
 dup 4 u.r @ dup 5 u.r space
 >name .name  $10 col - 0 max spaces .s
 state push  blk push  >in push
 [ ' 'quit      >body ] Literal  push
 [ ' >interpret >body ] Literal  push
 #tib push  tib #tib @ cpush  r0 push
 rp@ r0 !
 ['] oneline Is 'quit  quit ;







\ Block No. 52
\ tracer:do-trace traceable   clv12oct87

| Code do-trace \ installs TNEXT
 tnext 0 $100 m/mod
     # lda  Next $c + sta
     # lda  Next $b + sta
 $4C # lda  Next $a + sta  Next jmp
end-code

| : traceable ( cfa--<IP ) recursive
 dup @
 ['] :    @ case? IF >body     exit THEN
 ['] key  @ case? IF >body c@ Input  @ +
                   @ traceable exit THEN
 ['] type @ case? IF >body c@ Output @ +
                   @ traceable exit THEN
 ['] r/w  @ case? IF >body
                   @ traceable exit THEN
 @  [ ' Forth @ @ ] Literal =
                  IF @ 3 + exit THEN
 \ fuer def.Worte mit does>
 >name .name ." can't be DEBUGged"
 quit ;


\ Block No. 53
\ tracer:Benutzer/innen-Worte clv12oct87

: nest   \ trace into current word
 last' @ @ traceable drop nest? on ;

: unnest \ proceeds at calling word
 <ip on ip> off ; \ clears trap range

: endloop last' @ 4 + <ip ! ;
\ no trace of next word to skip LOOP..

' end-trace Alias unbug \ cont. execut.

: (debug  ( cfa-- )
 traceable range
 nest? off trap? on #spaces off
 Tools do-trace ;

Forth definitions

: debug  ' (debug ; \ word follows

: trace'            \ word follows
 ' dup (debug execute end-trace ;

\ Block No. 54
\ tools for decompiling,      clv12oct87

( interactive use                      )

Onlyforth Tools also definitions

| : ?:  ?cr dup 4 u.r ." :"  ;
| : @?  dup @ 6 u.r ;
| : c?  dup c@ 3 .r ;
| : bl  $24 col - 0 max spaces ;

: s  ( adr - adr+)
 ( print literal string)
 ?:  space c? 4 spaces dup count type
 dup c@ + 1+ bl  ;  ( count + re)

: n  ( adr - adr+2)
 ( print name of next word by its cfa)
 ?: @? 2 spaces
 dup @ >name .name 2+ bl ;

: k  ( adr - adr+2)
 ( print literal value)
 ?: @? 2+ bl ;

\ Block No. 55
( tools for decompiling, interactive   )

: d  ( adr n - adr+n) ( dump n bytes)
 2dup swap ?: 3 spaces  swap 0
 DO  c? 1+ LOOP
 4 spaces -rot type bl ;

: c  ( adr - adr+1)
 ( print byte as unsigned value)
 1 d ;

: b  ( adr - adr+2)
 ( print branch target location )
 ?: @? dup @  over + 6 u.r 2+ bl  ;

( used for : )
( Name String Literal Dump Clit Branch )
( -    -      -       -    -    -      )







\ Block No. 56
( debugging utilities      bp 19 02 85 )


: unravel   \  unravel perform (abort"
 rdrop rdrop rdrop
 cr ." trace dump is "  cr
 BEGIN  rp@   r0 @ -
 WHILE   r>  dup  8 u.r  space
         2- @  >name .name  cr
 REPEAT (error ;

' unravel errorhandler !













\ Block No. 57
\ Multitasker               BP 07apr91pz

Onlyforth

\needs multitask  1 +load  ( save )

  2  4 +thru        \ Tasker
\    5 +load        \ Demotask

















\ Block No. 58
\ Multitasker               BP 13.9.84 )

\needs Code -$36 +load  \ transient Ass

Code stop
 SP 2dec  IP    lda  SP X) sta
          IP 1+ lda  SP )Y sta
 SP 2dec  RP    lda  SP X) sta
          RP 1+ lda  SP )Y sta
 6 # ldy  SP    lda  UP )Y sta
     iny  SP 1+ lda  UP )Y sta
 1 # ldy  tya  clc  UP adc  W sta
 txa  UP 1+ adc  W 1+ sta
 W 1- jmp   end-code

| Create taskpause   Assembler
 $2C # lda  UP X) sta  ' stop @ jmp
end-code

: singletask
 [ ' pause @ ] Literal  ['] pause ! ;

: multitask   taskpause ['] pause ! ;


\ Block No. 59
\ pass  activate           ks 8 may 84 )

: pass  ( n0 .. nr-1 Tadr r -- )
 BEGIN  [ rot ( Trick ! ) ]
  swap  $2C over c! \ awake Task
  r> -rot           \ IP r addr
  8 + >r            \ s0 of Task
  r@ 2+ @  swap     \ IP r0 r
  2+ 2*             \ bytes on Taskstack
                    \ incl. r0 & IP
  r@ @ over -       \ new SP
  dup r> 2- !       \ into ssave
  swap bounds  ?DO  I !  2 +LOOP  ;
restrict

: activate ( Tadr --)
 0 [ -rot ( Trick ! ) ]  REPEAT ;
-2 allot  restrict

: sleep  ( Tadr --)
 $4C swap c! ;       \ JMP-Opcode

: wake  ( Tadr --)
 $2C swap c! ;       \ BIT-Opcode

\ Block No. 60
\ building a Task           BP 13.9.84 )

| : taskerror  ( string -)
 standardi/o  singletask
 ." Task error : " count type
 multitask stop ;

: Task ( rlen  slen -- )
 allot              \ Stack
 here $FF and $FE =
 IF 1 allot THEN     \ 6502-align
 up@ here $100 cmove \ init user area
 here  $4C c,       \ JMP opcode
                    \     to sleep Task
 up@ 1+ @ ,
 dup  up@ 1+ !      \ link Task
 3 allot            \ allot JSR wake
 dup  6 -  dup , ,  \ ssave and s0
 2dup +  ,          \ here + rlen = r0
 under  + here - 2+ allot
 ['] taskerror  over
 [ ' errorhandler >body c@ ] Literal + !
 Constant ;


\ Block No. 61
\ more Tasks           ks/bp  26apr85re)

: rendezvous  ( semaphoradr -)
 dup unlock pause lock ;

| : statesmart
 state @ IF [compile] Literal THEN ;

: 's  ( Tadr - adr.of.taskuservar)
 ' >body c@ + statesmart ; immediate

\ Syntax:   2  Demotask 's base  !
\ makes Demotask working binary

: tasks  ( -)
 ." MAIN " cr up@ dup 1+ @
 BEGIN  2dup - WHILE
  dup [ ' r0 >body c@ ] Literal + @
  6 + name> >name .name
  dup c@ $4C = IF ." sleeping" THEN cr
 1+ @ REPEAT  2drop ;




\ Block No. 62
\ Taskdemo                    clv12aug87

: taskmark ; \needs cbm>scr : cbm>scr ;

: scrstart  ( -- adr)
  (64 $288 C) (16 $53e C) c@ $100 * ;

Variable counter  counter off

$100 $100 Task Background

: >count  ( n -)
 Background 1 pass
 counter !
 BEGIN  counter @  -1 counter +! ?dup
 WHILE  pause 0 <# #s #>
  0 DO  pause  dup I + c@  cbm>scr
        scrstart I +  c!  LOOP  drop
 REPEAT
 BEGIN stop REPEAT ; \ stop's forever
: wait  Background sleep ;
: go    Background wake ;

multitask       $100 >count  page

\ Block No. 63
\ printer: z.B. LQ550 seriell  07apr91pz

  Onlyforth

  | ' noop ALIAS )      immediate

\ | ' noop ALIAS (u.pa2 immediate
\ | ' noop ALIAS (u.pc  immediate
  | ' noop ALIAS (s     immediate
  | ' (    ALIAS (u.pa2 immediate
  | ' (    ALIAS (u.pc  immediate
\ | ' (    ALIAS (s     immediate

(s
| 4 constant ga  \ geraeteadresse
| 0 constant sa  \ sekundaeradresse
)

  76 ( LQ550 printer controls )

  64 load ( 2nd loadscreen )




\ Block No. 64
\ printer: 2nd loadscreen      07apr91pz

Onlyforth

\needs (s .( 1st loadscreen? ) abort

Vocabulary Print
Print definitions also

Create Prter 2 allot  ( Semaphor)
Prter off

(s    1 +load )

    2 3 +thru
         load ( printer-controls )

    4 5 +thru

Onlyforth

( clear )



\ Block No. 65
\ serielles p! mit Puffer      06apr91pz

(s \ ) \\

| $100 Constant buflen
| Variable Prbuf  buflen allot Prbuf off

| : >buf  ( char --)
 Prbuf count + c!  1 Prbuf +! ;

| : full?  ( -- f)   Prbuf c@ buflen = ;

| : .buf  ( --)
 Prbuf count -trailing
 ga sa busout bustype busoff
 Prbuf off ;

: p!  ( char --)
 pause  >r
 r@ $C ( Formfeed  ) =
 IF  r> >buf .buf exit  THEN
 r@ $A ( Linefeed  ) =
 r@ $D ( CarReturn ) = or  full? or
 IF  .buf  THEN  r> >buf ;

\ Block No. 66
\ 2x p! am userport            06apr91pz

(u.pa2
: p!  \ char --
 $DD01 c!  $DD00 dup c@ 2dup
 $FB and swap c!  4 or swap c!
  BEGIN  pause  $DD0D c@ $10 and
  UNTIL ;  )

(u.pc
: p!  \ 8b --
 BEGIN  pause  $DD0D c@  $10 and  UNTIL
 $DD01 c! ;  )


| : init-p!
(u.pa2  $FF $DD03 c!
        $DD02 dup c@  4 or swap c! )
(u.pc   0 $DD01 c! $FF $DD03 c!    )
(s                                 ) ;





\ Block No. 67
\ cbm-ascii-wandlung           06apr91pz

| : (c>a  ( 8b0 -- 8b1)
dup $41 $5B uwithin IF $20 or  exit THEN
dup $C1 $DB uwithin IF $7F and exit THEN
dup $DB $E0 uwithin IF $A0 xor THEN ;

| defer c>a

: convert  ['] (c>a IS c>a ;
: linear   ['] noop IS c>a ;

  convert



| Variable pcol  pcol off
| Variable prow  prow off







\ Block No. 68
\ output: printer              06apr91pz


| : pemit  c>a p!  1 pcol +! ;
| : pcr   CRET LF  1 prow +!  0 pcol ! ;
| : pdel   DEL  -1 pcol +! ;
| : ppage  FF  0 prow !  0 pcol ! ;
| : pat    ( zeile spalte -- )
  over   prow @ < IF  ppage  THEN
  swap prow @ - 0 ?DO pcr LOOP
  dup  pcol < IF  CRET  pcol off  THEN
  pcol @ - spaces ;
| : pat?   prow @  pcol @ ;
| : ptype  ( adr count --)  dup pcol +!
 bounds ?DO  I c@ c>a p!  LOOP ;

| Output: >printer
 pemit pcr ptype pdel ppage pat pat? ;







\ Block No. 69
\ output: printer&display      06apr91pz

| : bemit   dup  c64emit  pemit ;
| : bcr          c64cr    pcr   ;
| : btype   2dup c64type  ptype ;
| : bdel         c64del   pdel  ;
| : bpage        c64page  ppage ;
| : bat     2dup c64at    pat   ;

| Output: >both
 bemit bcr btype bdel bpage bat pat? ;

Forth definitions

: Printer
  prinit  >printer ;

: Protocol
  prinit  >both ;






\ Block No. 70
\ rx80 printer controls        06apr91pz

| : ctrl:  ( 8b --)   Create c,
  does>  ( --)   c@ p! ;

   7 ctrl: BEL    | $7F ctrl: DEL
| $d ctrl: CRET   | $1B ctrl: ESC
  $a ctrl: LF       $0C ctrl: FF

| : esc:   ( 8b --)   Create c,
  does>  ( --)   ESC c@ p! ;

 $30 esc: 1/8"       $31 esc: 1/10"
 $32 esc: 1/6"
 $54 esc: suoff
 $4E esc: +jump      $4F esc: -jump


 1 2 +thru






\ Block No. 71
\   rx80 printer controls      06apr91pz

| : ESC2   ESC  p! p! ;

  : gorlitz  ( 8b --)   BL ESC2 ;

| : ESC"!"  ( 8b --)   $21 ESC2 ;

| Variable Modus  Modus off

| : on:  ( 8b --)  Create c,
  does>  ( --)
  c@ Modus c@ or dup Modus c! ESC"!" ;

| : off:  ( 8b --)   Create $FF xor c,
  does>  ( --)
  c@ Modus c@ and dup Modus c! ESC"!" ;

 $10 on: +dark    $10 off: -dark
 $20 on: +wide    $20 off: -wide
 $40 on: +cursiv  $40 off: -cursiv
 $80 on: +under   $80 off: -under
   1 on: 12cpi      1 off: 10cpi
   4 on: +cond      4 off: -cond

\ Block No. 72
\   rx80 printer controls      07apr91pz

: super   0 $53 ESC2 ;
: sub     1 $53 ESC2 ;
: lines  ( #lines --)  $43 ESC2 ;
: "long  ( inches --)   0 lines p! ;
: american   0 $52 ESC2 ;
: german     2 $52 ESC2 ;

: prinit
   init-p!
  convert
  Modus off  10cpi  american  suoff
  1/6"  $c "long  CRET ;

(   Ascii x gorlitz  Ascii b gorlitz
    Ascii e gorlitz  Ascii t gorlitz
    Ascii z gorlitz  Ascii l gorlitz )







\ Block No. 73
\ VC 1526 Printer Controls     06apr91pz

| : ctrl:  ( 8b --)   Create c,
  does>  ( --)   c@ p! ;

| $d ctrl: CRET   | $1B ctrl: ESC
| : LF ;

| variable #lines
: lines  ( #lines -- )  #lines ! ;

| : FF  #lines @  prow @ over mod
        DO CRET LOOP ;

| : DEL [compile] ; [compile] \ ;
        ( trick )


| : prinit
     init-p!  72 lines  linear ;





\ Block No. 74
\ CP-80 Printer Controls       06apr91pz

| : ctrl:  ( B -)   Create c,
  does>  ( -)   c@ p! ;

   07 ctrl: BEL    | $7F ctrl: DEL
| $0D ctrl: CRET   | $1B ctrl: ESC
  $0A ctrl: LF       $0C ctrl: FF

| : esc:   ( B -)   Create c,
  does>  ( -)   ESC c@ p! ;

 $30 esc: 1/8"       $31 esc: 1/10"
 $32 esc: 1/6"       $20 esc: gorlitz

| : ESC2   ESC  p! p! ;

 $0e esc: +wide  $14 esc: -wide
 $45 esc: +dark  $46 esc: -dark
 $47 esc: +dub   $48 esc: -dub
 $0f esc: +cond  $12 esc: -cond

: +under 1 $2D esc2 ;
: -under 0 $2D esc2 ;     1 +load

\ Block No. 75
\   CP-80 Printer Controls     07apr91pz

  $54 esc: suoff

: super   0 $53 ESC2 ;

: sub     1 $53 ESC2 ;

: lines  ( lines -)   $43 ESC2 ;

: "long  ( inches -)   0 lines p! ;

: american   0 $52 ESC2 ;

: german     2 $52 ESC2 ;

: pspaces  ( n -)
  0 swap bounds ?DO  BL p!  LOOP ;

: prinit   init-p! convert
  american  suoff  1/6"
  &12 "long  CRET ;



\ Block No. 76
\ LQ550 printer controls       07apr91pz

| : ctrl:  ( 8b --)   Create c,
  does>  ( --)   c@ p! ;

   7 ctrl: BEL    | $7F ctrl: DEL
  $d ctrl: CRET     $1B ctrl: ESC
  $a ctrl: LF       $0C ctrl: FF
  $f ctrl: +cond    $12 ctrl: -cond

| : esc:   ( 8b --)   Create c,
  does>  ( --)   ESC c@ p! ;

  $4E esc: +jump      $4F esc: -jump
  $50 esc: 10cpi      $4d esc: 12cpi
  $67 esc: 15cpi
  $34 esc: +ital      $35 esc: -ital
  $45 esc: +dark      $46 esc: -dark
  $40 esc: preset
  $30 esc: 1/8"       $32 esc: 1/6"
| $41 esc: n/60"
  : /60" ( n -- ) n/60" p! ;
                              1 +load


\ Block No. 77
\   LQ550 printer controls     07apr91pz

| : esc2:  ( 8b1 8b2 --)   Create c, c,
  does>  ESC  dup c@ p!  1+ c@ p! ;

  1 $57 esc2: +wide
  0 $57 esc2: -wide
  1 $77 esc2: +high
  0 $77 esc2: -high
  1 $2d esc2: +under
  0 $2d esc2: -under
  0 $53 esc2: super
  1 $53 esc2: sub
    $54 esc:  suoff
  1 $78 esc2: LQ
  0 $78 esc2: draft
  0 $52 esc2: american
  2 $52 esc2: german

| $43 esc: lin
  : lines  ( #lines --)  lin p! ;
  : "long  ( inches --)   0 lines p! ;

| : prinit  init-p! convert ;

\ Block No. 78
\ 2scr's nscr's thru           06apr91pz

onlyforth print also forth

| : 2scr's  ( blk1 blk2 --)   cr LF
 10cpi +cond +wide +dark $15 spaces
 over 3 .r $13 spaces dup 3 .r
 -dark -wide cr  b/blk 0 DO
  cr I c/l / $15 .r  4 spaces
  over block I +  C/L 1- type  5 spaces
  dup  block I +  C/L 1- -trailing type
 C/L +LOOP  2drop -cond cr ;

| : nscr's  ( blk1 n -- blk2)   2dup
 bounds DO I  over I + 2scr's LOOP + ;

: pthru  ( from to --)
 Prter lock  Output push Printer  1/8"
 1+ over - 1+ -2 and 6 /mod
 ?dup IF swap >r
 0 DO 3 nscr's 3+ page LOOP  r> THEN
 ?dup IF 1+ 2/ nscr's page THEN drop
 Prter unlock ;


\ Block No. 79
\ Printing with shadows        06apr91pz

onlyforth print also forth

| : 2scr's  ( blk1 blk2 --)
 cr LF  10cpi +cond +wide +dark
 $15 spaces  dup  3 .r
 -dark -wide cr  b/blk 0 DO
  cr I c/l / $15 .r  4 spaces
  dup  block I +  C/L 1- type  5 spaces
  over block I +  C/L 1- -trailing type
 C/L +LOOP  2drop -cond cr ;

| : nscr's  ( blk1 n -- blk2)
 0 DO dup [ Editor ]  shadow @   2dup
 u> IF negate THEN
 + over 2scr's 1+ LOOP ;

: dokument  ( from to --)
 Prter lock  Output push  Printer
 1/8"  1+ over - 3 /mod
 ?dup IF swap >r
 0 DO 3 nscr's page LOOP  r> THEN
 ?dup IF nscr's page THEN drop
 Prter unlock ;
\ Block No. 80
\ 2scr's nscr's thru           06apr91pz

onlyforth print also forth
| $40 Constant C/L

| : 2scr's  ( blk1 blk2 --)
 pcr LF LF 10cpi +dark  $12 spaces
 over 3 .r  $20 spaces dup 3 .r
 cr +cond -dark
 $10 C/L * 0 DO cr over block I + C/L
 6 spaces type 2 spaces
 dup block I + C/L -trailing type
 C/L  +LOOP  2drop -cond cr ;

| : nscr's ( blk1 n -- blk2)   under 0
 DO 2dup dup rot + 2scr's 1+ LOOP nip ;

: 64pthru  ( from to --)
 Prter lock  >ascii push  >ascii off
 Output push  Printer
 1/6" 1+ over - 1+ -2  and 6 /mod
 ?dup IF swap >r
 0 DO 3 nscr's 2+ 1+ page LOOP  r> THEN
 ?dup IF 1+ 2/ nscr's page THEN drop
 Prter unlock  ;
\ Block No. 81
\ Printspool                   02oct87re

\needs tasks  .( Tasker?!) \\

$100 $100 Task Printspool

: spool  ( from to --)
 Printspool 2 pass

 pthru
 stop ;

: endspool  ( --)
 Printspool activate
 stop ;










\ Block No. 82
\ 3scr's nscr's thru           06apr91pz
onlyforth print also forth

| : 3scr's  ( blk  - )
 cr  -cond +dark
  $8 spaces dup    3 .r
 $19 spaces dup 1+ 3 .r
 $19 spaces dup 2+ 3 .r
 cr  +cond  -dark  L/S C/L *  0 DO
  cr          dup  block I + C/L 1- type
   8 spaces dup 1+ block I + C/L 1- type
   8 spaces dup 2+ block I + C/L 1- type
 C/L +LOOP  drop  cr LF ;

| : nscr's ( blk1 n - blk2)  under 0
 DO dup 3scr's over + LOOP nip ;

: pthru  ( from to -)
 Output @ -rot  Printer Prter lock 1/8"
 10cpi  1+ over - 1+ 9 /mod
 ?dup IF swap >r
 0 DO 3 nscr's  page LOOP  r> THEN
 ?dup IF 1- 3 / 1+ 0
   DO dup 3scr's 3 + LOOP  THEN drop
 Prter unlock  Output ! ;
\ Block No. 83
\ printer routinen             07apr91pz

Only Forth also definitions

4 Constant B/scr

: .line  ( line# scr# --)
  block swap c/l * + c/l 1- type ;

: .===
 c/l 1- 0 DO  Ascii = emit  LOOP ;

: prlist ( scr# --)
 dup block drop    printer
 $E emit ." Screen Nr. " dup . $14 emit
 cr .=== cr
 l/s 0 DO I over .line cr LOOP drop
 .=== cr cr cr  display ;







\ Block No. 84
\ pfindex                      02oct87re

Onlyforth Print also

: pfindex  ( from to --)
 Prter lock  Printer  &12 "long
 +jump  findex  cr page  -jump
 Prter unlock  display  ;

















\ Block No. 85

























\ Block No. 86

























\ Block No. 87
\\ zu LongJsr fuer C16        clv08aug87


Das Speichermodell:

$0000 - $8000 : LowRAM
$8000 - $ffff : HighRAM  & ROM

Auf ROM schalten   Auf RAM schalten
sys kann wie jsr beutzt werden


ein ROM-Ruf der Art '0ffd2 sys'

 rom jsr ram  == $ff3e sta jsr $ff3f sta

das geht natuerlich nicht, wenn
HERE groesse $8000 ist. Warum wohl?

--- Beim c64 Lassen sich Basic und
 Betriebssystem getrennt schalten.
 Diese Makros sind nur fuer das
 Basic-Rom noetig.


\ Block No. 88
\\ zu LongJsr fuer C16      clv20aug87re

ACHTUNG! bei falscher Benutzung
         Systemabsturz


das Makro muss immer unter $8000 liegen

ein Aufruf der Form ' $ffd2 sysMacro'
gibt:
   pha
   $ff # lda  LONG1 2+ sta
   $d2 # lda  LONG1 1+ sta
   pla  LONG jsr
so hat mittels Umleitung doch noch der
Sprung ins drueberliegende ROM geklappt

sys entscheidet nun selbst, ob Umleitung
oder nicht.

ACHTUNG! DAS ZERO-Flag wird zerstoert!




\ Block No. 89
( transient Forth-6502 Assemclv20aug87re
( Basis: Forth Dimensions VOL III No. 5)

Der Assembler wird komplett auf den
Heap geladen und ist so nur bis zum
naechsten 'clear' oder 'save' benutzbar,
danach ist er komplett aus dem Speicher
entfernt. Er ist dann zwar nicht mehr
zu benutzen, aber er belegt auch nicht
unnoetig Speicherplatz.















\ Block No. 90
\ SCRIPTOR by Heinz Schnitter  07apr91pz

onlyforth decimal

\needs endcase  131 load  \ case

\needs Code 4 load \ tansient Ass

 30 +load   \ charset
 36 +load   \ umlaute

  57 load   \ tasker

  63 load   \ printer

onlyforth decimal

 1 29 +thru \ scriptor







\ Block No. 91
\ constants               hfs  07apr91pz


             8 constant tab
            80 constant char/line
            64 constant line/paper
            40 constant col/display
            24 constant row/display

             0 constant first.row
             0 constant first.col
row/display 1- constant last.row
col/display 1- constant last.col

             0 constant first.char
             0 constant first.line
char/line   1- constant last.char
line/paper  1- constant last.line


char/line line/paper 1+ *  2*
limit  first @  -  u< c/l 2* and >in +!
cr .( scriptor needs more block buffers)
cr abort

\ Block No. 92
\ constants/variables          07apr91pz

133 constant f.1
137 constant f.2
134 constant f.3
138 constant f.4
135 constant f.5
139 constant f.6
136 constant f.7
140 constant f.8


variable left.window  left.window  off
variable upper.window upper.window off
variable line.stack   line.stack   off










\ Block No. 93
\ line.list paper           hf 07apr91pz

create  line.list
        line/paper 1+ 2* allot

first @ constant paper

paper   char/line line/paper 1+ * +
        constant printer.buffer

: init.line.list
  paper
  [ line.list line/paper 1+ 2* + ]
  literal
  line.list
  do dup i ! char/line +
  2 +loop drop
  line.stack off
;
: initpaper
  paper
  [ char/line line/paper 1+ * ] literal
  bl fill init.line.list
;

\ Block No. 94
\ display.chars             hf 07apr91pz

hex

code display.chars ( adr len ---)
  2 # lda Setup jsr 0 # ldy
  [[  n cpy 0<>
 ?[[  n 2+ )y lda 0E716 jsr iny
  ]]? 0d4 stx 0d8 stx 1 # ldy
  Next jmp
end-code

decimal












\ Block No. 95
\ display-window          hfs  07apr91pz

: display-window   ( ---)
  at?
  0 0 at
  left.window  @
  upper.window @
  dup row/display + dup + line.list +
  swap dup + line.list +
  do
    i @ over +            ( char.addr )
    col/display           ( len)
    display.chars
  2 +loop
  drop
  over upper.window @ + 3 .r
  dup  left.window  @ + 3 .r
  at
;






\ Block No. 96
\ char.addr store.char    hfs  07apr91pz

: char.addr    ( ---)
  at? swap
  upper.window @ + dup +
  line.list + @
  left.window  @ + +
;
: store.char   ( 8b ---)
  char.addr c!
;














\ Block No. 97
\ ins.char del.char         hf 07apr91pz

  variable insert   insert off

| : char.to.move
    last.char left.window @ - col -
  ;

: ins.char
  char.addr
  dup dup 1+
  char.to.move
  move
  c!
;

: clr.char
  bl char.addr
  char.to.move
  over 2dup 1+ rot rot
  move
  + c!
;


\ Block No. 98
\ blank.line home              07apr91pz

: blank.line
  char.addr
  char.to.move 1+
  bl fill
;

: home
  upper.window off
  left.window  off
  0 0 at
;












\ Block No. 99
\ cursor.down               hf 07apr91pz

: cursor.down
  at?
  over last.row <
  if
   swap 1+ swap
  else
   upper.window @
   [ line/paper row/display - ] literal
   <
   if
    1 upper.window +!
   then
  then
  at
;








\ Block No. 100
\ cursor.up               hfs  07apr91pz

: cursor.up
  at?
  over 0>
  if
    swap 1- swap
  else
    upper.window @ 0>
    if
      -1 upper.window +!
    then
  then
  at
;










\ Block No. 101
\ cursor.right            hfhf 07apr91pz

: cursor.right
  at?
  dup last.col <
  if
   1+
  else
   left.window @
   [ char/line col/display - ] literal
   <
   if
    1 left.window +!
   then
  then
  at
;








\ Block No. 102
\ cursor.left del.char    hfs  07apr91pz

: cursor.left
  at?
  dup 0>
  if
    1-
  else
    left.window @ 0>
    if
      -1 left.window +!
    then
  then
  at
;

: del.char
  cursor.left
  bl store.char
;





\ Block No. 103
\ tab.down                hfs  07apr91pz

: tab.down
  at? swap
  upper.window @ + tab + last.line min
  1+ tab / tab * 1-
  upper.window @ -
  dup last.row >
  if
    last.row - upper.window +!
    last.row
  then
  swap at
;











\ Block No. 104
\ tab.up                  hfs  07apr91pz

: tab.up
  at? swap
  upper.window @ + tab - first.line max
  dup 0>
  if
  1+ tab / tab * 1- first.line max
  then
  upper.window @ -
  dup first.row <
  if
    upper.window +!
    first.row
  then
  swap at
;








\ Block No. 105
\ tab.left                hfs  07apr91pz

: tab.left
  at?
  left.window @ + tab - first.char max
  dup 0>
  if
  1+ tab / tab * 1- first.char max
  then
  left.window @ -
  dup first.col <
  if
    left.window +!
    first.col
  then
  at
;








\ Block No. 106
\ tab.right               hfs  07apr91pz

: tab.right
  at?
  left.window @ + tab + last.char min
  1+ tab / tab * 1-
  left.window @ -
  dup last.col >
  if
    last.col - left.window +!
    last.col
  then
  at
;











\ Block No. 107
\ nextline                  hf 07apr91pz

: next.line
  left.window off
  row dup
  last.row <
  if
   1+
  else
   upper.window @
   [ line/paper row/display - ] literal
   <
   if
    1 upper.window +!
   then
  then
  0 at
;







\ Block No. 108
\ ask.filename initdisplay     07apr91pz

 :      f.scratch " s:" ;
 :      f.device " @0:" ;
 create f.name  30 allot  f.name off
 :      f.type " ,s" ;
 :      f.read " ,r" ;
 :      f.write " ,w" ;

: .filename 24 7 at f.name count type ;

: ask.filename
  at?
  147 con!
  24  0 at ." file? "
  24  7 at
  f.name 1+ 20 c64expect
  span @ f.name c!
  at ;

: initdisplay
  left.window  off
  upper.window off
  147 con!  .filename  0 0 at ;

\ Block No. 109
\ open.file close.file      hf 07apr91pz

 : open.file ( f --) ( true write)
   8 2 busopen
   f.device     count bustype
   f.name       count bustype
   f.type       count bustype
   if   f.write count bustype
   else f.read  count bustype then
   busoff ;

 : close.file
   8 2 busclose ;

 : delete.file
   f.name c@
   if 8 15 busopen
      8 15 busout
      f.scratch count bustype
      f.name    count bustype
      busoff
      8 15 busclose
   then ;


\ Block No. 110
\ write.file read.file      hf 07apr91pz

: write.file
  f.name c@
  if  true  open.file
   8 2 busout
   [ line.list line/paper 2* + ] literal
   line.list
   do i @ char/line bustype
   2 +loop
  busoff close.file then ;

: read.file
  f.name c@
  if false open.file
   init.line.list
   8 2 busin
   [ line.list line/paper 2* + ] literal
   line.list
   do i @ char/line businput
   2 +loop
  busoff close.file then ;



\ Block No. 111
\ pick.line put.line           07apr91pz

: pick.line
  1 line.stack +!
  line.list upper.window @ row + 2* +
  dup @ swap dup 2+ swap dup
  [ line.list last.line 1+ 2* + ]
  literal
  swap - move
;

: put.line
  line.stack @
  if -1 line.stack +!
   line.list upper.window @ row + 2* +
   dup
   dup 2+ over
   [ line.list last.line 1+ 2* + ]
   literal
   swap - move
   !
  then
;


\ Block No. 112
\ directory               hfs  07apr91pz

| : .blocks bus@ bus@ 256 * + . ;

| : busdrop 0 do bus@ drop loop ;

: directory
  at?
  147 con!
  8 0 busopen " $" count bustype busoff
  8 0 busin   4 busdrop .blocks
  begin
   bus@ dup 0=
   if drop cr bus@ 1 busdrop 0<>
    if .blocks
    else 2 busdrop
    then
   else con!
   then 144 c@ stop? or
  until 8 0 busclose
  key drop
  .filename
  at
;

\ Block No. 113
\ initprinter               hf 07apr91pz

onlyforth print also forth

: initprinter
   prter lock
   convert
   12 "long  1/6"
   10cpi draft german CRET
   prter unlock ;


: control
   bl word 1+ c@ $1f and
   [compile] literal ;
  immediate restrict









\ Block No. 114
\ print.paper               hf 07apr91pz

100 100 task background

: print.paper
  background activate
  prter lock printer
  printer.buffer
  line.list line/paper 2* + line.list
  do pause
   i @ over char/line move
   char/line +
  2 +loop
  drop
  printer.buffer
  [ char/line line/paper * ] literal +
  printer.buffer
  do
    i char/line type cr
  char/line +loop
  page
  display  prter unlock
  begin stop repeat
;

\ Block No. 115
\                           hf 07apr91pz

: american.charset
  prter lock
   american             \ printer
  prter unlock
  rom.charset           \ display
  initdisplay
;

: german.charset
  prter lock
   german               \ printer
  prter unlock
  ram.charset umlaute   \ display
  initdisplay
;

: helptype 0 do cr count type loop ;






\ Block No. 116
\ help 70 load              hf 07apr91pz

: help at? 147 con!
" F7     tab.right    F8     tab.left"
" F5     tab.down     F6     tab.up"
" F3     put.line     F4     pick.line"
" F1     directory    F2     file?"
  4 helptype cr
" CTRL I Insert on    CTRL O Overwrite"
" CTRL W Write  file  CTRL R Read file"
" CTRL D Delete file  CTRL H Help me"
" CTRL A American     CTRL G German"
" CTRL P print Pica   CTRL N print Nlq"
" CTRL X eXit         CTRL B Blank line"
  6 helptype
"  on" count type cr
" HOME   HOMEposition"
" DEL    DELete character left  cursor"
" INST   INSerT blank     under cursor"
" CLR    CLeaR  character under cursor"
  4 helptype cr
  key drop .filename at
;


\ Block No. 117
\ scriptor f.1 ... f.8      hf 07apr91pz

: scriptor
  empty-buffers
  initpaper
  initprinter
  german.charset  \ printer und display
  multitask
  display-window
  begin
  key? not
  if display-window then
  key
  case  f.1 of directory    endof
        f.2 of ask.filename endof
        f.3 of put.line     endof
        f.4 of pick.line    endof
        f.5 of tab.down     endof
        f.6 of tab.up       endof
        f.7 of tab.right    endof
        f.8 of tab.left     endof

                          [


\ Block No. 118
\ scriptor controls         hf 07apr91pz

  ]
  control a of american.charset endof
  control b of blank.line       endof
  control g of german.charset   endof
  control i of insert on        endof
  control h of help             endof
  control n of prter lock
               LQ
               prter unlock
               print.paper      endof
  control o of insert off       endof
  control p of prter lock
               draft
               prter unlock
               print.paper      endof
  control r of read.file        endof
  control d of delete.file      endof
  control w of write.file       endof
  control x of prev off limit first !
      all-buffers 147 con! exit endof
                          [


\ Block No. 119
\ scriptor cr crsr home.... hf 07apr91pz

    ]   #cr of next.line    endof
         29 of cursor.right endof
        157 of cursor.left  endof
         17 of cursor.down  endof
        145 of cursor.up    endof
         19 of home         endof
        147 of clr.char     endof
         20 of del.char     endof
        148 of bl ins.char  endof  dup
  elsecase
    printable?
    if    insert @
     if   ins.char
     else store.char
     then at? dup last.col <
     if   1+ at
     else 2drop cursor.right
     then
    else  drop
    then
  endcase
  repeat ;

\ Block No. 120
\ charset Load-Screen       hf 07apr91pz

Onlyforth

\needs Code 4 load \ tansient Ass

 01 +load     \ hires graphic

| Vocabulary charset

charset also definitions

 02 05 +thru  \ hires graphic

Onlyforth










\ Block No. 121
\ >byte hbyte lbyte         hfs 170687

hex

Code >byte   ( 16b - 8bl 8bh)
 SP )Y lda pha txa SP )Y sta SP 2dec
 txa SP )Y sta pla Puta jmp   end-code

: hbyte >byte nip ;
: lbyte >byte drop ;

decimal













\ Block No. 122
\ Gr Constanten             hfs 170687

hex
| 0288 Constant scrpage
| E000 Constant bitmap
| D800 Constant charset
| C400 Constant colram
| C000 Constant vidram

   bitmap hbyte 40 /mod  3 swap -
| Constant bank
   20 / 8 *
   colram  hbyte 3F and 4 / 10 * or
| Constant bmoffs
   vidram  hbyte 3F and 4 / 10 *
   charset hbyte 3F and 4 / or
| Constant tmoffs

decimal






\ Block No. 123
\ Gr movecharset            hfs 170687

hex

| Code movecharset
 sei  32 # lda  01 sta   dey  8 # ldx
 N sty  N 2+ sty  D8 # lda  N 1+ sta
 charset hbyte # lda  N 3 + sta
  [[
   [[  N )Y lda  N 2+ )Y sta  iny  0= ?]
  N 1+ inc  N 3 + inc  dex  0= ?]
 36 # lda  01 sta  cli  iny
 Next jmp  end-code

decimal










\ Block No. 124
\ Gr text                   hfs 170687

hex


Code text
  1B # lda D011 sta
  tmoffs # lda D018 sta
  Next jmp

decimal














\ Block No. 125
\ Gr graphic forth          hfs 170687

hex

Forth definitions

: ram.charset
  charset movecharset
  DD00 c@ 00FC and bank or DD00 c!
  vidram hbyte scrpage c!
  text
;
: rom.charset
  Onlyforth
  17 D018 c!
  4 scrpage c!
  DD00 c@  3 or  DD00 c! ;

Charset definitions

decimal




\ Block No. 126
\ umlaute                   hf 07apr91pz

1 4 +thru






















\ Block No. 127
\   umlaute                 hf 07apr91pz

hex

create paragraph   0 c,
          03c c, 060 c, 07c c, 066 c,
          03e c, 006 c, 03c c, 000 c,

create upper.ae   1b c,
          0db c, 03c c, 066 c, 07e c,
          066 c, 066 c, 066 c, 000 c,

create upper.oe   1c c,
          066 c, 000 c, 03c c, 066 c,
          066 c, 066 c, 03c c, 000 c,

create upper.ue   1d c,
          066 c, 000 c, 066 c, 066 c,
          066 c, 066 c, 03c c, 000 c,

decimal




\ Block No. 128
\   umlaute                 hf 07apr91pz

hex

create lower.ae   5b c,
          066 c, 000 c, 03c c, 006 c,
          03e c, 066 c, 03e c, 000 c,

create lower.oe   5c c,
          000 c, 066 c, 000 c, 03c c,
          066 c, 066 c, 03c c, 000 c,

create lower.ue   5d c,
          000 c, 066 c, 000 c, 066 c,
          066 c, 066 c, 03c c, 000 c,

create sz         5e c,
          03c c, 066 c, 066 c, 07c c,
          066 c, 066 c, 06c c, 060 c,

decimal




\ Block No. 129
\   umlaute                 hf 07apr91pz

hex

| : change.char  ( addr --)
    dup  1+
    swap c@ 08 * 0d800 +
    08 move
  ;

decimal














\ Block No. 130
\   umlaute                hfs 07apr91pz

hex

: umlaute
  0dc0e
  dup c@ 0fe and swap c! \ interrupt off
  01 c@
  01 c@ dup 0fc and 01 c! \ ram ein
  paragraph   change.char
  upper.ae    change.char
  upper.oe    change.char
  upper.ue    change.char
  lower.ae    change.char
  lower.oe    change.char
  lower.ue    change.char
  sz          change.char
  01 c!                   \ ram aus
  0dc0e
  dup c@ 01  or  swap c! \ interrupt on
;

decimal


\ Block No. 131
\ ein case-of-konstrukt        07apr91pz
| variable caselink

  : case ( -- ) ( comp: -- n 10 )
     caselink @ caselink off  10 ;
    immediate restrict

  : of ( n1 n2 -- n1/-- )
     compile case?
     compile ?branch >mark 11 ;
    immediate restrict

  : endof ( -- )
     compile branch  here
     caselink @ ,  caselink !
     11 ?pairs  >resolve ;
    immediate restrict

  : elsecase ;  immediate restrict

  : endcase   10 ?pairs
     caselink @ BEGIN ?dup WHILE
     dup @ swap >resolve REPEAT
     caselink ! ;
    immediate restrict
\ Block No. 132
\\ zu tracer:loadscreen       clv12oct87


***Fuer die naechste u4th-Version****

Falls jemand mal die <IP IP>-Sache
ordnet und mit Atari vereinheitlicht,
hier ein paar kritische
Beispiele zum Testen:

| : aa dup drop ;
| : bb aa ;
\\
debug bb
trace' aa

trace' Forth



Mein Verdacht: Das IP 2inc findet bei
CBM/Atari vorher bzw. nachher statt.



\ Block No. 133
\\ zu tracer:wcmp variables   clv04aug87



benutzt in der Form: adr1 adr2 wcmp
vergleicht das ganze Wort. Danach
ist: Carry=1  : (adr1) >= (adr2)
     Carry=0  : (adr1) <  (adr2)
mit den andern Flags ist nix anzufangen


Temporaer Speicher fuer W
Bereich, in dem getraced werden soll
Flag: ins Wort rein  Flag: trace ja/nein
hab ich vergessen    Schachtelungstiefe










\ Block No. 134
\ zu tracer:cpush oneline     clv04aug87




sichert LEN bytes ab ADDR auf dem
Return-Stack. Das naechste UNNEST
oder EXIT tut sie wieder zurueck


die neue Hauptbefehlsschleife.
Ermoeglicht die Eingabe einer Zeile.



ermittelt den zu tracenden Bereich









\ Block No. 135
\ zu tracer:step tnext        clv04aug87

wird am Ende von TNEXT aufgerufen,
 um TRAP? wieder einzuschalten und
 die angeschlagene NEXT-Routine
 wieder zu reparieren.




Diese Routine wird auf die NEXT-Routine
gepatched und ist das Kernstueck.
 Wenn nicht getraced wird: ab
 Ins aktuelle Wort rein?
    nein: ist IP im debug-Bereich?
          nein: dann ab
    ja:   dann halb(!) loeschen

 trap? ausschalten ( der Tracer soll
 sich schliesslich nicht selbst tracen,
 wo kommen wir denn da hin!)




\ Block No. 136
\ tracer:..tnext              clv04aug87

Forth-Teil von TNEXT
 ins aktuelle Wort rein?
    ja: Debug-Bereich pushen, neuen
        Schachtelungstiefe incr.
 STEP soll nachher ausgefuehrt werden
 PUSHed alle wichtige Sachen

 gibt eine Informationszeile aus



 PUSHed nochmehr Zeug

 PUSHed den Return-Stack-Pointer !!
 und tut so, als waer der RStack leer
 Haengt ONELINE in die
 Haupt-Befehls-Schleife und ruft sie auf






\ Block No. 137
\ zu tracer:do-trace traceableclv12oct87

installiert (patched) TNEXT in NEXT
 (NEXT ist die innerste Routine,
  zu der jedes Wort zurueckkehrt)




guckt, ob Wort getraced werden kann
  und welche adr dazugehoert
 :      -def.  <IP=cfa+2
 INPUT: -def.  <IP aus input-Vektor

 OUTPUT:-def.  <IP aus output-Vektor

 DEFER  -def.  <IP aus [cfa+2]

 DOES>  -def.  <IP=[cfa]+3

 alle anderen Definitionen gehen nicht




\ Block No. 138
\ zu tracer:Benutzer-Worte    clv12oct87

NEST erlaubt das Hineinsteigen in
     ein getracedes Worte

UNNEST fuehrt das Wort zuende und
     traced dann wieder.

ENDLOOP traced erst hinterm naechsten
     Wort wieder (z.B. bei LOOPs)

UNBUG schaltet jegliches getrace ab.






DEBUG <word> setzt den zu tracenden
     Bereich. Wenn <word> anschliessend
     ausgefuehrt wird, meldet sich der
     Tracer.

TRACE' fuehrt <word> gleich noch aus.

\ Block No. 139
\\ zu tools for decompil  01oct87clv/re)

\ Wenn zum Beispiel das Wort


: test 5   0   DO    ." magst Du mich ?"
    key Ascii j =
    IF   ." selber schuld " leave
    ELSE ." Aber bestimmt " THEN LOOP
  ." !" ;

\\ beguckt werden soll, dann gehts so:


  bitte umblaettern..>










\ Block No. 140
\  zu tools for decompil  01occlv10oct87

cr
tools
' test
k      n c n   n b   n s
    n   n     c n
    n b  n   s              n
    n b    n   s                   n b
  n s   n















\ Block No. 141

























\ Block No. 142
                               07apr91pz



                          -->
Dieses SAVE hat mir schon x-mal den
transienten Assembler, den ich noch
brauchte, rausgeschmissen. Deshalb
habe ich es auskommentiert.
















\ Block No. 143
\ savesystem                   07apr91pz

| : (savsys ( adr len -- )
 [ Assembler ] Next  [ Forth ]
 ['] pause  dup push  !  \ singletask
 i/o push  i/o off  bustype ;

: savesystem   \ name muss folgen
    \ Forth-Kernal vorbereiten:
 scr push  1 scr !  r# push  r# off
    \ Editor vorbereiten:
 [ \needs Editor                  (
 Editor ]  stamp$ dup push off
           (pad   dup push off [  ( )
 ]  \ nun geht's los:
 save
 8 2 busopen  0 parse bustype
 " ,p,w" count bustype  busoff
 8 2 busout  origin $17 -
 dup  $100 u/mod  swap bus! bus!
 here over - (savsys  busoff
 8 2 busclose
 0 (drv ! derror? abort" save-error" ;

Onlyforth
\ Block No. 144
\ bamallocate, formatdisk      07apr91pz

: bamallocate ( --)
 diskopen ?exit
 pad &18 0 readsector 0=
  IF pad 4 + $8C erase
     pad &18 0 writesector drop
  THEN  diskclose
 8 &15 busout " i0" count bustype
 busoff ;

: formatdisk ( --)  \ Name muss folgen
 8 &15 busout " n0:" count bustype
 0 parse bustype busoff
 derror? ?exit
 bamallocate ;

\ z.B.: formatdisk ultraFORTH,id






FORTH-GESELLSCHAFT  (c) bp/ks/re/we/clv
\ Block No. 145
\ copydisk                    007apr91pz

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

\ Block No. 146
                               07apr91pz
























\ Block No. 147

























\ Block No. 148
== Formular zum Laden eines == 07apr91pz
====== Druckertreibers =======

Es brauchen nur die mit '*' markierten
Stellen veraendert werden:

* Hier waehlt man durch Auskommentieren
* mit '\' die Schnittstelle: Serieller
* Bus oder Userport mit entweder PA2
* oder PC als Strobe-Leitung ( Bei den
* mir bekannten Userport-Kabeln wird
* PA2 verwendet.)


  * Verwendet man den seriellen Bus, so
  * muss hier die Geraete- und
    Sekundaeradresse festgelegt werden.

  ** Schliesslich muss hier die Nummer
     eines Loadscreens fuer Drucker-
     Steuerzeichen einfach angegeben
     werden. Er wird vom 2nd loadscreen
     an der richtigen Stelle geladen.


\ Block No. 149
Loadscreen des Treibers        07apr91pz

setzt order auf FORTH FORTH ONLY   FORTH

Im 1. loadscreen muessen (s (u.xx etc
definiert werden !



fuer multitasking


Der naechsten Screen ist nur fuer den
 seriellen Bus
Treiber Teil 1
Die Nummer des Screens mit den Printer-
 Controls muss auf dem Stapel liegen.
Treiber Teil 2


Das anschliessende CLEAR sollte man
lieber von Hand durchfuehren. Ich
aergere mich jedenfalls immer, wenn mir
irgendein Loadscreen den transienten
Assembler rausschmeisst ...
\ Block No. 150
Beim seriellen Bus ist die Ausg06apr91pz
einzelnen Zeichens zu langsam


Bufferlaenge
Buffer fuer Zeichen zum Drucker

ein Zeichen zum Buffer hinzufuegen


Buffer voll?

Buffer ausdrucken und leeren




Hauptausgaberoutine fuer seriellen Bus
Zeichen merken
ist es ein Formfeed?
  ja, Buffer ausdrucken incl. Formfeed
ist es ein Linefeed?
oder ein CR oder ist der Buffer voll?
  ja, Buffer ausdrucken, CR/LF merken

\ Block No. 151
                               06apr91pz

Zeichenausgabe am Userport
mit Strobe an PA2:
Zeichen auf Port  , Strobe-Puls
 'von Hand' erzeugen    und auf die
negative Flanke von Busy bzw. Acknlg
warten.

Zeichenausgabe am Userport
mit Strobe an PC:
letzte negative Flanke abwarten (s.o.)
Zeichen auf Port; Strobe wird vom CIA-
Baustein automatisch erzeugt.

Initialisierung der Schnittstelle:
        Port auf Ausgabe
        Strobe auf High
        Strobe geben & Port auf Ausgabe
 Seriell braucht keine Initialisierung





\ Block No. 152
                               06apr91pz

wandelt Commodore's Special-Ascii in
ordinaeres ASCII



Wandlung umschaltbar machen

Wandlung einschalten
Wandlung ausschalten

Default: ein



Position des Druckkopfes fuer at & at?








\ Block No. 153
                               06apr91pz
Routinen zur Druckerausgabe      Befehl

ein Zeichen auf Drucker          emit
CR und LF auf Drucker            cr
ein Zeichen loeschen (?!)        del
Formfeed ausgeben                page
Drucker auf zeile und spalte     at
positionieren, wenn noetig,
neue Seite


Position feststellen             at?
Zeichenkette ausgeben            type


erzeugt die Ausgabetabelle >printer








\ Block No. 154
                               06apr91pz

Routinen fuer Drucker
und Bildschirm gleichzeitig (both)

Ausgabe erfolgt zuerst auf Bildschirm
( Routinen von DISPLAY )
dann auf Drucker
( Routinen von >PRINTER )

erzeugt die Ausgabetabelle >both

Von Forth aus zugaenglich:

legt Ausgabe auf Drucker


legt Ausgabe auf Drucker und Bildschirm







\ Block No. 155
printer controls fuer RX80     07apr91pz

'printer controls' wird vom loadscreen
des Treibers geladen und muss folgende
Worte zur Verfuegung stellen:
CRET, LF, FF, DEL und PRINIT. PRINIT
muss als erstes INIT-P! ausfuehren.


gibt Escape-Sequenzen an Drucker


Zeilenabstand in Zoll

Superscript und Subscript ausschalten
Perforation ueberspringen ein/aus









\ Block No. 156
                               06apr91pz

Escape + 2 Zeichen

 nur fuer Goerlitz-Interface

spezieller Epson-Steuermodus

  Kopie des Drucker-Steuer-Registers

schaltet Bit in Steuer-Register ein



schaltet Bit in Steuer-Register aus



Diese Steuercodes muessen fuer andere
Drucker mit Hilfe von ctrl:, esc: und
ESC2 umgeschrieben werden

Zeichenbreite in characters per inch
eventuell durch Elite, Pica und Compress
ersetzen
\ Block No. 157
                               07apr91pz



Aufruf z.B.mit 66 lines
Aufruf z.B mit 11 "long
Zeichensaetze, beliebig erweieterbar


Initialisierung ...
   ... der Schnittstelle
  CBM-ASCII-Wandlung an
  Drucker mit Standardwerten einstellen


Diese Initialisierung fuer das
Goerlitz-Interface kann man noch in
prinit einbauen, wenn man ein solches
hat. Was das alles im einzelnen
bedeutet, weiss ich nicht. Der Code
stammt von Georg Rehfeld (glaub ich).




\ Block No. 158
                               07apr91pz
Diesen Screen habe ich verbrochen, ohne
den 1526 besonders zu kennen. Ich hoffe,
es taugt trotzdem ein bisschen.

CRET und ESC ganz normal
Commodore hat Autolinefeed nach CRET


Einstellen der Seitenlaenge

Seitenvorschub: etwas getrickst


DEL: noch mehr getrickst; bewirkt, dass
     pdel gar nichts tut.



               keine CBM-ASCII-Wandlung

Sekundaeradresse im Loadscreen
auf 7 setzen


\ Block No. 159
                               07apr91pz
Die CP80-controls habe ich praktisch
unveraendert von Claus Vogt uebernommen.










                     Wieder das ESC ' '
                     fuer Goerlitz





Die Schmalschrift habe ich von
comp(ressed) in cond(ensed) umbenannt



\ Block No. 160
                               07apr91pz















noch ein extra PSPACES








\ Block No. 161
                              c07apr91pz







Schmalschrift an/aus




'Perforation ueberspringen' an/aus
           Pica                Elite
           Microschrift
Kursivschrift an/aus
Fettdruck an/aus
Druckerreset
Zeilenabstand 1/8 Zoll         1/6 Zoll

    n/60 Zoll, z.B. 6 /60" = 1/10 Zoll



\ Block No. 162
                               07apr91pz




Breitschrift an
             aus
Hochschrift an
            aus
Unterstreichen an
               aus
Superscript an ( Exponenten- )
Subscript an   ( Index- )
Sub- bzw Superscript aus
Schoenschrift
Schnellschrift
Amerikanischer Zeichensatz
Deutscher

Seitenlaenge
  in Zeilen:  72 lines
  in Zoll:    12 "long

Keine Aenderung der Druckereinstellung

\ Block No. 163
                               06apr91pz



gibt 2 Screens nebeneinander aus
Screennummer in Fettschrift und 17cpi


formatierte Ausgabe der beiden Screens




gibt die Screens so aus:   1    4
                           2    5
                           3    6
gibt die Screens von from bis to aus
Ausgabegeraet merken und Printer ein
errechnet Druckposition der einzelnen
Screens und gibt sie nach obigem Muster
aus




\ Block No. 164




wie 2scr's (mit Shadow)








wie nscr's (mit Shadow)
                           screen Shadow
                           scr+1  Sh+1


wie pthru  (mit Shadow)






\ Block No. 165




Dasselbe nochmal fuer Standard-Forth
Screens mit 16 Zeilen zu 64 Zeichen







Siehe oben


Wie pthru fuer Standard-Screens








\ Block No. 166
Drucken im Untergrund

Der Tasker wird gebraucht

Der Arbeitsbereich der Task wird erzeugt

Hintergrund-Druck ein
 von/bis werden an die Task gegeben
 beim naechsten PAUSE fuehrt die
 Task pthru aus und legt sich dann
 schlafen.

Hintergrund-Druck abbrechen
 die Task wird nur aktiviert,
 damit sie sich sofort wieder schlafen
 legt.









\ Block No. 167
                               06apr91pz



gibt 2 Screens nebeneinander aus
Screennummer in Fettschrift und 17cpi


formatierte Ausgabe der beiden Screens




gibt die Screens so aus:   1    4
                           2    5
                           3    6
gibt die Screens von from bis to aus
Ausgabegeraet merken und Printer ein
errechnet Druckposition der einzelnen
Screens und gibt sie nach obigem Muster
aus




\ Block No. 168




wie 2scr's (mit Shadow)








wie nscr's (mit Shadow)
                           screen Shadow
                           scr+1  Sh+1


wie pthru  (mit Shadow)






\ Block No. 169




Dasselbe nochmal fuer Standard-Forth
Screens mit 16 Zeilen zu 64 Zeichen







Siehe oben


Wie pthru fuer Standard-Screens








