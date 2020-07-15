PRT2C ok
14 0 pall    ATARIIO.FB Scr 0 Dr 0 
 0 
 1 
 2 
 3 
 4 
 5 
 6 
 7 
 8 
 9 
10 
11 
12 
13 
14 
15 
   ATARIIO.FB Scr 1 Dr 0 
 0 \ loadscreen fuer ATARI 8bit                          cas11aug06
 1 \ 800 / 600 XL / 800 XL / 1200 XL / 130 XE / 65 XE / 800 XE
 2 
 3  1  &14 +thru
 4 
 5 
 6 
 7 
 8 
 9 
10 
11 
12 
13 
14 
15 
   ATARIIO.FB Scr 2 Dr 0 
 0 \ 65KEY? GETKEY                                       cas09jan07
 1 
 2 | $02FC Constant CH
 3 | CODE 65KEY? ( -- FLAG) CH lda clc 1 # adc push0a jmp end-code
 4 
 5  LABEL GETCHK $E425 lda pha $E424 lda pha rts
 6 | CODE GETKEY  ( -- 8B) $FF sty $FE stx GETCHK jsr
 7                         $FE ldx $FF ldy  push0a jmp end-code
 8 
 9 | $02F0 Constant CRSINH
10 | CODE CURON   ( --) 01 # lda
11      LABEL CRS01    CRSINH sta NEXT JMP END-CODE
12 | CODE CUROFF  ( --) 00 # lda CRS01 JMP END-CODE
13 
14 | : 65KEY  ( -- 8B)
15     CURON BEGIN PAUSE 65KEY?  UNTIL CUROFF GETKEY ;
   ATARIIO.FB Scr 3 Dr 0 
 0 \ DECODE EXPECT KEYBOARD      BP28MAY85)              cas09jan07
 1 $7E CONSTANT #BS   $9B CONSTANT #CR  &27 CONSTANT #ESC
 2 
 3 | : 65DECODE  ( ADDR CNT1 KEY -- ADDR CNT2)
 4    #BS CASE?  IF  DUP  IF DEL 1- THEN EXIT  THEN
 5    #CR CASE?  IF  DUP SPAN ! EXIT THEN
 6    >R  2DUP +  R@ SWAP C!  R> EMIT  1+ ;
 7 
 8 | : 65EXPECT ( ADDR LEN1 -- )  SPAN !  0
 9    BEGIN  DUP SPAN @  U<
10    WHILE  KEY  DECODE
11    REPEAT 2DROP SPACE ;
12 
13 INPUT: KEYBOARD   [ HERE INPUT ! ]
14  65KEY 65KEY? 65DECODE 65EXPECT [
15 
   ATARIIO.FB Scr 4 Dr 0 
 0 \ (emit 65emit        )                               cas09jan07
 1 
 2 LABEL OUTCHK
 3   $E407 lda pha $E406 lda pha txa rts
 4 
 5 | Code (emit ( 8b -- ) $FF sty $FE stx
 6                      SP X) lda tax OUTCHK jsr
 7                      $FE ldx $FF ldy (drop jmp end-code
 8 
 9 
10 
11 
12 
13 
14 
15 
   ATARIIO.FB Scr 5 Dr 0 
 0 \ EMIT CR DEL PAGE AT AT?     25JAN85RE)              cas09jan07
 1 
 2 | &40 Constant c/row
 3 
 4 | : 65emit   ( 8b -- ) (emit ;
 5 
 6 | : 65CR     #CR 65emit ;
 7 
 8 | : 65DEL    #bs 65emit  SPACE  #bs 65emit ;
 9 
10 | : 65PAGE   &125 EMIT ;
11 
12 | : 65at ( row col -- ) $55 ! $54 C! ;
13 
14 | : 65AT?  ( -- ROW COL ) $54 C@ $55 @ ;
15 
   ATARIIO.FB Scr 6 Dr 0 
 0 \                                                     cas09jan07
 1 
 2 | : 65type ( adr len -- ) bounds ?DO I c@ emit LOOP ;
 3 
 4 
 5 
 6 
 7 
 8 
 9 
10 
11 
12 
13 
14 
15 
   ATARIIO.FB Scr 7 Dr 0 
 0 \ TYPE DISPLAY (BYE       BP  28MAY85RE)              cas09dec05
 1 
 2 OUTPUT: DISPLAY   [ HERE OUTPUT ! ]
 3  65EMIT 65CR 65TYPE 65DEL 65PAGE 65AT 65AT? [
 4 
 5 \ fix dosini vector and jump through dosvec
 6 | code (bye warmboot 1+ lda  $0C sta  warmboot 2+ lda
 7             $0D sta $000A ) jmp end-code
 8 
 9 
10 
11 
12 
13 
14 
15 
   ATARIIO.FB Scr 8 Dr 0 
 0 \ FileInterface                                       cas09jan07
 1 
 2 
 3 \  definitions for fileinterface
 4 
 5 &4 CONSTANT R/O   &8 CONSTANT W/O   &12 CONSTANT R/W
 6 3 CONSTANT IO-OPEN    5 CONSTANT IO-GETREC 7 CONSTANT IO-GETCHR
 7 9 CONSTANT IO-PUTREC $B CONSTANT IO-PUTCHR $C CONSTANT IO-CLOSE
 8 
 9 $340 CONSTANT ICFLG  $342 CONSTANT ICCOM  $343 CONSTANT ICSTA
10 $344 CONSTANT ICBAL  $345 CONSTANT ICBAH  $348 CONSTANT ICBLL
11 $349 CONSTANT ICBLH  $34A CONSTANT ICAX1  $34B CONSTANT ICAX2
12 $E456 CONSTANT CIOV
13 
14 
15 
   ATARIIO.FB Scr 9 Dr 0 
 0 \  definitions for fileinterface                      cas09jan07
 1 
 2 label freeiocb0  70 # lda label freeiocb2 tay ICFLG ,y lda
 3       $FF # cmp 0<> ?[ tya sec $10 # sbc freeiocb2 bne ]?
 4       tya rts
 5 
 6 | code freeiocb freeiocb0 jsr .a lsr .a lsr .a lsr .a lsr pha
 7               push0a jmp end-code
 8 
 9 label getfileid sp x) lda .a ASL .a ASL .a ASL .a ASL tay rts
10 
11 label getparam 2 # ldy  sp )y lda  ICBLL ,x sta
12                    iny  sp )y lda  ICBLH ,x sta
13                    iny  sp )y lda  ICBAL ,x sta
14                    iny  sp )y lda  ICBAH ,x sta
15                    rts
   ATARIIO.FB Scr 10 Dr 0 
 0 \  definitions for fileinterface                      cas13dec05
 1 
 2 code close-file  getfileid jsr tax IO-CLOSE # lda ICCOM ,x sta
 3          CIOV jsr  sp 2inc ICSTA ,x lda  0>= ?[ 0 # lda ]? pha
 4                  PUSH0A jmp end-code
 5 
 6 code open-file   freeiocb0 jsr tax  IO-OPEN # lda ICCOM ,y sta
 7                  4 # ldy  sp )y lda  ICBAL ,x sta
 8                      iny  sp )y lda  ICBAH ,x sta
 9                  0 # ldy  sp )y lda  ICAX1 ,x sta
10                      tya             ICAX2 ,x sta
11      CIOV jsr sp 2inc 0 # ldy ICSTA ,x lda  sp )y sta
12           0>= ?[ 0 # lda sp )y sta ]? 0 # lda tay iny sp )y sta
13                  iny iny sp )y sta txa clc .a lsr .a lsr .a lsr
14                  .a lsr dey sp )y sta xynext jmp end-code
15 
   ATARIIO.FB Scr 11 Dr 0 
 0 \  definitions for fileinterface                      cas11aug06
 1 
 2 code read-file ( caddr u fileid -- u2 ior )
 3      getfileid jsr  tax  getparam jsr
 4         IO-GETCHR # lda  ICCOM ,x sta
 5         CIOV jsr   sp 2inc  0 # ldy
 6         ICSTA ,x lda  sp )y sta
 7         0>= ?[ 0 # lda  sp )y sta ]?  tya iny sp )y sta
 8         clc  iny  ICBLL ,x lda  sp )y sta
 9              iny  ICBLH ,x lda  sp )y sta  xynext jmp end-code
10 
11 
12 
13 
14 
15 
   ATARIIO.FB Scr 12 Dr 0 
 0 \  definitions for fileinterface                      cas11aug06
 1 
 2 code read-line ( caddr u fileid -- u2 flag ior )
 3    getfileid jsr  tax  getparam jsr
 4       IO-GETREC # lda  ICCOM ,x sta
 5       CIOV jsr 0 # ldy
 6       ICSTA ,x lda
 7        0>= ?[ tya ]? sp )y sta
 8    4 # ldy ICBLL ,x lda  sp )y sta
 9    ICBAL ,x adc  tay dey n sty  5 # ldy ICBLH ,x lda sp )y sta
10    ICBAH ,x adc  n 1+ sta  0 # lda  tay  n )y sta iny
11    sp )y sta iny sp )y sta iny sp )y sta xynext jmp end-code
12 
13 
14 
15 
   ATARIIO.FB Scr 13 Dr 0 
 0 \  definitions for fileinterface                      cas11aug06
 1 
 2 code write-file ( caddr u fileid -- ior )
 3      getfileid jsr  tax  getparam jsr
 4         IO-PUTCHR # lda  ICCOM ,x sta
 5         CIOV jsr   sp 2inc sp 2inc  0 # ldy
 6         ICSTA ,x lda  sp )y sta
 7         0>= ?[ 0 # lda  sp )y sta ]?
 8         xynext jmp end-code
 9 
10 
11 
12 
13 
14 
15 
 ok
display PRT2C   ATARIIO.FB Scr 14 Dr 0 
 0 \  definitions for fileinterface                      cas09jan07
 1 VARIABLE SOURCE-ID  0 SOURCE-ID !
 2 | $580 CONSTANT FNBUF
 3 : REFILL tib $50 erase tib $50 SOURCE-ID @ READ-LINE
 4          ROT 1 - #tib ! >in off nip ;
 5 : INCLUDE-FILE ( fileid -- )
 6   SOURCE-ID ! BEGIN REFILL $80 < WHILE INTERPRET .STATUS REPEAT
 7   SOURCE-ID @ CLOSE-FILE ABORT" File Error" ;
 8 : INCLUDED ( caddr u -- )
 9   SOURCE-ID @  >R   R/O OPEN-FILE DUP $80 < IF DROP
10   INCLUDE-FILE HERE $50 ERASE #TIB @ >IN ! ELSE
11   ." FileError:" . ABORT THEN R> SOURCE-ID ! ;
12 : FILE" FNBUF $50 BL FILL HERE $50 BL FILL ASCII " WORD
13         COUNT FNBUF SWAP CMOVE FNBUF 0 ;
14 : INCLUDE" ( FNAME ) FILE" INCLUDED ; IMMEDIATE
15 
