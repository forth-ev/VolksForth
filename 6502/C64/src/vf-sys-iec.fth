
\ *** Block No. 136, Hexblock 88
88 fthpage

( i/o busoff                  10may85we)

Variable i/o  0 i/o !  \ Semaphore

Label LsnDev 0 c,
Label TlkDev 0 c,

Code busoff  ( --)
 LsnDev lda  0<> ?[ LsnDev stx  UNLSN jsr ]?
 TlkDev lda  0<> ?[ TlkDev stx  UNTLK jsr ]?
Label unlocki/o  1 # ldy  0 # ldx
 ;c:  i/o unlock ;

Label nodevice     0 # ldx  1 # ldy
 ;c:  busoff   buffers unlock
      true Abort" no device" ;


\ *** Block No. 137, Hexblock 89
89 fthpage

\ ?device                     clv12jul87

Label (?dev  ( a: dev )
 \ Clear IOStatus because it isn't cleared by LISTEN or TALK
 (C64 IOStatus stx ( ) (C16 IOStatus stx ( )
 (X16 pha  1 # lda  ExtApi jsr  pla ( )
 \ It's unclear in which situation or use case the following
 \ workaround for a C16 OS error is needed. The v4th tests pass
 \ even with the following line removed.
 (C16 CurDev sta ( )  \ current device number - because of error in OS
 LISTEN jsr
 60 # lda  SECOND jsr  UNLSN jsr
 READST jsr  0<> ?[ pla pla nodevice jmp ]?
 rts    end-code

 Code (?device  ( dev --)
 SP X) lda  (?dev jsr  SP 2inc
 unlocki/o jmp  end-code

: ?device  ( dev -- )
 i/o lock  (?device ;

 Code (busout  ( dev 2nd -- )
 2 # lda  Setup jsr
 N 2+ lda  (?dev jsr
 N 2+ lda  LISTEN jsr
 N lda  60 # ora SECOND jsr
 N 2+ ldx  LsnDev stx
 xyNext jmp  end-code


\ *** Block No. 138, Hexblock 8a
8a fthpage

\ busout/open/close/in        clv12jul87

: busout    ( dev 2nd -- )
 i/o lock (busout ;

: busopen   ( dev 2nd -- )
 0F0 or busout ;

: busclose  ( dev 2nd -- )
 0E0 or busout busoff ;

 Code (busin  ( dev 2nd -- )
 2 # lda  Setup jsr
 N 2+ lda  (?dev jsr
 N 2+ lda  TALK jsr
 N lda  60 # ora (C16 $ad sta ( )
 TKSA jsr
 N 2+ ldx  TlkDev stx
 xyNext jmp end-code

: busin  ( dev 2nd -- )
 i/o lock  (busin ;


\ *** Block No. 139, Hexblock 8b
8b fthpage

( bus-!/type/@/input derror?  24feb85re)

Code bus!  ( 8b --)
 SP X) lda  CIOUT jsr  (xydrop jmp
 end-code

: bustype  ( adr n --)
 bounds  ?DO  I c@ bus!  LOOP pause ;

Code bus@  ( -- 8b)
 ACPTR jsr Push0A jmp  end-code

: businput  ( adr n --)
 bounds  ?DO  bus@ I c! LOOP pause ;

: derror?  ( -- flag )
 disk $F busin bus@  dup Ascii 0 =
   IF drop BEGIN bus@ drop i/o-status? UNTIL false
   ELSE BEGIN emit bus@ i/o-status? UNTIL emit true THEN
 busoff ;

