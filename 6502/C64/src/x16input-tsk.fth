
include vf-lbls-cbm.fth

7f fthpage

\ X16 labels

0ffd2 >label ConOut
0febd >label KbdbufPeek
0feab >label ExtApi
09f2c >label BrdCol
 0376 >label BkgPenCol  \ aka color
 0381 >label CurFlg  \ aka qtsw
 0385 >label InsCnt  \ aka insrt

1 >label RomBank
0 >label RamBank

  037B >label blnsw  \ C64: $cc
\   037C >label blnct  \ C64: $cd
\   037D >label gdbln  \ C64: $ce
\   037E >label blnon  \ C64: $cf
\   0262 >label pnt    \ C64: $d1
\   0380 >label pntr   \ C64: $d3
\   0373 >label gdcol

\ C64 labels that X16 doesn't have:

\ 028a >label KeyRep  \ aka rptflg


\ *** Block No. 129, Hexblock 81
81 fthpage

\ X16 c64key? getkey

Code c64key? ( -- flag)
 KbdbufPeek jsr
 txa  pha
 Push jmp  end-code

Code getkey  ( -- 8b)
 GETIN jsr
 Push0A jmp   end-code


\ *** Block No. 130, Hexblock 82
82 fthpage

\ X16 curon curoff

Code curon   ( --)
  blnsw stx  Next jmp  end-code

Code curoff   ( --)
  blnsw sty  Next jmp  end-code


\ *** Block No. 131, Hexblock 83
83 fthpage

( #bs #cr ..keyboard         clv12.4.87)

: c64key  ( -- 8b)
 curon BEGIN pause c64key?  UNTIL
 curoff getkey ;

14 Constant #bs   0D Constant #cr

: c64decode
 ( addr cnt1 key -- addr cnt2)
  #bs case?  IF  dup  IF del 1- THEN
                            exit  THEN
  #cr case?  IF  dup span ! exit THEN
  >r  2dup +  r@ swap c!  r> emit  1+ ;

: c64expect ( addr len1 -- )
 span !  0
 BEGIN  dup span @  u<
 WHILE  key  decode
 REPEAT 2drop space ;

Input: keyboard-pause   [ here input ! ]
 c64key c64key? c64decode c64expect ;
