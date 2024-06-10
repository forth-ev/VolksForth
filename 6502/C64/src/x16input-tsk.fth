\ Sample: a changed input that would run the tasker
\ even while waiting for keyboard input, similar to
\ the keyboard input on C16 and C64:

include trns6502asm.fth

\ X16 labels

 $037B >label blnsw  \ C64: $cc

\ X16 curon curoff

Code curon   ( --)
  blnsw stx  Next jmp  end-code

Code curoff   ( --)
  blnsw sty  Next jmp  end-code

: c64decode
 ( addr cnt1 key -- addr cnt2)
  #bs case?  IF  dup  IF del 1- THEN
                            exit  THEN
  #cr case?  IF  dup span ! exit THEN
  >r  2dup +  r@ swap c!  r> emit  1+ ;

: c64expect ( addr len1 -- )
 span !  0
 BEGIN  dup span @  u<
 WHILE  curon key curoff  decode
 REPEAT 2drop space ;

Input: keyboard-tasker  [ here input ! ]
 x16key x16key? c64decode c64expect ;

keyboard-tasker
