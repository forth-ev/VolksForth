\ *** Block No. 62, Hexblock 3e

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
