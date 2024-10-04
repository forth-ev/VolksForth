\ Taskdemo for X16

: taskmark ; \needs cbm>scr : cbm>scr ;

\ This method of writing directly into the X16's screen memory
\ is a bit hacky and relies on the default settings that the
\ Kernal uses for text mode.
: scrstart  ( -- adr)  $b000 ;
: ctrl@  ( -- b )  $9f25 c@ ;
: addr-sel@ ( -- 0/1 )  ctrl@  1 and ;
: datax!  ( c -- )  $9f23 addr-sel@ +  c! ;
: addrx-l/m!  ( u -- )  $9f20 ! ;
: scr!  ( scrcode vram-addr -- )
  addrx-l/m!  datax! ;

Variable counter  counter off

$100 $100 Task Background

: >count  ( n -)
 Background 1 pass
 counter !
 BEGIN  counter @  -1 counter +! ?dup
 WHILE  pause 0 <# #s #>   dup >r
  0 DO  pause  dup I + c@  cbm>scr
        scrstart I 2* +  scr!  LOOP  drop
  bl  r> 2* scrstart +  scr!
 REPEAT
 BEGIN stop REPEAT ; \ stop's forever
: wait  Background sleep ;
: go    Background wake ;

multitask       $1000 >count  page
