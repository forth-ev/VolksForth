\ Part of the target compiler, first step

\ *** Block No. 4, Hexblock 4

\ Relocating a system        clv2:jull87

$9400         $0400
( stacklength rstacklength -)
 empty hex
 over + origin +  origin 0A + ! \ r0
 origin +  dup    origin   1+ ! \ task
             6 -  origin  8 + ! \ s0
 (16 $c000 ' limit >body ! C)
 cold
\\ symbolic map of system
up@ origin - is stacklength
r0 @ up@ -   is rstacklength

disk-buffer  limit        first @
rstack       r0 @         rp@

user, warm   up@ udp @ +  up@
(heap)       up@          heap
stack        s0 @         sp@

system       here         origin 0FE +
user, cold   origin 0FE + origin
screen       0800         0400
page 0-3     0400         0000
