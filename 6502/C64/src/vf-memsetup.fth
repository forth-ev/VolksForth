
\ *** Block No. 125, Hexblock 7d
7d fthpage

\ System patchup              clv06aug87

s0 @ dup s0 2- !      6 + s0 7 - !
here dp !

Host  Tudp @          Target  udp !
Host  Tvoc-link @     Target  voc-link !
Host  move-threads

\ Final part of loadscreen

Assembler nonrelocate

.unresolved
