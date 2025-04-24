
\ Simple Editor for Installation                    cas 10nov05

\ If the Full-Screen Editor cannot be used during installation
\ (incompatible display hardware), the screens must be altered
\ with this Simple Editor "PRIMED", which contains only one word
\ definition::

\ Usage: Select Screen nn with command "nn LIST",
\  and edit a screen with "ll NEW". It is only possible to
\  rewrite whole lines. ll is the first line where the editing
\  should start. The editing can be terminated by entering an
\  empty line (just RETURN). Each RETURN will store the editied
\  line and the whole screen will be reprinted.

\ primitivst Editor PRIMED                           cas 10nov05
 Vocabulary Editor

| : !line ( adr count line# -- )
      scr @ block  swap c/l * +  dup c/l bl fill
      swap cmove update ;

: new ( n -- )
   l/s 1+ swap
   ?DO cr  I .
      pad c/l expect   span @ 0= IF leave THEN
      pad  span @  I !line   cr  scr @ list  LOOP ;

 ' scr | Alias scr'

 .( Simple Editor loaded ) cr
