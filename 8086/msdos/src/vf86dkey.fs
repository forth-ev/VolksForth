
  Forth definitions

\ *** Block No. 114, Hexblock 72

\  BDOS  keyboard input                           ks 16 sep 88
\ es muss wirklich so kompliziert sein, da sonst kein ^C und ^P

| Variable newkey   newkey off

  Code (key@  ( -- 8b )    D push   newkey #) D mov   D+ D+ or
     0= ?[  $7 # A+ mov   $21 int   A- D- mov  ]?
     0 # D+ mov   D+ newkey 1+ #) mov   Next
  end-code

  Code (key?  ( -- f )    D push   newkey #) D mov   D+ D+ or
     0= ?[  -1 # D- mov   6 # A+ mov   $21 int  0=
            ?[  0 # D+ mov
            ][  -1 # A+ mov   A newkey #) mov   -1 # D+ mov
        ]?  ]?  D+ D- mov   Next
  end-code

\ *** Block No. 115, Hexblock 73

\ empty-keys  (key                                ks 16 sep 88

  Code empty-keys   $C00 # A mov   $21 int
     0 # newkey 1+ #) byte mov   Next   end-code

  : (key   ( -- 16b )   BEGIN  pause (key?  UNTIL
     (key@ ?dup ?exit  (key? IF  (key@ negate exit  THEN  0 ;










\ *** Block No. 116, Hexblock 74

\ BIOS  keyboard input                           ks 16 sep 88

\  Code (key@  ( -- 8b )  D push   A+ A+ xor   $16 int
\     A- D- xchg   0 # D+ mov   Next   end-code

\  Code (key?  ( -- f )   D push   1 # A+ mov   D D xor
\     $16 int   0= not ?[  D dec  ]?   Next   end-code

\  Code empty-keys   $C00 # A mov   $21 int   Next   end-code

\  : (key  ( -- 8b )   BEGIN  pause (key? UNTIL  (key@ ;

\ mit diesen Keytreibern sind die Funktionstasten nicht
\ mehr durch ANSI.SYS Sequenzen vorbelegt.



