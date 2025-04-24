\ This video display interface utilizes the ROM BIOS call $10.
\ The display is fairly fast and should work on most IBM
\ compatible computers

\ BIOS display interface                             cas 15feb25
  Onlyforth   \needs Assembler   include asm86.fs
  Variable dpage        dpage off
  Variable top          top off
  Variable status       status off
  Code (at    ( lin col -- )   A pop   R push   U push
     dpage #) R+ mov   A- D+ mov   2 # A+ mov   $10 int
     U pop   R pop   D pop   Next   end-code

  Code (at?   ( -- lin col )   D push   R push   U push
     dpage #) R+ mov   3 # A+ mov   $10 int   U pop   R pop
     D+ A- mov   0 # A+ mov   A+ D+ mov   A push   Next
  end-code
  
  ' (at? alias curat?
  
  .( 1 )
\ BIOS  normal invers  blankline

  : full            top off ;

  Variable attribut 7 attribut !

  : normal     ;   : invers   ;
  : underline  ;   : bright   ;

  Code blankline      D push   R push   U push
     dpage #) R+ mov   attribut #) R- mov
     3 # A+ mov   $10 int   ' c/row >body #) C mov
     D- C- sub   bl # A- mov   9 # A+ mov   $10 int
     U pop   R pop   D pop   Next   end-code

| : lineerase   0 (at blankline ;

.( 2 )
\ BIOS  (type  (emit                                 cas 14nov05

  Code (type   ( addr len -- )   W pop   R push   U push
     D U mov   dpage #) R+ mov   attribut #) R- mov
     3 # A+ mov   $10 int   U inc   C push   $E0E # C mov
     1 # A+ mov   $10 int   1 # C mov   [[  U dec  0= not
     ?[[  D- inc   ' c/row >body #) D- cmp  0= not
          ?[[  W ) A- mov   W inc   9 # A+ mov
               $10 int   2 # A+ mov   $10 int  ]]?
     ]?   C pop   1 # A+ mov   $10 int
     U pop   R pop   D pop   ' pause #) jmp
  end-code

  : (emit  ( char -- )   sp@ 1 (type drop ;
.( 3 )
\ BIOS  (del scroll (cr (page                        cas 15nov05

  : (del    (at? ?dup
     IF  1- 2dup (at  bl (emit  (at exit  THEN  drop ;

  Code scroll   D push   R push   U push   attribut #) R+ mov
     top #) C+ mov   0 # C- mov   ' c/row >body #) D- mov
     D- dec   ' c/col >body #) D+ mov   D+ dec status #) D+ add
     $601 # A mov   $10 int   U pop   R pop   D pop   Next
  end-code

  : (cr     (at? drop 1+ dup 2+ c/col status @ 1+ +  u>
       IF scroll drop c/col 1- status @ + THEN lineerase ;

  : (page top @ c/col 2- DO  I lineerase  -1 +LOOP ;

  .( 4 )

\ BIOS  status display                               cas 15nov05
 
\  ' (emit ' display   2 + !  ' (cr   ' display   4 + !
\  ' (type ' display   6 + !  ' (del  ' display   8 + !
\  ' (page ' display &10 + !
\  ' (at   ' display &12 + !  ' (at?  ' display &14 + !

  Output: pofodisplay 
           (emit (cr  (type (del (page (at (at? ;

  pofodisplay
       
  : .sp      ( n -- )  ."  s" depth  swap 1+ - 2 .r ;
  : .base    base @  decimal dup 2 .r   base ! ;
  : .space   ."  D" s0 @ here $100 + - 6 u.r ;

.( 5 )

\ statuszeile                                        cas 15nov05

| : fstat  ( n -- )   .base .sp .space ;

  : .stat      output @  (at?
     c/col 1- 0 (at  status @ IF 3 fstat THEN
     over c/col 1- < IF blankline THEN
     (at output !   ;

  : +stat   ['] .stat Is .status  .status ;

  : -stat   [']  noop Is .status ;

.( end )
