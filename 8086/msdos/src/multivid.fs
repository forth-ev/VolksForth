
\ *** Block No. 0, Hexblock 0

\ This file is a pure .fs-version of multi.vid.

\ This display interface uses BIOS call $10 functions for a fast
\ display interface. A couple of state variables is contained
\ in a vector that is task specific such that different tasks
\ may use different windows. For simplicity windows always
\ span the whole width of the screen. They can be defined by
\ top and bottom line. This mechanism is used for a convenient
\ status display line on the bottom of the screen.









\ *** Block No. 1, Hexblock 1

\ Multitsking display interface loadscreen        ks phz 31jan22
  Onlyforth    \needs Assembler   include t86asm.fs

      User area     area off    \ points at active window
  Variable status   \ to switch status on/off
| Variable cursor   \ points at area with active cursor

\   1 8 +thru   .( Multitasking display driver loaded ) cr









\ *** Block No. 2, Hexblock 2

\ Multitsking display interface                   ks  6 sep 86

  : Area:  Create  0 , 0 , 7 c, Does> area ! ;
\ | col | row | top | bot | att |

Area: terminal    terminal   area @ cursor !

  : (area   Create dup c, 1+ Does> c@ area @ + ;

0 | (area ccol    | (area crow    | (area ctop
  | (area cbot      (area catt    drop

  : window  ( topline botline -- )   cbot c!  ctop c! ;

  : full     0 c/col 2- window ;  full


\ *** Block No. 3, Hexblock 3

\ Multitask (type (emit                           ks 20 dez 87

  Code (type   ( addr len -- )   W pop   I push   R push
     u' area U D) I mov   U push   D U mov
     $F # A+ mov   $10 int   u' catt I D) R- mov
     3 # A+ mov   $10 int   C push   D push   $E0E # C mov
     1 # A+ mov   $10 int   I ) D mov   1 # C mov
     U inc   [[  U dec  0= not ?[[  2 # A+ mov   $10 int
         D- inc   ' c/row >body #) D- cmp   0= not
     ?[[  W ) A- mov   W inc   9 # A+ mov   $10 int   ]]? ]?
     D I ) mov   D pop   cursor #) I cmp   0= ?[  I ) D mov  ]?
     2 # A+ mov   $10 int   C pop   1 # A+ mov   $10 int   U pop
     R pop   I pop   D pop   ' pause #) jmp   end-code

  : (emit  ( char -- )   sp@ 1 (type drop ;


\ *** Block No. 4, Hexblock 4

\ Multitask (at (at?                              ks 04 aug 87
  Code (at    ( row col -- )   A pop   A- D+ mov
     u' area U D) W mov   D W ) mov   cursor #) W cmp  0=
     ?[  R push   U push   $F # A+ mov   $10 int
         2 # A+ mov   $10 int   U pop   R pop
     ]?  D pop   Next   end-code

  Code (at?   ( -- row col )
     D push   u' area U D) W mov   W ) D mov
     D+ A- mov   0 # A+ mov   A+ D+ mov   A push   Next
  end-code

  Code curat? ( -- row col )   D push   R push
     $F # A+ mov   $10 int   3 # A+ mov   $10 int
     R pop   0 # A mov   D+ A- xchg   A push   Next
  end-code

\ *** Block No. 5, Hexblock 5

\ cur!  curshape  setpage                         ks 28 jun 87

  : cur!   \ set cursor into current task's window
     area @ cursor !  (at? (at ;  cur!

  Code curshape  ( top bot -- )   D C mov   D pop
     D- C+ mov   1 # A+ mov   $10 int   D pop   Next
  end-code

  Code setpage   ( n -- )
     $503 # A mov   D- A- and   $10 int   D pop   Next
  end-code





\ *** Block No. 6, Hexblock 6

\ Multitask normal invers blankline               ks 01 nov 88
  : normal     7 catt c! ;     : invers   $70 catt c! ;
  : underline  1 catt c! ;     : bright    $F catt c! ;

  Code blankline    D push   R push   U push   $F # A+ mov
     $10 int   u' area U D) W mov   u' catt W D) R- mov
     3 # A+ mov   $10 int   C push   D push
     $E0E # C mov   1 # A+ mov   $10 int   W ) D mov
     2 # A+ mov   $10 int   ' c/row >body #) C mov
     D- C- sub   bl # A- mov   9 # A+ mov
     C- C- or   0= not ?[  $10 int  ]?
     D pop   2 # A+ mov   $10 int  \ set cursor back
     C pop   1 # A+ mov   $10 int  \ cursor visible again
     U pop   R pop   D pop   ' pause #) jmp   end-code

| : lineerase  ( line# -- )  0 (at blankline ;

\ *** Block No. 7, Hexblock 7

\ Multitask (del scroll (cr (page                 ks 04 okt 87

  : (del    (at? ?dup
     IF  1- 2dup (at  bl (emit  (at exit  THEN  drop ;

  Code scroll   D push   R push   U push
     u' area U D) W mov   u' catt W D) R+ mov
     u' ctop W D) D mov   D- C+ mov   0 # C- mov
     ' c/row >body #) D- mov   D- dec   $601 # A mov
     $10 int   U pop   R pop   D pop   Next
  end-code

  : (cr     (at? drop 1+ dup cbot c@ u>
     IF  scroll drop cbot c@  THEN  lineerase ;

  : (page   ctop c@ cbot c@ DO  I lineerase  -1 +LOOP ;

\ *** Block No. 8, Hexblock 8

\ Multitask status display                        ks 10 okt 87

  ' (emit ' display   2 + !  ' (cr   ' display   4 + !
  ' (type ' display   6 + !  ' (del  ' display   8 + !
  ' (page ' display &10 + !
  ' (at   ' display &12 + !  ' (at?  ' display &14 + !

  : .base    base @ decimal  dup 2 .r  base ! ;
  : .sp      ( n -- )  ."  s" depth   swap 1+ - 2 .r ;
  : (.drv    ( n -- )   Ascii A + emit ." : " ;
  : .dr      ."   " drv (.drv ;
  : .scr     blk @ IF  ."   Blk" blk  ELSE  ."   Scr" scr  THEN
             @ 5 .r ;
  : .space   ."   Dic" s0 @ here $100 + - 6 u.r ;



\ *** Block No. 9, Hexblock 9

\ statuszeile                                 ks  ks 04 aug 87

| : fstat  ( n -- )   .base .sp
     .space .scr .dr file? 2 spaces order ;

| Area: statusline
  statusline c/col 1- dup window page invers  terminal

  : (.status   output @ display   area @ statusline
     status @ IF  (at? drop 0 (at 2 fstat  blankline
              ELSE  normal page invers
              THEN  area !  output ! ;
  ' (.status Is .status

  : bye  status off .status bye ;

.( Multitasking display driver loaded ) cr
