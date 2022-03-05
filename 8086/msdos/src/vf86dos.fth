\ *** Block No. 112, Hexblock 70

\ lc@ lc!  l@ l!  special 8088 operators          ks 27 oct 86

  Code lc@  ( seg:addr -- 8b )   D: pop   D W mov
     W ) D- mov   0 # D+ mov   C: A mov   A D: mov   Next
  end-code

  Code lc!  ( 8b seg:addr -- )   D: pop   A pop   D W mov
     A- W ) mov   C: A mov   A D: mov   D pop   Next   end-code

  Code l@  ( seg:addr -- 16b )   D: pop   D W mov
     W ) D mov   C: A mov   A D: mov   Next   end-code

  Code l!  ( 16b seg:addr -- )   D: pop   A pop   D W mov
     A W ) mov   C: A mov   A D: mov   D pop   Next   end-code



\ *** Block No. 113, Hexblock 71

\ ltype  lmove    special 8088 operators          ks 11 dez 87

  : ltype   ( seg:addr len -- )
     0 ?DO  2dup I + lc@ emit  LOOP  2drop ;

  Code lmove  ( from.seg:addr to.seg:addr quan -- )
     A I xchg   D C mov   W pop   E: pop
     I pop   D: pop   I W cmp  CS
     ?[  rep byte movs
     ][  C dec   C W add   C I add   C inc
         std   rep byte movs   cld
     ]?  A I xchg   C: A mov   A E: mov
     A D: mov   D pop   Next   end-code




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

\\ BIOS  keyboard input                           ks 16 sep 88

  Code (key@  ( -- 8b )  D push   A+ A+ xor   $16 int
     A- D- xchg   0 # D+ mov   Next   end-code

  Code (key?  ( -- f )   D push   1 # A+ mov   D D xor
     $16 int   0= not ?[  D dec  ]?   Next   end-code

  Code empty-keys   $C00 # A mov   $21 int   Next   end-code

  : (key  ( -- 8b )   BEGIN  pause (key? UNTIL  (key@ ;

\ mit diesen Keytreibern sind die Funktionstasten nicht
\ mehr durch ANSI.SYS Sequenzen vorbelegt.



\ *** Block No. 117, Hexblock 75

\ (decode expect                                  ks 16 sep 88

   7 Constant #bel            8 Constant #bs
   9 Constant #tab           $A Constant #lf
  $D Constant #cr

  : (decode  ( addr pos1 key -- addr pos2 )
     #bs case? IF  dup 0=exit del 1- exit  THEN
     #cr case? IF  dup span ! space   exit  THEN
     >r  2dup +  r@ swap c!  r> emit  1+ ;

  : (expect ( addr len1 -- )  span !   0
     BEGIN   dup span @ u< WHILE  key decode  REPEAT  2drop ;

  Input: keyboard [ here input ! ]
          (key (key? (decode (expect [ drop

\ *** Block No. 118, Hexblock 76

\ MSDOS character output                          ks 29 jun 87

  Code charout  ( char -- )   $FF # D- cmp  0= ?[  D- dec  ]?
     6 # A+ mov   $21 int   D pop   ' pause # W mov   W ) jmp
  end-code

  &80 Constant c/row            &25 Constant c/col

  : (emit   ( char -- )  dup bl u< IF  $80 or  THEN  charout ;
  : (cr                  #cr charout #lf charout ;
  : (del                 #bs charout bl charout #bs charout ;
  : (at                  2drop ;
  : (at?                 0 0 ;
  : (page                c/col 0 DO  cr  LOOP ;



\ *** Block No. 119, Hexblock 77

\ MSDOS character output                          ks  7 may 85

  : bell   #bel charout ;

  : tipp   ( addr len -- )  bounds ?DO  I c@ emit  LOOP ;

  Output: display [ here output ! ]
           (emit (cr  tipp (del (page (at (at? [ drop









\ *** Block No. 120, Hexblock 78

\ MSDOS printer   I/O Port access                 ks 09 aug 87

  Code lst! ( 8b -- )  $5 # A+ mov   $21 int   D pop   Next
  end-code

  Code pc@    ( port -- 8b )
     D byte in   A- D- mov   D+ D+ xor   Next
  end-code

  Code pc!    ( 8b port -- )
     A pop   D byte out   D pop   Next
  end-code





\ *** Block No. 121, Hexblock 79

\ zero terminated strings                         ks 09 aug 87

  : counted   ( asciz -- addr len )
     dup -1 0 scan drop over - ;

  : >asciz   ( string addr -- asciz )   2dup >r  -
     IF  count r@ place  r@  THEN  0 r> count + c!  1+ ;



  : asciz    ( -- asciz )   name here >asciz ;






\ *** Block No. 122, Hexblock 7a

\ Disk capacities                                 ks 08 aug 88
  Vocabulary Dos   Dos also definitions

  6 Constant #drives

  Create capacities   $4B0 , $4B0 , $1B31 , $1B31 , $1B0F , 0 ,

| Code ?capacity ( +n -- cap )  D shl   capacities # W mov
     D W add   W ) D mov   Next   end-code








\ *** Block No. 123, Hexblock 7b

\ MS-dos disk handlers direct access              ks 31 jul 87

| Code block@  ( addr blk drv -- ff )
     D- A- mov   D pop   C pop   R push   U push
     I push   C R mov   2 # C mov   D shl   $25 int
  Label end-r/w    I pop   I pop   U pop   R pop   0 # D mov
     CS ?[  D+ A+ mov   A error# #) mov   D dec  ]?  Next
  end-code

| Code block!  ( addr blk drv -- ff )  D- A- mov   D pop
     C pop   R push   U push   I push   C R mov   2 # C mov
     D shl   $26 int   end-r/w # jmp
  end-code




\ *** Block No. 124, Hexblock 7c

\ MS-dos disk handlers direct access              ks cas 18jul20

| : ?drive  ( +n -- +n )   dup #drives u< ?exit
     Error" beyond drive capacity" ;

  : /drive ( blk1 -- blk2 drive )  0 swap  #drives 0
     DO  dup I ?capacity under u< IF drop LEAVE THEN
         - swap 1+ swap  LOOP  swap ;

  : blk/drv  ( -- capacity )  drv ?capacity ;

  Forth definitions

  : >drive    ( blk1 +n -- blk2 )   ?drive
     0 swap  drv  2dup u> dup >r  0= IF  swap  THEN
     ?DO  I ?capacity + LOOP  r> IF  negate  THEN - ;

\ *** Block No. 125, Hexblock 7d

\ MS-DOS   file access                            ks 18 mär 88
  Dos definitions

| Variable fcb         fcb off      \ last fcb accessed
| Variable prevfile                 \ previous active file

  &30 Constant fnamelen             \ default length in FCB

  Create filename   &62 allot       \ max 60 + count + null

  Variable attribut   7 attribut !  \ read-only, hidden, system






\ *** Block No. 126, Hexblock 7e

\ MS-DOS   disk errors                            ks cas 18jul20

| : .error#   ." error # " base push decimal error# @ . ;

| : .ferrors   error# @ &18 case? IF  2  THEN
       1 case? Abort" file exists"
       2 case? Abort" file not found"
       3 case? Abort" path not found"
       4 case? Abort" too many open files"
       5 case? Abort" no access"
       9 case? Abort" beyond end of file"
     &15 case? Abort" illegal drive"
     &16 case? Abort" current directory"
     &17 case? Abort" wrong drive"
     drop ." Disk" .error# abort ;


\ *** Block No. 127, Hexblock 7f

\ MS-DOS   disk errors                            ks cas 18jul20

  : (diskerror   ( *f -- )   ?dup 0=exit
     fcb @ IF  error# !  .ferrors exit  THEN
     input push   output push   standardi/o   1-
     IF  ." read"  ELSE  ." write"  THEN
     .error# ."  retry? (y/n)"
     key cr capital Ascii Y = not Abort" aborted" ;

  ' (diskerror Is ?diskerror







\ *** Block No. 128, Hexblock 80

\ ~open  ~creat  ~close                           ks 04 aug 87

  Code ~open  ( asciz mode -- handle ff / err# )
     A D xchg   $3D # A+ mov
  Label >open   D pop   $21 int   A D xchg
     CS not ?[  D push   0 # D mov  ]?  Next
  end-code

  Code ~creat  ( asciz attribut -- handle ff / err# )
     D C mov   $3C # A+ mov   >open ]]   end-code

  Code ~close   ( handle -- )   D R xchg
     $3E # A+ mov   $21 int   R D xchg   D pop   Next
  end-code



\ *** Block No. 129, Hexblock 81

\ ~first  ~unlink  ~select  ~disk?                ks 04 aug 87

  Code ~first  ( asciz attr -- err# )
     D C mov   D pop   $4E # A+ mov
 [[  $21 int   0 # D mov   CS ?[  A D xchg  ]?   Next
  end-code

  Code ~unlink  ( asciz -- err# )    $41 # A+ mov  ]]  end-code

  Code ~select  ( n -- )
     $E # A+ mov   $21 int   D pop   Next   end-code

  Code ~disk?  ( -- n )   D push   $19 # A+ mov
     $21 int   A- D- mov   0 # D+ mov   Next
  end-code


\ *** Block No. 130, Hexblock 82

\ ~next  ~dir                                     ks 04 aug 87

  Code ~next    ( -- err# )   D push   $4F # A+ mov
     $21 int   0 # D mov   CS ?[  A D xchg  ]?   Next
  end-code

  Code ~dir    ( addr drive -- err# )   I W mov
     I pop   $47 # A+ mov   $21 int   W I mov
     0 # D mov   CS ?[  A D xchg  ]?   Next
  end-code







\ *** Block No. 131, Hexblock 83

\ MS-DOS file control Block                          cas 19jun20

| : Fcbytes  ( n1 len -- n2 )  Create over c, +
  Does>      ( fcbaddr -- fcbfield )  c@ + ;

\ first field for file-link
2        1 Fcbytes f.no       \ must be first field
         2 Fcbytes f.handle
         2 Fcbytes f.date
         2 Fcbytes f.time
         4 Fcbytes f.size
  fnamelen Fcbytes f.name     Constant b/fcb

b/fcb  Host   ' tb/fcb >body !
       Target Forth also Dos also definitions


\ *** Block No. 132, Hexblock 84

\ (.file fname  fname!                            ks 10 okt 87

  : fname!   ( string fcb -- )   f.name >r   count
     dup fnamelen < not Abort" file name too long"  r> place ;

| : filebuffer?   ( fcb -- fcb bufaddr / fcb ff )
     prev  BEGIN  @ dup WHILE  2dup 2+ @  = UNTIL ;

| : flushfile     ( fcb -- )
     BEGIN  filebuffer? ?dup
     WHILE  dup backup emptybuf  REPEAT  drop ;

  : fclose   ( fcb  -- )   ?dup 0=exit
     dup f.handle @ ?dup 0= IF  drop exit  THEN
     over flushfile  ~close  f.handle off ;


\ *** Block No. 133, Hexblock 85

\ (.file fname  fname!                            ks 18 mär 88

| : getsize   ( -- d )     [ $80 &26 + ] Literal 2@ swap ;

  : (fsearch  ( string -- asciz *f )
     filename >asciz dup attribut @ ~first ;

  Defer fsearch   ( string -- asciz *f )

  ' (fsearch Is fsearch

\ graceful behaviour if file does not exist
| : ?notfound  ( f* -- )  ?dup 0=exit  last' @  [fcb] =
     IF  hide   file-link @ @ file-link !  prevfile @ setfiles
         last @ 4 - dp !  last off   filename count here place
     THEN  ?diskerror ;

\ *** Block No. 134, Hexblock 86

\ freset fseek                                    ks 19 mär 88

  : freset  ( fcb -- )   ?dup 0=exit
     dup f.handle @ ?dup IF  ~close  THEN   dup >r
     f.name fsearch ?notfound   getsize r@ f.size 2!
     [ $80 &22 + ] Literal @ r@ f.time !
     [ $80 &24 + ] Literal @ r@ f.date !
     2 ~open ?diskerror  r> f.handle ! ;


  Code fseek ( dfaddr fcb -- )
     D W mov   u' f.handle W D) W mov   W W or  0=
     ?[  ;c: dup freset fseek ; Assembler ]?  R W xchg
     C pop   D pop   $4200 # A mov  $21 int   W R mov
     CS not ?[  D pop   Next  ]?  A D xchg  ;c: ?diskerror ;


\ *** Block No. 135, Hexblock 87

\ lfgets  fgetc  file@                            ks 07 jul 88

\ Code ~read   ( seg:addr quan handle -- #read )  D W mov
Assembler  [[   W R xchg   C pop   D pop
     D: pop   $3F # A+ mov   $21 int   C: C mov   C D: mov
     W R mov  A D xchg  CS not ?[  Next  ]?  ;c: ?diskerror ;

  Code lfgets  ( seg:addr quan fcb -- #read )
     D W mov   u' f.handle W D) W mov   ]]  end-code

  true Constant eof

  : fgetc  ( fcb -- 8b / eof )
     >r 0 sp@ ds@ swap 1 r> lfgets ?exit 0= ;

  : file@  ( dfaddr fcb -- 8b / eof )  dup >r fseek r> fgetc ;

\ *** Block No. 136, Hexblock 88

\ lfputs  fputc  file!                            ks 24 jul 87

| Code ~write  ( seg:addr quan handle -- )   D W mov
[[   W R xchg   C pop   D pop
     D: pop   $40 # A+ mov   $21 int   W R mov  A D xchg
     C: W mov   W D: mov  CS ?[  ;c: ?diskerror ; Assembler  ]?
     C D sub  0= ?[  D pop   Next  ]?  ;c: Abort" Disk voll" ;

  Code lfputs  ( seg:addr quan fcb -- )
     D W mov   u' f.handle W D) W mov  ]]  end-code

  : fputc  ( 8b fcb -- )  >r sp@ ds@ swap 1 r> lfputs drop ;

  : file!  ( 8b dfaddr fcb -- )  dup >r fseek r> fputc ;



\ *** Block No. 137, Hexblock 89

\ /block  *block                                  ks 02 okt 87

  Code /block  ( d -- rest blk )   A D xchg   C pop
     C D mov   A shr   D rcr   A shr   D rcr   D+ D- mov
     A- D+ xchg   $3FF # C and   C push   Next
  end-code
\ : /block  ( d -- rest blk )   b/blk um/mod ;

  Code *block  ( blk -- d )  A A xor   D+ D- xchg   D+ A+ xchg
     A+ sal   D rcl   A+ sal   D rcl   A push   Next
  end-code
\ : *block  ( blk -- d )   b/blk um* ;





\ *** Block No. 138, Hexblock 8a

\ fblock@  fblock!                                ks 19 mär 88
  Dos definitions

| : ?beyond   ( blk -- blk )  dup 0< 0=exit  9 ?diskerror ;

| : fblock   ( addr blk fcb -- seg:addr quan fcb )
     fcb !  ?beyond dup *block  fcb @  fseek   ds@ -rot
     fcb @ f.size 2@ /block rot -  ?beyond
     IF  drop b/blk  THEN  fcb @ ;

  : fblock@  ( addr blk fcb -- )    fblock lfgets drop ;

  : fblock!  ( addr blk fcb -- )    fblock lfputs ;




\ *** Block No. 139, Hexblock 8b

\ (r/w  flush                                     ks 18 mär 88
  Forth definitions

  : (r/w   ( addr blk fcb r/wf -- *f )  over fcb !  over
     IF  IF  fblock@ false exit  THEN  fblock! false exit
     THEN  >r drop  /drive ?drive
     r> IF  block@ exit  THEN  block! ;

  ' (r/w Is r/w

| : setfiles  ( fcb -- )   isfile@ prevfile !
     dup isfile !   fromfile ! ;

  : direct   0 setfiles ;



\ *** Block No. 140, Hexblock 8c

\ File  >file                                     ks 23 mär 88

  : File    Create   file-link @   here file-link !  ,
     here [ b/fcb 2 - ] Literal   dup allot   erase
     file-link @   dup @ f.no c@ 1+   over f.no c!
     last @ count $1F and   rot f.name place
  Does> setfiles ;

  File kernel.scr    ' kernel.scr @  Constant [fcb]

  Dos definitions

  : .file   ( fcb -- )
     ?dup IF  body> >name .name exit  THEN  ." direct" ;



\ *** Block No. 141, Hexblock 8d

\ .file  pushfile  close  open                    ks 12 mai 88
  Forth  definitions

  : file?    isfile@ .file ;

  : pushfile    r>  isfile push  fromfile push  >r ; restrict

  : close    isfile@ fclose ;

  : open     isfile@ freset ;

  : assign   isfile@ dup fclose   name swap fname!   open ;





\ *** Block No. 142, Hexblock 8e

\      use from loadfrom include                  ks 18 mär 88

  : use      >in @   name find
     0= IF  swap >in !   File   last'  THEN  nip
     dup @ [fcb] =  over ['] direct = or
     0= Abort" not a file"   execute open ;

  : from         isfile push   use ;

  : loadfrom     ( n -- )   pushfile  use load close ;

  : include      1 loadfrom ;





\ *** Block No. 143, Hexblock 8f

\ drive  drv  capacity   drivenames               ks 18 mär 88

  : drive ( n -- )   isfile@ IF  ~select exit  THEN
     ?drive   offset off 0 ?DO  I ?capacity offset +!  LOOP ;

  : drv   ( -- n )
     isfile@ IF  ~disk? exit  THEN  offset @ /drive nip ;

  : capacity   ( -- n )   isfile@ ?dup
     IF  dup f.handle @ 0= IF  dup freset  THEN
         f.size 2@ /block swap 0<> - exit  THEN  blk/drv ;

| : Drv:   Create c,  Does> c@ drive ;

  0 Drv: A:     1 Drv: B:     2 Drv: C:     3 Drv: D:
  4 Drv: E:     5 Drv: F:     6 Drv: G:     7 Drv: H:

\ *** Block No. 144, Hexblock 90

\ lfsave  savefile  savesystem                    ks 10 okt 87

  : lfsave   ( seg:addr quan string -- )
     filename >asciz 0 ~creat ?diskerror
     dup >r  ~write  r> ~close ;

  : savefile ( addr len -- )  ds@ -rot
     name nullstring? Abort" needs name" lfsave ;

  : savesystem   save flush   $100 here savefile ;







\ *** Block No. 145, Hexblock 91

\ viewing                                         ks 19 mär 88
  Dos definitions
| $400 Constant viewoffset

  : (makeview   ( -- n )
     blk @ dup 0=exit   loadfile @ ?dup 0=exit   f.no c@ ?dup
     IF  viewoffset * + $8000 or exit  THEN  0= ;
  ' (makeview Is makeview

  : @view  ( acf -- blk fno )   >name 4 - @   dup 0<
     IF  $7FFF and viewoffset u/mod  exit  THEN
     ?dup 0= Error" eingetippt"  0 ;

  : >file   ( fno -- fcb )   dup 0=exit    file-link
     BEGIN  @  dup WHILE  2dup f.no c@ = UNTIL  nip ;


\ *** Block No. 146, Hexblock 92

\ forget FCB's                                    ks 23 okt 88
  Forth definitions
| : 'file  ( -- scr )  r>   scr push   isfile push   >r
     [ Dos ] ' @view >file isfile ! ;

  : view   'file list ;
  : help   'file capacity 2/ + list ;

| : remove?   ( dic symb addr -- dic symb addr f )
     2 pick over 1+ u< ;

| : remove-files  ( dic symb -- dic symb )  file-link
     BEGIN  @ ?dup WHILE  remove? IF  dup fclose  THEN  REPEAT
     file-link remove
     isfile@    remove? nip IF  file-link @ isfile !  THEN
     fromfile @ remove? nip 0=exit isfile@ fromfile ! ;

