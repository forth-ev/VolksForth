
\ *** Block No. 0, Hexblock 0

\ CP/M 2.2 File-Interface (3.80a)                     UH 05Oct87

\ Dieses File enthaelt das File-Interface von volksFORTH zu CP/M.
\ Damit ist Zugriff auf normale CP/M-Files moeglich.
\ Wenn ein File mit USE benutzt wird, beziehen sich alle Worte,
\ die mit dem Massenspeicher arbeiten, auf dieses File.

\ Benutzung:
\   USE <name>       \ benutze ein schon existierendes File
\   FILE <name>      \ erzeuge ein Forthfile mit dem Namen <name>.
\   MAKE <name>      \ Erzeuge ein File mit <name> und ordne
\                   \ es dem aktuellen Forthfile zu.
\   MAKEFILE <name>  \ Erzeuge ein File mit CP/M und FORTH-Namen
\                      <name>.
\   INCLUDE <name>   \ Lade File mit Forthnamen <name> ab Screen 1
\   DOS RESET        \ zum Wechsel von Disketten. (Oh! CP/M)

\ *** Block No. 1, Hexblock 1

\ CP/M 2.2 File-Interface  load-Screen                UH 18Feb88
OnlyForth

\       2 load \ view numbers for this file
\   3   4 thru \ DOS   File Functions
\   5 $11 thru \ Forth File Functions
\ $12 $16 thru \ User Interface

\ File source.fb      \  Define already existing Files
\ File fileint.fb     File startup.fbr

\ ' (makeview     Is makeview
\ ' remove-files  Is custom-remove
\ ' file-r/w      Is r/w
\ ' noop          Is drvinit
 \      include startup.fb \ load Standard System

\ *** Block No. 2, Hexblock 2

\ Build correct view-numbers for this file           UUH 19Nov87

| : fileintview ( -- )  $400 blk @ + ;

' fileintview Is makeview












\ *** Block No. 3, Hexblock 3

\ File Control Blocks                                 UH 18Feb88
Dos definitions also
| : Fcbyte ( n len -- len' )  Create over c, + does> c@ + ;
&11 Constant filenamelen
0              2 | Fcbyte nextfile  immediate
               1   Fcbyte drive    ' drive | Alias >dosfcb
 filenamelen 3 -   Fcbyte filename
               3   Fcbyte extension
           &21 + \ ex, s1, s2, rc, d0, ... dn, cr
               2   Fcbyte record    \  r0, r1
              1+ \ r2
               2   Fcbyte opened
               2   Fcbyte fileno
               2   Fcbyte filesize  \ in 128-Byte-Records
               4   Fcbyte position
Constant b/fcb

\ *** Block No. 4, Hexblock 4

\ dos primitives                                      UH 10Oct87

' 2-  | Alias body>             ' 2- | Alias dosfcb>

: drive!     ( drv    --      )     $0E bdos ;
: search0    ( dosfcb -- dir  )     $11 bdosa ;
: searchnext ( dosfcb -- dir  )     $12 bdosa ;
: read-seq   ( dosfcb -- f    )     $14 bdosa dos-error? ;
: write-seq  ( dosfcb -- f    )     $15 bdosa dos-error? ;
: createfile ( dosfcb -- f    )     $16 bdosa dos-error? ;
: size       ( dos    -- size ) dup $23 bdos dosfcb> record @ ;
: drive@     (        -- drv  )   0 $19 bdosa ;
: killfile   ( dosfcb --      )     $13 bdos ;




\ *** Block No. 5, Hexblock 5

\ File sizes                                          UH 05Oct87

: (capacity  ( fcb -- n )  \ filecapacity in blocks
    filesize @  rec/blk  u/mod swap 0= ?exit 1+ ;

: in-range ( block fcb -- )
    (capacity u< not Abort" beyond capacity!" ;

Forth definitions

: capacity ( -- n )    isfile@  (capacity ;

Dos definitions




\ *** Block No. 6, Hexblock 6

\ (open                                               UH 18Feb88

: (open  ( fcb -- )
    dup opened @ IF drop exit THEN   dup position  0. rot 2!
    dup >dosfcb openfile  Abort" not found!"  dup opened on
    dup >dosfcb size  swap filesize ! ;

: (make  ( fcb -- )
   dup >dosfcb killfile
   dup >dosfcb createfile Abort" directory full!"
   dup position 0. rot 2!
   dup filesize off   opened on  offset off ;

: file-r/w  ( buffer block fcb f  -- f )
    over 0= Abort" no Direct Disk IO supported! "
    >r  dup  (open   2dup in-range r> (r/w ;

\ *** Block No. 7, Hexblock 7

\ Print Filenames                                     UH 10Oct87

: .file ( fcb -- ) 0 case? IF ." DIRECT"  exit THEN
         fcb dosfcb> case? IF ." DEFAULT" exit THEN
         body> >name .name ;

: .drive   ( fcb -- ) drive c@ ?dup 0=exit
     [ Ascii A 1- ] Literal + emit  Ascii : emit ;

: .dosfile ( fcb -- )  dup filename  8 -trailing type
    Ascii . emit           extension 3           type ;






\ *** Block No. 8, Hexblock 8

\ Print Filenames                                     UH 10Oct87

: tab ( -- ) col &59 > IF cr exit THEN
        &20  col &20 mod  -  0 max  spaces ;

: .fcb  ( fcb -- )  dup fileno @  3 u.r tab
     dup .file tab dup .drive dup .dosfile
     tab dup opened @ IF ." opened" ELSE ." closed" THEN
     3 spaces  base push  decimal   (capacity  3 u.r  ."  kB" ;








\ *** Block No. 9, Hexblock 9

\ Filenames                                           UH 05Oct87

: !name ( addr len fcb -- )
     dup >r filename filenamelen bl fill
     over 1+ c@  Ascii : =
     IF over c@ [ Ascii A 1- ] Literal - >r 2 /string r>
     ELSE 0 THEN  r@ drive c! r> dup filename 2swap
     filenamelen 1+ min bounds
     ?DO  I c@ Ascii . =
         IF drop dup  extension ELSE  I c@ over c! 1+ THEN
     LOOP 2drop ;

: !fcb  ( fcb -- )  dup opened off   name count rot !name ;




\ *** Block No. 10, Hexblock a

\ Print Directory                                     UH 18Nov87

| Create dirbuf  b/rec allot    dirbuf b/rec erase
| Create fcb0    b/fcb allot    fcb0   b/fcb erase

| : wildchard?  ( f c -- f' )  Ascii * = IF drop Ascii ? THEN ;
| : (expand ( addr len -- )  false -rot bounds
     ?DO  I c@  wildchard? dup ?dup IF I c! THEN  LOOP drop ;
| : expand ( fcb -- )  \ expand * to ???
     dup filename 8 (expand   extension 3 (expand ;

: (dir  ( addr len -- )
    fcb0 !name fcb0 expand  dirbuf dma!  fcb0 >dosfcb search0
    BEGIN dup dos-error? not
    WHILE  $20 * dirbuf + dosfcb> tab .dosfile
           fcb0 >dosfcb searchnext  stop? UNTIL  drop ;

\ *** Block No. 11, Hexblock b

\ File List                                           UH 10Oct87

User file-link   file-link off

| : #file ( -- n )   file-link @ dup IF fileno c@ THEN 1+ ;


Forth definitions

: forthfiles ( -- )
   file-link @
   BEGIN dup WHILE  cr  dup .fcb  @ stop? UNTIL drop ;

Dos definitions



\ *** Block No. 12, Hexblock c

\ Close a file                                        UH 10Oct87

' save-buffers >body $0C + @ | Alias backup

| : filebuffer?  ( fcb -- fcb bufaddr/flag )
   prev  BEGIN @ dup WHILE  2dup 2+ @ = UNTIL ;

| : flushfile  ( fcb -- )  \ flush file buffers
     BEGIN  filebuffer?  ?dup  WHILE
            dup backup  emptybuf  REPEAT  drop ;

: (close  ( fcb -- )  \ close file in fcb
   dup flushfile
   dup opened  dup @ 0= IF 2drop exit THEN  off
   >dosfcb closefile Abort" not found!" ;


\ *** Block No. 13, Hexblock d

\ Create fcbs                                         UH 10Oct87

: !files ( fcb -- )  dup  isfile ! fromfile ! ;

' r@ | Alias newfcb

Forth definitions

: File  ( -- )
   Create here >r   b/fcb allot  newfcb b/fcb erase
          last @ count $1F and  newfcb !name
          #file   newfcb fileno !
          file-link @  newfcb nextfile !   r> file-link !
   Does> !files ;

: direct     0 !files ;

\ *** Block No. 14, Hexblock e

\ flush buffers & misc.                  UH 10Oct87   UH 28Nov87
Dos definitions

: save-files  ( -- )  file-link BEGIN  @ ?dup  WHILE
     dup opened @ IF dup >dosfcb closefile drop THEN  REPEAT ;

' save-files Is save-dos-buffers

\ : close-files  ( -- )  file-link
\     BEGIN  @ ?dup  WHILE  dup (close  REPEAT ;

Forth definitions

: file?   isfile@ .file ;  \ print current file

: list  ( n -- )   3 spaces  file? list ;

\ *** Block No. 15, Hexblock f

\ words for viewing                                   UH 10Oct87

Forth definitions

| $200 Constant viewoffset \ max. %512 kB files

: (makeview  ( -- n )  \ calc. view filed for a name
   blk @ dup  0= ?exit
   loadfile @ ?dup IF fileno @ viewoffset * + THEN ;

: (view      ( blk -- blk' ) \ select file and leave block
   dup 0=exit
   viewoffset u/mod  file-link
   BEGIN @ dup WHILE 2dup fileno @ = UNTIL
   !files drop ;        \ not found: direct access


\ *** Block No. 16, Hexblock 10

\ FORGETing files                                     UH 10Oct87

| : remove?      ( dic symb addr -- dic symb addr f )
     dup heap? IF  2dup u> exit THEN 2 pick over 1+ u< ;


| : remove-files ( dic symb -- dic symb )  \ flush files !
     isfile@      remove? nip  IF direct       THEN
     fromfile @   remove? nip  IF fromfile off THEN
     file-link
     BEGIN @ ?dup WHILE remove? IF dup (close THEN REPEAT
     file-link remove ;





\ *** Block No. 17, Hexblock 11

\ print a list of all buffers                         UH 20Oct86

: .buffers
   prev BEGIN  @ ?dup WHILE  stop? abort" stopped"
           cr dup u. dup 2+ @ dup 1+
           IF ."    Block: " over 4+ @ 5 .r
              ."    File : " [ Dos ] .file
              dup 6 + @ 0< IF ."     updated" THEN
           ELSE ." Buffer empty" drop THEN  REPEAT ;








\ *** Block No. 18, Hexblock 12

\ File Interface User words                           UH 11Oct87

| : same ( addr -- )  >in ! ;
: open      isfile@ (open   offset off ;
: close     isfile@ (close ;
: assign    close   isfile@ !fcb  open ;
: make      isfile@ dup !fcb   (make ;

| : isfile?  ( addr -- addr f )    \ is adr a fcb?
  file-link BEGIN  @ dup 0=exit 2dup body> = UNTIL drop true ;

: use       >in @ name find \ create a fcb if not present
            IF isfile? IF execute drop  exit THEN THEN drop
            dup same File   same ' execute  open ;



\ *** Block No. 19, Hexblock 13

\ File Interface User words                           UH 25May88

: makefile     >in @   File  dup  same ' execute  same make ;
: emptyfile    isfile@ >dosfcb createfile ;

: from         isfile push use ;
: loadfrom     ( n -- )
               isfile push  fromfile push  use load   close ;
: include ( -- )
  rec-offset push  isfile push  fromfile push
  use  cr file?
  include-inner
  incfile @
    IF increc @ incfile @ cr+ex!
    incfile @ readrec Abort" error re-reading after include"
    THEN ;

: eof ( -- f ) isfile@  dup filesize @ swap record @ = ;

: files        " *.*" count (dir ;
: files"       Ascii " word count 2dup upper (dir ;

' files Alias dir     ' files" Alias dir"

\ *** Block No. 20, Hexblock 14

\ extend Files                                        UH 20Nov87

| : >fileend   isfile@ >dosfcb size drop ;

| : addblock  ( n -- )  \ add block n to file
     dup buffer under   b/blk  bl fill
     isfile@   rec/blk  over filesize +!  false file-r/w
     IF close Abort" disk full!" THEN ;

: more ( n -- )   open >fileend
   capacity swap bounds  ?DO I addblock LOOP close
   open close ;

: Drive: ( n -- n' ) dup Constant 1+ Does> @ drive! ;
0   Drive: a:   Drive: b:   Drive: c:   Drive: d:
5 + Drive: j:  drop

\ *** Block No. 21, Hexblock 15

\ save memory-image as disk-file                      UH 29Nov86

Forth definitions

: savefile ( from count -- ) \ filename
   isfile push  makefile bounds
   ?DO I dma!  isfile@ >dosfcb write-seq  Abort" disk full!"
       b/rec +LOOP  close ;









\ *** Block No. 22, Hexblock 16

\ Status                                              UH 10OCt87


: .blk ( -- )   blk @ ?dup 0=exit
   dup 1 = IF cr file? THEN base push hex ."  Blk " . ?cr ;

' .blk Is .status










\ *** Block No. 23, Hexblock 17

File source.fb      \  Define already existing Files
File fileint.fb     File startup.fbr

' (makeview     Is makeview
' remove-files  Is custom-remove
' file-r/w      Is r/w
' noop          Is drvinit
 \      include startup.fb \ load Standard System
