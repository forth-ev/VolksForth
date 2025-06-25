
\ file interface for use by the target compiler

\ *** Block No. 2, Hexblock 2

\ File functions for save-system                     cas20130105

: arguments ( n -- )
     depth 1- > abort" not enough Parameters" ;

Vocabulary Dos   Dos also definitions

| Code (createfile   ( C$ -- handle )
   0 # A7 -) move               \ normal file, no protection
   SP )+ D6 move   D6 reg) A0 lea   .l A0 A7 -) move
   .w $3C # A7 -) move   1 trap   8 A7 addq
   D0 SP -) move   Next   end-code

| Code (closefile    ( handle -- f )
   SP )+  A7 -) move
   $3E # A7 -) move   1 trap   4 A7 addq
   D0 SP -) move   Next   end-code


\ *** Block No. 3, Hexblock 3

\ write into file                                    cas20130105

| Code (filewrite  ( buff len handle -- n )
   SP )+ D0 move   .l D2 clr  .w  SP )+ D2 move
   SP )+ D6 move   D6 reg) A0 lea
   .l  A0 A7 -) move           \ buffer adress
       D2 A7 -) move           \ buffer length
   .w  D0 A7 -) move           \ handle
    $40 # A7 -) move           \ call  WRITE
   1 trap    $0C # A7 adda
   D0 SP -) move               \ errorflag, num written Bytes
   Next  end-code





\ *** Block No. 4, Hexblock 4

\ save-system                                        cas20130105

\ save-system removed from tfileint.fs







\ *** Block No. 5, Hexblock 5

\ disk errors                                          13oct86we


| ' 2-   Alias body>            \ just for style





| : 2digits   ( n -- adr len )
    base push  decimal   extend <# # # #> ;

| 0 Constant #adr
        \ will hold the adr of "00" in following abort" ..."


\ *** Block No. 6, Hexblock 6

\ disk errors                                        cas20130105

: .diskerror  ( -n -- )     negate
    &13 case? abort" disk is proteced"
    &33 case? abort" file not found"
    &34 case? abort" path not found"
    &36 case? abort" access denied"
    &37 case? abort" illegal handle#"
    &46 case? abort" illegal drive num"
    2digits  #adr swap   cmove
    true     [ here 2+      ( adress of counted string )   ]
    abort" Dos-Error #00"
             [ count +  2-  ' #adr >body !  ( adr of "00") ] ;

: ?diskabort   ( -n -- )    dup 0< IF .diskerror  THEN  drop ;


\ *** Block No. 7, Hexblock 7

\ File control block structure                         09sep86we

cr .( pzdebug order) order cr

| : Fcbyte ( n len -- n' )   \ defining word for fcb contents
    Create over c, +  does>  c@ + ;

words

&25 Constant filenamelen      \ only SHORT pathes will fit !
|  0  2 Fcbyte nextfile       \ link to next file
filenamelen Fcbyte filename       \ name of file
      4 Fcbyte filesize       \ size in Bytes ,  low..high
      2 Fcbyte filehandle     \ handle from GEMdos
      2 Fcbyte fileno         \ fileno. for VIEW
    Constant b/fcb            \ bytes per file

: handle        ( -- n )  isfile@ filehandle @ ;

\ *** nextfile must be the first field !

\ *** Block No. 8, Hexblock 8

\ position into block                                  13oct86we

Code lseek      ( d handle n -- d' )
   SP )+ A7 -) move    SP )+ A7 -) move    .l SP )+ A7 -) move
   .w $42 # A7 -) move   1 trap    $0A # A7 adda
   .l D0 SP -) move   Next  end-code

: position      ( d handle -- f )
   0 lseek   0< ?exit   drop false ;

: position?     ( handle -- d )
   0 0 rot  1  lseek   dup 0<  IF  ?diskabort  THEN ;





\ *** Block No. 9, Hexblock 9

\ read and write a memory area                       cas20130105

Code (fileread   ( buff len handle -- n )
   SP )+ D0 move   .l D2 clr  .w  SP )+ D2 move
   SP )+ D6 move   D6 reg) A0 lea
   .l  A0 A7 -) move           \ buffer adress
       D2 A7 -) move           \ buffer length
   .w  D0 A7 -) move           \ handle
    $3F # A7 -) move           \ call  READ
   1 trap    $0C # A7 adda
   D0 SP -) move               \ errorflag or bytes read
   Next  end-code

\ *** Block No. 10, Hexblock a

\ (open-file  setdta                                   26oct86we

Code (openfile  ( C$ -- handle )
   2 # A7 -) move
   SP )+ D6 move   D6 reg) A0 lea   .l A0 A7 -) move
   .w $3D # A7 -) move   1 trap   8 A7 addq
   D0 SP -) move   Next   end-code

Create dta      &44 allot

Code setdta     ( addr -- )
   SP )+ D6 move   D6 reg) A0 lea   .l A0 A7 -) move
   .w $1A # A7 -) move   1 trap   6 A7 addq   Next   end-code

\ *** Block No. 11, Hexblock b

\ search for files                                     03oct86we

Code search0    ( C$ attr -- f )    \ search for first file
   SP )+ A7 -) move    SP )+ D6 move   D6 reg) A0 lea
   .l A0 A7 -) move   .w $4E # A7 -) move   1 trap   8 A7 addq
   D0 SP -) move    Next  end-code

Code searchnext    ( -- f )         \ search for next file
   $4F # A7 -) move   1 trap   2 A7 addq
   D0 SP -) move    Next  end-code







\ *** Block No. 12, Hexblock c

\ Create a subdir                                   bp 11 oct 86

Code (makedir  ( C$ -- f )     \ Create a subdir
   $39 # D1 move
Label long-adr
   SP )+ D6 move   D6 reg) A0 lea   .l A0 A7 -) move
   .w D1 A7 -) move   1 trap   6 A7 addq
   D0 SP -) move   Next  end-code

Code (setdir     ( C$ -- f )
   $3B # D1 move   long-adr bra    end-code






\ *** Block No. 13, Hexblock d

\ select drive                                         09sep86we

Code setdrive   ( n -- )
   SP )+ A7 -) move
   $0E # A7 -) move   1 trap   4 A7 addq   Next end-code

Code getdrive   ( -- n )
   $19 # A7 -) move   1 trap   2 A7 addq
   D0 SP -) move   Next   end-code

Code getdir     ( addr n -- f )  \ n is drive, string in addr
   SP )+ A7 -) move   SP )+ D6 move   D6 reg) A0 lea
   .l A0 A7 -) move   .w $47 # A7 -) move   1 trap   8 A7 addq
   D0 SP -) move   Next   end-code



\ *** Block No. 14, Hexblock e

\ file sizes                                          b30aug86we

: (capacity  ( fcb -- n)             \ calculates size in blocks
   filesize 2@   2dup or  0= IF  drop exit  THEN
   b/blk  um/mod  swap  IF  1+  THEN ;  \ add 1 block for rest

| : in-range  ( block fcb -- f) \ makes sure, block is in file
     (capacity  u< not &36 * ;    \ Errorcode -&36









\ *** Block No. 15, Hexblock f

\ read and write into files                         bp 11 oct 86

| : set-pos   ( block handle -- f)
      >r  b/blk um*  r>  position ;

| : fileaccess   ( buff block fcb -- buff len handle/ errorcode)
      2dup  in-range  ?dup IF >r 2drop drop r> rdrop exit THEN
      filehandle @   under  set-pos
                      ?dup IF >r 2drop      r> rdrop exit THEN
      b/blk swap ;

| : fileread     ( buff block fcb -- ff / errorcode )
      fileaccess  (fileread  dup 0>  IF  drop false  THEN ;

| : filewrite    ( buff block fcb -- ff / errorcode )
      fileaccess  (filewrite  dup 0>  IF  drop false  THEN ;

\ *** Block No. 16, Hexblock 10

\ twiggling the file variables                      bp 11 oct 86

: scan-name     ( C$ -- adr len')  \ length of "C"-string
     $1000 over swap  0 scan  drop  over - ;

: .file ( fcb --)                  \ print only filename
     ?dup 0=  IF  ." DIRECT ! " exit  THEN  body> >name .name ;

: .fcb      ( fcb -- )             \ print filename
     dup filehandle @ 2 .r   dup filesize 2@  6 d.r   3 spaces
     dup .file  2 spaces  filename scan-name type ;

: !files   ( fcb -- )              \ set file and isfile
      dup  isfile !  fromfile ! ;



\ *** Block No. 17, Hexblock 11

\ PATHes                                            bp 11 oct 86

| &30 Constant pathlen          \ max. len of all pathes

Variable  pathes  pathlen allot \ counted string of pathes
     pathes off

: pathes?       ( -- )          \ print a list of the pathes
     cr  3 spaces  pathes count type ;

: setpath       ( adr len --)   \ set's the list of pathes
     pathlen min   pathes place
     Ascii ;  pathes count + c!   pathes c@ 1+ pathes c! ;


\ *** Block No. 18, Hexblock 12

\ search for files                                  bp 11 oct 86

Variable workspace    &64 allot       \ place for c$

| : try.path   ( adr len fcb attr -- f )
    2swap   workspace swap   2dup + >r   move
    swap   filename  r>  filenamelen cmove
    workspace   swap  search0 0= ;

| : makec$     ( adr len -- c$ )        \ make adr len to a c$
    workspace swap  2dup + >r   move
    r> off  ( make a c$ ) workspace ;





\ *** Block No. 19, Hexblock 13

\ "                                                 bp 11 oct 86

| Variable sfile                       \ "dirty" variable
| 7 Constant defaultattr               \ find all filetypes

| : path@       ( adr len -- adr len1 adr len2) \ isolate a path
     Ascii ; skip   2dup  2dup  Ascii ; scan   nip -  ;

: (searchfile   ( fcb -- ff/ C$ f)     \ search for file in path
   sfile !    pathes count             \ and in act. directory
   BEGIN   path@  sfile @  defaultattr   try.path
                  IF  2drop  workspace true  exit  THEN
           Ascii ; scan   dup 0=  UNTIL  nip ;

: searchfile   ( fcb -- C$ )   \ file was found in path
   (searchfile ?exit    -&33 ?diskabort ;

\ *** Block No. 20, Hexblock 14

\ open a file, filer/w                                b26oct86we

| : @length       ( -- d)          dta  &26 +   2@ ;
| : copylength    ( fcb --)        @length  rot filesize 2! ;

: (open         ( fcb --)       \ open file
     dup  filehandle @  IF  drop exit  THEN
     dta setdta  dup searchfile  over copylength    (openfile
          dup ?diskabort   swap filehandle ! ;

Forth definitions

: capacity      ( -- n)
    isfile@ ?dup  IF  dup (open (capacity  exit THEN  blk/drv ;

Dos definitions

\ *** Block No. 21, Hexblock 15

\ filer/w, Create a file                            bp 11 oct 86

: filer/w       ( buff block fcb f -- f)
     over  0= IF  STr/w exit  THEN
     over (open
     IF  fileread  ELSE  filewrite  THEN  dup ?diskabort ;

: createfile    ( fcb --)       \ create a file in fcb
   dup filename (createfile     dup ?diskabort
   over filehandle !      0 0 rot filesize  2!
   offset off ;






\ *** Block No. 22, Hexblock 16

\ store names for files                             bp 11 oct 86

| : !name       ( adr len --)      \ store name in record
   2dup erase     >r  name count
   dup  r>   < not abort" string too long"
   >r swap r> cmove ;

: !fcb          ( fcb --)          \ next word is filename
   dup filehandle off   filename  filenamelen !name ;








\ *** Block No. 23, Hexblock 17

\ print dta and directory                              26oct86we

| : .dtaname      ( addr --)       \ addr is addr of name
      dup  BEGIN  dup c@  ?dup  WHILE  emit  1+  REPEAT
       -  &15 +  spaces ;

: .dta          ( --)           \ print contents of dta
     cr  dta &21 +  c@ $10 and
     IF  Ascii D  ELSE  bl  THEN emit   space
     dta &30 +  .dtaname   @length  &10 d.r ;

: (dir          ( attr adr len --)   \ given a match string
     makec$  swap   dta setdta   search0
     BEGIN  0=  WHILE  stop? 0= WHILE .dta  searchnext  REPEAT ;



\ *** Block No. 24, Hexblock 18

\ primitives for fcb's                                bp 18May86

User file-link   file-link off    \ list thru files

| : #file       ( -- n)         \ View number of next file
   file-link @ dup  IF  fileno @  THEN  1+ ;


: forthfiles         ( --)      \ print a list of :
    file-link @             \ forthword,filename,handle,len
    BEGIN  dup  WHILE
       cr   dup .fcb     @  stop? UNTIL drop ;





\ *** Block No. 25, Hexblock 19

\ Close a file                                        bp 18May86

|  ' save-buffers  >body $C  + @  Alias backup

| : filebuffer?        ( fcb -- fcb bufaddr/flag)
   prev  BEGIN  @ dup  WHILE  2dup  2+ @  =  UNTIL ;

| : flushfile          ( fcb -- )       \ flush file buffers
   BEGIN  filebuffer?  ?dup  WHILE
          dup backup  emptybuf  REPEAT  drop ;

: (close        ( fcb --)       \ close file in fcb
   dup flushfile
   filehandle dup @   ?dup 0= IF  drop exit THEN   swap off
   (closefile -$41 case? ?exit  ?diskabort  ;


\ *** Block No. 26, Hexblock 1a

\ Create fcb's                                      bp 11 oct 86

Forth definitions


: File          ( -- )          \ Create a fcb
     Create  here  b/fcb allot   dup b/fcb erase
             #file  over fileno !
             file-link @   over file-link !  swap !
    does>  !files  ;

: direct        0 !files ;      \ switch to direct access





\ *** Block No. 27, Hexblock 1b

\ flush buffers & misc.                                bp 8jun86

: flush         ( --)           flush  file-link
   BEGIN  @ ?dup  WHILE  dup (close  REPEAT ;

: file?    isfile@  .file ;        \ print current file

: list          ( n --)
   3 spaces  file?  list ;

: path          ( -- )          \ this is a smart word !
   name count
   dup 0=   IF  2drop  pathes?  exit  THEN
   dup 1 =  IF  over c@  Ascii ;  =
                 IF  2drop  pathes off  exit  THEN  THEN
   setpath ;

\ *** Block No. 28, Hexblock 1c

\ File Interface User words                            26oct86we

| : isfile?     ( adr -- adr f)  \ is adr a fcb ?
   file-link  BEGIN  @ dup 0= ?exit  2dup 2- = UNTIL drop true ;

| : ?isfile@   isfile@ body>
               isfile? 0= abort" not in direct mode"   >body ;

: open         ?isfile@ (open   offset off ;
: close        ?isfile@ (close ;
: assign       close  isfile@ !fcb  open ;
: make         ?isfile@ dup !fcb  createfile ;

: use          >in @  name find  \ create a fcb if not present !
   IF  isfile?  IF execute drop  exit THEN THEN drop
   dup >in ! File    dup >in ! ' execute    >in !  assign ;

\ *** Block No. 29, Hexblock 1d

\ File Interface User words                         bp 11 oct 86

: makefile     >in @  file  dup >in ! ' execute  >in ! make ;

: from         isfile push  use ;         \ sets only fromfile
: loadfrom     ( n --)                    \ load 1 scr from file
               isfile push  fromfile push   use load   close ;
: include      1 loadfrom ;

: eof           ( -- f)                   \ end of file ?
   isfile@  dup  filehandle @  position?
             rot  filesize  2@  d= ;

: files         $10   " *.*"   count  (dir ;
: files"        $10 Ascii "  word count (dir ;


\ *** Block No. 30, Hexblock 1e

\ extend files                                      bp 11 oct 86

| : >fileend    isfile@ filesize 2@   handle  position
                ?diskabort ;

| : addsize     isfile@ filesize dup  2@ b/blk 0 d+  rot 2! ;

| : addblock    ( n --)         \ add block n to file
    buffer b/blk  2dup bl fill  >fileend  handle (filewrite
    dup ?diskabort   b/blk -
    IF  close  abort" Disk voll" THEN  addsize ;

: (more   ( n --)
    capacity swap bounds ?DO  I addblock  LOOP ;

: more    ( n --)     ?isfile@  (open  (more  close ;

\ *** Block No. 31, Hexblock 1f

\ make,kill and set directories                     bp 11 oct 86

| : dir$        ( -- adr )      name count   makec$ ;

: makedir       dir$ (makedir ?diskabort ;

: dir           name count
                0 case? IF  getdrive 2dup 1+ getdir ?diskabort
                            cr 3 spaces  Ascii A + emit   ." :"
                            scan-name type exit  THEN
                makec$ (setdir ?diskabort ;

| : driveset    Create c,  Does>  c@ setdrive ;
0 driveset A:   1 driveset B:   2 driveset C:   3 driveset D:



\ *** Block No. 32, Hexblock 20

\ words for VIEWing                                 bcas20130105

| $200 Constant viewoffset      \ max. &512 kbyte long files

| : (makeview     ( -- n)       \ calc. view field for a name
   blk @  dup  0= ?exit
   loadfile @  ?dup  IF  fileno @  viewoffset *   +  THEN  ;

: (view         ( blk -- blk')  \ select file and leave block
   dup  0= ?exit
   viewoffset  u/mod  file-link
   BEGIN  @ dup  WHILE  2dup fileno @ = UNTIL
   dup  searchfile drop   \ file not found : abort
   !files  drop  ;



\ *** Block No. 33, Hexblock 21

\ ugly FORGETing of files                           bp 11 oct 86

: remove?            ( dic symb addr -- dic symb addr f)
   dup heap?  IF  2dup u>  exit  THEN  2 pick  over 1+ u< ;

| : remove-files ( dic symb -- dic symb)   \ flush files !
   isfile   @ remove?  nip  IF  0 !files      THEN
   fromfile @ remove?  nip  IF  fromfile off  THEN
   file-link
   BEGIN  @ ?dup  WHILE  remove?  IF  dup (close  THEN  REPEAT
   file-link remove ;






\ *** Block No. 34, Hexblock 22

\ convey for files                                  bp 11 oct 86

| : togglefiles    ( -- )      \ changes isfile and fromfile
     isfile@  fromfile @   isfile !  fromfile ! ;

: convey           ( [blk1 blk2] [to.blk --)
   3 arguments   >r  2dup swap -  >r
   togglefiles       dup capacity 1- >
   togglefiles  r> r@ +  capacity 1- >
    or abort" wrong range!"
   r>  convey ;



' (makeview     Is makeview
' remove-files  Is custom-remove
' filer/w       Is r/w
