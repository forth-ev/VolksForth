\ from fileint.fb.txt:
\ note: most fileint words should go into vocabulary Dos:

\ *** Block No. 18, Hexblock 12

\ search for files                                  bp 11 oct 86

Variable workspace    &64 allot       \ place for c$

| : makec$     ( adr len -- c$ )        \ make adr len to a c$
    workspace swap  2dup + >r   move
    r> off  ( make a c$ ) workspace ;

\ *** Block No. 2, Hexblock 2

\ File functions for save-system                     cas20130105

\ : arguments ( n -- )
\     depth 1- > abort" not enough Parameters" ;

  Code (createfile   ( C$ -- handle )
   0 # A7 -) move               \ normal file, no protection
   SP )+ D6 move   D6 reg) A0 lea   .l A0 A7 -) move
   .w $3C # A7 -) move   1 trap   8 A7 addq
   D0 SP -) move   Next   end-code

  Code (closefile    ( handle -- f )
   SP )+  A7 -) move
   $3E # A7 -) move   1 trap   4 A7 addq
   D0 SP -) move   Next   end-code


\ *** Block No. 5, Hexblock 5

\ disk errors                                          13oct86we

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

Code (filewrite  ( buf len handle -- n )
   SP )+ D0 move   .l D2 clr  .w SP )+ D2 move
   SP )+ D6 move   D6 reg) A0 lea
   .l  A0 A7 -) move           \ buffer adress
       D2 A7 -) move           \ buffer length
   .w  D0 A7 -) move           \ handle
    $40 # A7 -) move           \ call WRITE
   1 trap    $0C # A7 adda
   D0 SP -) move               \ errorflag, num written bytes
   Next  end-code


\ *** Block No. 10, Hexblock a

\ (open-file  setdta                                   26oct86we

| Code (openfile  ( C$ -- handle )
   2 # A7 -) move
   SP )+ D6 move   D6 reg) A0 lea   .l A0 A7 -) move
   .w $3D # A7 -) move   1 trap   8 A7 addq
   D0 SP -) move   Next   end-code

| Create dta      &44 allot

| Code setdta     ( addr -- )
   SP )+ D6 move   D6 reg) A0 lea   .l A0 A7 -) move
   .w $1A # A7 -) move   1 trap   6 A7 addq   Next   end-code


\ search for files                                     03oct86we

| Code search0    ( C$ attr -- f )    \ search for first file
   SP )+ A7 -) move    SP )+ D6 move   D6 reg) A0 lea
   .l A0 A7 -) move   .w $4E # A7 -) move   1 trap   8 A7 addq
   D0 SP -) move    Next  end-code

| Code searchnext    ( -- f )         \ search for next file
   $4F # A7 -) move   1 trap   2 A7 addq
   D0 SP -) move    Next  end-code







\ *** Block No. 12, Hexblock c

\ Create a subdir                                   bp 11 oct 86

| Code (makedir  ( C$ -- f )     \ Create a subdir
   $39 # D1 move
Label long-adr
   SP )+ D6 move   D6 reg) A0 lea   .l A0 A7 -) move
   .w D1 A7 -) move   1 trap   6 A7 addq
   D0 SP -) move   Next  end-code

| Code (setdir     ( C$ -- f )
   $3B # D1 move   long-adr bra    end-code


\ *** Block No. 13, Hexblock d

\ select drive                                         09sep86we

| Code setdrive   ( n -- )
   SP )+ A7 -) move
   $0E # A7 -) move   1 trap   4 A7 addq   Next end-code

| Code getdrive   ( -- n )
   $19 # A7 -) move   1 trap   2 A7 addq
   D0 SP -) move   Next   end-code

| Code getdir     ( addr n -- f )  \ n is drive, string in addr
   SP )+ A7 -) move   SP )+ D6 move   D6 reg) A0 lea
   .l A0 A7 -) move   .w $47 # A7 -) move   1 trap   8 A7 addq
   D0 SP -) move   Next   end-code


\ from include.fb.txt:

\ *** Block No. 2, Hexblock 2

\ incl-variables fread-incl[]                        phz 05jun25

  $80 constant /incl[]
  create incl[  /incl[] allot  variable ]incl
  create incl# 0 , 0 ,
  variable incl>   variable incl-eof
  : incl[]-reset   incl[ dup ]incl ! incl> !   incl-eof off ;
  variable incl-filehandle  incl-filehandle off

  : fread-incl[]  ( -- )
      incl[ /incl[] incl-filehandle @ (fileread
      dup ?diskabort
      dup incl[ + ]incl !   /incl[] u< incl-eof !
      incl[ incl> ! ;


\ *** Block No. 3, Hexblock 3

\ incl-fgetc tibeof eolf?                            phz 04jun25

  : incl-fgetc  ( -- c )
    incl> @ ]incl @ u< 0=
      IF incl-eof @ IF -1 exit THEN fread-incl[] THEN
    incl> @  1 incl> +!  c@  incl# 2@ 1 extend d+ incl# 2! ;

  variable tibeof tibeof off

  : eolf? ( c -- f )  \ end-of-line-or-file?
      \ f=-1: not yet eol; store c and continue
      \ f=0: eol but not yet eof; return line and flag continue
      \ f=1: eof: return line and flag eof
    tibeof off  dup #lf = IF drop 0 exit THEN
    -1 = IF tibeof on  1 ELSE -1 THEN ;


\ *** Block No. 4, Hexblock 4

\ /tib freadline                                     phz 03jun25

  : freadline ( -- eof )
  tib /tib bounds DO
    incl-fgetc dup eolf? under 0< IF I c! ELSE drop THEN
    0< 0= IF I tib - #tib ! ENDLOOP tibeof @ exit THEN
  LOOP /tib #tib !
  ." warning: line exteeds max " /tib . cr
  ." extra chars ignored" cr
  BEGIN incl-fgetc eolf? 1+ UNTIL tibeof @ ;


\ *** Block No. 5, Hexblock 5

\ save/restoretib                                    phz 05jun25

  $50 constant /stash
  create stash[  /stash allot  here constant ]stash
  variable stash>   stash[ stash> !

  : savetib  ( -- n )
      #tib @ >in @ -  0 umin  dup stash> @ + ]stash u>
        abort" tib stash overflow"   >r
      tib >in @ +  stash> @  r@ cmove
      r@ stash> +!  r> ;

  : restoretib  ( n -- )
      dup >r negate stash> +!   stash> @ tib r@ cmove
      r> #tib !  >in off ;


\ *** Block No. 6, Hexblock 6

\ interpret-via-tib                                  phz 03jun25

  : (interpret-from-file  ( filehandle -- )
  incl-filehandle push   incl-filehandle !
  incl# dup push off  incl# 2+ dup push off  incl[]-reset
  BEGIN freadline >r .status >in off interpret
  r> UNTIL  incl-filehandle @ (closefile ?diskabort ;

\ *** Block No. 7, Hexblock 7

\ include                                            phz 03jun25

\ *** adapt open
\ : (open         ( fcb --)       \ open file
\      dup  filehandle @  IF  drop exit  THEN
\      dta setdta  dup searchfile  over copylength    (openfile
\           dup ?diskabort   swap filehandle ! ;

\ : open         ?isfile@ (open   offset off ;
\ : close        ?isfile@ (close ;
\ : assign       close  isfile@ !fcb  open ;

\ : use          >in @  name find  \ create a fcb if not present !
\    IF  isfile?  IF execute drop  exit THEN THEN drop
\    dup >in ! File    dup >in ! ' execute    >in !  assign ;

| : (use-openfile ( -- filehandle )
  name count  2dup cr type bl emit  makec$
  (openfile  dup ?diskabort ;

  : (include ( filehandle -- )
  savetib >r (interpret-from-file  r> restoretib 
  incl-filehandle @
    IF incl# 2@  incl-filehandle @  position  ?diskabort
    incl[]-reset THEN
  ;

  : include ( -- )
  (use-openfile (include
  ;


\ *** Block No. 8, Hexblock 8

\ stashinit \ .blk|tib                               phz 08jun25

  : stashinit  stash[ stash> ! ;
  : (stashquit  stashinit  (quit ;
  : stashrestore  ['] (stashquit IS 'quit ;
  ' stashrestore IS 'restart
    stashinit

  : \  blk @ IF >in @ negate  c/l mod  >in +!
       ELSE #tib @ >in ! THEN ; immediate

  : .blk|tib  ( -- )
    blk @ ?dup IF ." Blk " u. ?cr  exit THEN
    incl-filehandle @ IF tib #tib @ type cr THEN ;

Vocabulary Dos
Dos definitions

' (openfile alias (openfile
' dta alias dta
' setdta alias setdta
' search0 alias search0
' searchnext alias searchnext
' (makedir alias (makedir
' (setdir alias (setdir
' setdrive alias setdrive
' getdrive alias getdrive
' getdir alias getdir

Transient definitions
