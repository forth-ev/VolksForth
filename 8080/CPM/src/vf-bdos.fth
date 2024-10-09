\ *** Block No. 119, Hexblock 77

\ CP/M-Interface                                         05Oct87
Vocabulary Dos    Dos definitions  also
Label >bios   pchl
Code biosa ( arg fun -- res )
   1 lhld   D pop   D dcx   D dad   D dad   D dad
   D pop   IP push   D IP mvx   >bios call
Label back
   IP pop   0 H mvi   A L mov   Hpush jmp   end-code

Code bdosa ( arg fun -- res )
   H pop   D pop   IP push   L C mov   5 call  back jmp
end-code

: bios ( arg fun -- ) biosa drop ;
: bdos ( arg fun -- ) bdosa drop ;


\ *** Block No. 120, Hexblock 78

\ Character-IO Constants  Character input                05Oct87

Target Dos also

$08 Constant #bs         $0D Constant #cr
$0A Constant #lf         $1B Constant #esc
$09 Constant #tab        $7F Constant #del
$07 Constant #bel        $0C Constant #ff

: con!   ( c -- )     4 bios ;
: (key?  ( -- ? )   0 2 biosa 0= not ;
: getkey ( -- c )   0 3 biosa ;

: (key   ( -- c )   BEGIN  pause (key?  UNTIL getkey ;



\ *** Block No. 121, Hexblock 79

\ Character output                           07Oct87  UH 27Feb88

| Code ?ctrl ( c -- c' )  H pop   L A mov
    $20 cpi  cs ?[ $80 ori ]?   A L mov   Hpush jmp   end-code

: (emit ( c -- )  ?ctrl  con!  pause ;

: (cr     #cr con!  #lf con! ;
: (del    #bs con!  bl  con!  #bs con! ;
: (at? ( -- row col )  0 0 ;

: tipp ( addr len -- )   0 ?DO count emit LOOP drop ;

Output: display    [ here output ! ]
   (emit (cr tipp (del noop 2drop (at? ;


\ *** Block No. 122, Hexblock 7a

\ Line input                                             04Oct87

| : backspace ( addr pos1 -- addr pos2 ) dup 0=exit (del 1- ;

: (decode ( addr pos1 key -- addr pos2 )
     #bs  case? IF backspace         exit THEN
     #del case? IF backspace         exit THEN
     #cr  case? IF dup span !  space exit THEN
     dup emit >r  2dup +  r> swap c!  1+ ;

: (expect   ( addr len -- )  span ! 0
    BEGIN  span @ over u>  WHILE  key decode  REPEAT  2drop ;

Input:  keyboard    [ here input ! ]
     (key  (key?  (decode  (expect ;


\ *** Block No. 123, Hexblock 7b

\ Default Disk Interface: Constants and Primitives       18Nov87

 $80 Constant b/rec              b/blk b/rec / Constant rec/blk

Dos definitions
' 2- | Alias dosfcb>         ' 2+ | Alias >dosfcb

: dos-error? ( n -- f )   $FF = ;

$5C Constant fcb
: reset     (   --     )  0 &13 bdos ;
: openfile  ( fcb -- f )    &15 bdosa dos-error? ;
: closefile ( fcb -- f )    &16 bdosa dos-error? ;
: dma!      ( dma --   )    &26 bdos  ;
: rec@      ( fcb -- f )    &33 bdosa ;
: rec!      ( fcb -- f )    &34 bdosa ;

\ *** Block No. 124, Hexblock 7c

\ Default Disk Interface: open and close                 20Nov87

Target Dos also     Defer drvinit       Dos definitions

| Variable  opened
: default  ( -- )  opened off
   fcb 1+ c@ bl = ?exit   $80 count here place   #tib off
   fcb dup  dosfcb> dup  isfile ! fromfile !
   openfile Abort" default file not found!"  opened on  ;
' default Is drvinit

: close-default ( -- ) opened @ not ?exit
    fcb closefile Abort" can't close default-file!" ;
' close-default Is save-dos-buffers



\ *** Block No. 125, Hexblock 7d

\ Default Disk Interface: read/write                     14Feb88

Target Dos also

| : rec# ( 'dosfcb -- 'rec# )  &33 + ;

: (r/w  ( adr blk file r/wf -- flag )  >r
    dup 0= Abort" no Direct Disk IO supported! " >dosfcb
    swap rec/blk *  over rec#   0 over 2+ c!   !
    r> rot  b/blk bounds
    DO I dma!  2dup IF rec@ drop
       ELSE rec! IF 2drop true endloop exit THEN THEN
       over rec#   0 over 2+ c!  1 swap +!
    b/rec +LOOP  2drop false ;

' (r/w Is r/w

\ *** Block No. 126, Hexblock 7e

\ Postlude                                               20Nov87

Defer postlude

| : (bye ( -- )   postlude 0 0 bdos ;

| : #pages ( -- n ) here $100 -  $100 u/mod swap 0=exit 1+ ;

: .size ( -- ) base push  decimal
     cr ." Size: &" #pages u. ." Pages" ;

' .size Is postlude

