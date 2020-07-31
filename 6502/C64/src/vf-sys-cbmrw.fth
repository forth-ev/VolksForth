
\ *** Block No. 140, Hexblock 8c
8c fthpage

( s#>s+t  x,x                 28may85re)

165 | Constant 1.t
1EA | Constant 2.t
256 | Constant 3.t

| : (s#>s+t ( sector# -- sect track)
      dup 1.t u< IF 15 /mod exit THEN
 3 +  dup 2.t u< IF 1.t - 13 /mod 11 +
                            exit THEN
      dup 3.t u< IF 2.t - 12 /mod 18 +
                            exit THEN
 3.t - 11 /mod 1E + ;

| : s#>t+s  ( sector# -- track sect )
 (s#>s+t  1+ swap ;

| : x,x ( sect track -- adr count)
 base push  decimal
 0 <# #s drop Ascii , hold #s #> ;


\ *** Block No. 141, Hexblock 8d
8d fthpage

( readsector writesector      28may85re)

100 | Constant b/sek

: readsector  ( adr tra# sect# -- flag)
 disk 0F busout
 " u1:13,0," count   bustype
 x,x bustype busoff pause
 derror? ?exit
 disk 0D busin b/sek businput busoff
 false ;

: writesector  ( adr tra# sect# -- flag)
 rot disk 0F busout
 " b-p:13,0" count bustype busoff
 disk 0D busout b/sek bustype busoff
 disk 0F busout
 " u2:13,0," count  bustype
 x,x bustype busoff pause  derror? ;


\ *** Block No. 142, Hexblock 8e
8e fthpage

( 1541r/w                     28may85re)

: diskopen  ( -- flag)
 disk 0D busopen  Ascii # bus! busoff
 derror? ;

: diskclose ( -- )
 disk 0D busclose  busoff ;

: 1541r/w  ( adr blk file r/wf -- flag)
 swap Abort" no file"
 -rot  blk/drv /mod  dup (drv ! 3 u>
 IF . ." beyond capacity" nip exit  THEN
 diskopen  IF  drop nip exit  THEN
 0 swap   2* 2* 4 bounds
 DO  drop  2dup I rot
     IF    s#>t+s readsector
     ELSE  s#>t+s writesector THEN
     >r b/sek + r> dup  IF  LEAVE  THEN
 LOOP   -rot  2drop  diskclose  ;

' 1541r/w  Is   r/w


\ *** Block No. 143, Hexblock 8f
8f fthpage

\ index findex ink-pot         05nov87re

: index ( from to --)
 1+ swap DO
   cr  I 3 .r  I block 28 type
   stop?  IF LEAVE THEN  LOOP ;

: findex ( from to --)
 diskopen  IF  2drop  exit  THEN
 1+ swap DO  cr  I 3 .r
   pad dup I 2* 2* s#>t+s readsector
   >r 28 type
   r> stop? or IF LEAVE THEN
 LOOP  diskclose  ;
