\ *** Block No. 12, Hexblock c

\ ramfill                             3:

Onlyforth

Code ramfill   ( adr n 8b -)
 sei  34 # lda  1 sta
 3 # lda setup jsr
 N 3 + ldx txa  N 2+ ora 0<>
  ?[ N lda 0 # ldy
    [[ 0 # cpx 0<>
      ?[[ [[ N 4 + )Y sta iny 0= ?]
          N 5 + inc dex ]]?
   N 2+ ldx 0<> ?[
   [[ N 4 + )Y sta iny N 2+ cpy CS ?] ]?
  ]?
 36 # lda  1 sta cli
 0 # ldx 1 # ldy Next jmp
end-code

$C000 $4000 (16 $300 - C)  0 ramfill

forget ramfill


\ *** Block No. 13, Hexblock d

( Deleting Assembler Labels bp27jun85we)

: delete   Assembler name find
 IF  >name count $1F and
   bounds ?DO $1F I c! LOOP
 ELSE  count type space THEN ;

delete setup     delete xyNext
delete Puta      delete SP
delete Pop       delete Next
delete N         delete UP
delete Poptwo    delete W
delete IP        delete RP
delete Push      delete Push0A
delete PushA     delete ;c:

forget delete Onlyforth


\ *** Block No. 14, Hexblock e

( Definition for  .status     28jun85we)

: status
 blk @ ?dup IF
  ."  blk " u.
  ." here "  here u.
  ." there " there u.
  ." heap "  heap u.  cr
  THEN ;

' status is .status
