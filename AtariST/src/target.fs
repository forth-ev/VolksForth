\ Target compiler loadscr                              09sep86we
\ Idea and first Implementation by ks/bp
\ Implemented on 6502  by ks/bp
\ ultraFORTH83-Version by bp/we
\ Atari 520 ST - Version by we

 : ?.s  state @ 0= IF .s THEN ;
 : .stat .blk ?.s ;     ' .stat Is .status
\ : .stat .blk|tib ?.s ;     ' .stat Is .status
\ : .stat order cr .blk|tib ;     ' .stat Is .status

\  : .blk|tib+.s  ( -- )
\    blk @ ?dup IF ." Blk " u.  .s ." ::" ?cr  exit THEN
\    incl-file @ IF tib #tib @ type cr THEN 
\    state @ 0= IF ." stack: " .s ." ::" cr THEN ;
    
\    ' .blk|tib+.s Is .status
    
Onlyforth      Assembler nonrelocate
07 Constant imagepage     \ Virtual memory bank
Vocabulary Ttools
Vocabulary Defining

use target.fb

\   1 $12 +thru   \ Target compiler
\ $13 $15 +thru   \ Target Tools
\ $16 $18 +thru   \ Redefinitions
\ save  $19 $22 +thru  \ Predefinitions

  2 $13 thru   \ Target compiler
$14 $16 thru   \ Target Tools
$17 $19 thru   \ Redefinitions
save  $1a $23 thru  \ Predefinitions
