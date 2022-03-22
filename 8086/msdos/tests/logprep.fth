
  include extend2.fth
\needs drv  : drv 2 ;  \ showing C: if drv isn't defined
  include multivid.fth

\ : .blk|tib
\     blk @ ?dup IF  ."  Blk " u. ?cr  exit THEN
\     incfile @ IF tib #tib @ cr type THEN ;

\ ' .blk|tib Is .status

\  include dos2.fth
  include dos3.fth
  include log2file.fth
