
  include extend2.fs
\needs drv  : drv 2 ;  \ showing C: if drv isn't defined
  include multivid.fs

\ : .blk|tib
\     blk @ ?dup IF  ."  Blk " u. ?cr  exit THEN
\     incfile @ IF tib #tib @ cr type THEN ;

\ ' .blk|tib Is .status

\  include dos2.fs
  include dos3.fs
  include log2file.fs
