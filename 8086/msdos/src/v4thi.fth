
\ with build log:
' noop alias \log
\ without build log:
\ ' \ alias \log

\log logopen output.log

  \ : .blk|tib
  \     blk @ ?dup IF  ."  Blk " u. ?cr  exit THEN
  \     incfile @ IF tib #tib @ cr type THEN ;

  \ ' .blk|tib Is .status

  Onlyforth

  2 loadfrom META.fb

  new v4thi.com   Onlyforth Target definitions

  include vf86core.fth
  include vf86ikey.fth
  include vf86dos.fth
  include vf86file.fth
  include vf86end.fth

\log logclose
  flush
\log logreopen

  cr .( new kernel written as v4thi.com) cr

\log logclose
