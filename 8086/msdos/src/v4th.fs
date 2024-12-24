
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

  new v4th.com   Onlyforth Target definitions

  include vf86core.fs
  include vf86dkey.fs
  include vf86dos.fs
  include vf86file.fs
  include vf86end.fs

\log logclose
  flush
\log logreopen

  cr .( new kernel written as v4th.com) cr

\log logclose
