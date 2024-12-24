
\ : .blk|tib
\     blk @ ?dup IF  ."  Blk " u. ?cr  exit THEN
\     incfile @ IF tib #tib @ cr type THEN ;

include log2file.fs
logopen output.log

include ans-shim.fs
: \vf  [compile] \ ; immediate

include prelim.fs
include tester.fs
\ 1 verbose !
include core.fs
include coreplus.fs

include util.fs
include errorrep.fs

include coreext.fs

\ ' .blk|tib Is .status

include doubltst.fs

REPORT-ERRORS

logclose
