
\ : .blk|tib
\     blk @ ?dup IF  ."  Blk " u. ?cr  exit THEN
\     incfile @ IF tib #tib @ cr type THEN ;

include logfile.fth
logopen

include ans-shim.fth
: \vf  [compile] \ ; immediate

include prelim.fth
include tester.fth
\ 1 verbose !
include core.fr
include coreplus.fth

include util.fth
include errorrep.fth

include coreext.fth

\ ' .blk|tib Is .status

include doubltst.fth

REPORT-ERRORS

logclose
