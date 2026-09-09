
\ : .blk|tib
\     blk @ ?dup IF  ."  Blk " u. ?cr  exit THEN
\     incfile @ IF tib #tib @ cr type THEN ;

include makefile.fs
include log2file.fs
logopen

include ans-shim.fs
: \vf  [compile] \ ; immediate

\ \atari marks tests that fail with VolksForth 3.85.2
: \atari  [compile] \ ; immediate

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
