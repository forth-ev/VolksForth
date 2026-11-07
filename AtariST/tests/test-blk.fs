
include fileint.fs

use log2file.fb  4 5 thru  \ so that include with block file gets tested
' noop Is .status
\ ' .blk|tib IS .status

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
include doubltst.fs

include block.fs

REPORT-ERRORS

logclose

