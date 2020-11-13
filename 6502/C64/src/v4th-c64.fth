
\ with build log:
' noop alias \log
\ without build log:
\ ' \ alias \log

\log include logtofile.fth

include vf-tc-prep.fth

\log logopen" v4th-c64.log"

include vf-trg-c64.fth

\ The actual volksForth sources

include vf-head-c64.fth
include vf-cbm-core.fth
include vf-sys-c64.fth
include vf-cbm-file.fth
include vf-finalize.fth
  c000 ' limit >body !  bc00 s0 !  c000 r0 !
include vf-memsetup.fth

include vf-pr-target.fth
.( target compile complete) cr
\log logclose

quit
