
\ with build log:
' noop alias \log
\ without build log:
\ ' \ alias \log

\log include logtofile.fth

include vf-tc-prep.fth

\log logopen" v4th-x16e.log"

include vf-trg-x16.fth

\ The actual volksForth sources

include vf-head-x16.fth
include vf-cbm-core.fth
include vf-sys-x16.fth
include vf-cbm-file.fth
include vf-x16edit.fth
include vf-finalize.fth
  9f00 ' limit >body !  9b00 s0 !  9f00 r0 !
include vf-memsetup.fth

include vf-pr-target.fth
.( target compile complete) cr
\log logclose

quit
