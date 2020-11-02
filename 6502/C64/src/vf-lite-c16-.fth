
\ with build log:
' noop alias \log
\ without build log:
\ ' \ alias \log

\log include logtofile.fth

include vf-tc-prep.fth

\log logopen" vf-lite-c16-.log"

include vf-trg-c16-.fth

\ The actual volksForth sources

include vf-head-c16.fth
include vf-cbm-core.fth
include vf-sys-c16.fth
include vf-cbm-file.fth
include vf-finalize.fth
  8000 ' limit >body !  7c00 s0 !  8000 r0 !
include vf-memsetup.fth

include vf-pr-target.fth
.( target compile complete) cr
\log logclose

quit
