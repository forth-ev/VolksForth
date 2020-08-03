
include vf-tc-prep.fth

include vf-trg-c16-.fth

\ The actual volksForth sources

include vf-head-c16.fth
include vf-cbm-core.fth
include vf-sys-c16.fth
include vf-cbm-file.fth
include vf-cbm-bufs.fth
include vf-finalize.fth
  8000 ' limit >body !  7700 s0 !  7b00 r0 !
include vf-memsetup.fth

include vf-pr-target.fth
quit
