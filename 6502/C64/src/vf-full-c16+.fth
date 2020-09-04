
include vf-tc-prep.fth

include vf-trg-c16+.fth

\ The actual volksForth sources

include vf-head-c16.fth
include vf-cbm-core.fth
include vf-sys-c16.fth
include vf-cbm-file.fth
include vf-cbm-bufs.fth
include vf-finalize.fth
  fd00 ' limit >body !  bc00 s0 !  c000 r0 !
include vf-memsetup.fth

include vf-pr-target.fth
quit
