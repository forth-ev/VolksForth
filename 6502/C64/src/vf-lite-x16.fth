
include vf-tc-prep.fth

include vf-trg-x16.fth

\ The actual volksForth sources

include vf-head-x16.fth
include vf-cbm-core.fth
include vf-sys-x16.fth
include vf-cbm-file.fth
include vf-finalize.fth
  9f00 ' limit >body !  9b00 s0 !  9f00 r0 !
include vf-memsetup.fth

include vf-pr-target.fth
quit
