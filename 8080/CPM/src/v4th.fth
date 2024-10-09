
Onlyforth
    $9000 displace !
Target definitions   $100 here!

  include vf-core.fth
  include vf-io.fth
  include vf-bufs.fth
  include vf-sys.fth
  use source.fb
  $76 load    \ Standard 8080-System
  include vf-end.fth

cr .( unresolved: )  .unresolved   ( ' .blk is .status )

save-target  V4TH.COM
