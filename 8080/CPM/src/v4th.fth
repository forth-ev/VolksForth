
Onlyforth
    $9000 displace !
Target definitions   $100 here!

  include vf-core.fth
  include vf-io.fth
  include vf-bufs.fth
  include vf-sys.fth
  use source.fb
  $74 $75 thru    \ Standard 8080-System

cr .( unresolved: )  .unresolved   ( ' .blk is .status )

save-target  V4TH.COM
