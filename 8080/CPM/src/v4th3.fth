
Onlyforth
: status
  ." here "  here u.
  ." there " there u.
  ." displaced there " there displace @ + u.
  ." heap "  heap u.  cr
;

\ ' status is .status

    $8000 displace !
Target definitions   $100 here!

 .( order1 ) order
  include vf-core.fth
 .( order2 ) order
  include vf-io.fth
 .( order3 ) order
  include vf-bufs.fth
 .( order4 ) order
  include vf-sys.fth
 .( order5 ) order
  include vf-bdos.fth
  \ Target definitions
 .( order6 ) order
  include vf-file1.fth
 .( order7 ) order
  include vf-end.fth

cr .( unresolved: )  .unresolved   ( ' .blk is .status )

save-target  V4TH.COM
