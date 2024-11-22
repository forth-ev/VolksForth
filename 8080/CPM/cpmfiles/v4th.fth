
Onlyforth

: .pagestatus  ( n -- )
  cr ." page " .
  ." here "  here u.
  ." there " there u.
  ." displaced there " there displace @ + u.
  ." heap "  heap u.  cr
;

    $8000 displace !
Target definitions   $100 here!

  include vf-core.fth
  include vf-io.fth
  include vf-sys.fth
  include vf-bdos.fth
  include vf-file.fth
  include vf-end.fth

cr .( unresolved: )  .unresolved   ( ' .blk is .status )

save-target  V4TH.COM
