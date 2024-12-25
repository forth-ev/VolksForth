
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

  include vf-core.fs
  include vf-io.fs
  include vf-sys.fs
  include vf-bdos.fs
  include vf-file.fs
  include vf-end.fs

cr .( unresolved: )  .unresolved   ( ' .blk is .status )

save-target  V4TH.COM
