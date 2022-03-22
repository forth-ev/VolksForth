
  : forth-83 ;     \ last word in Dictionary

  0 ' limit >body !   $DFF6 s0 !    $E77C r0 !
  s0 @ s0 2- !   here dp !

  Host  tudp @       Target   udp !
  Host  tvoc-link @  Target   voc-link !
  Host  tnext-link @ Target   next-link !
  Host  tfile-link @ Target Forth  file-link !
  Host  T move-threads H
  save-buffers cr .( unresolved: )  .unresolved
