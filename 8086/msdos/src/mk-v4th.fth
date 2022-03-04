
  logopen output.log

  Onlyforth

  2 loadfrom META.fb
  use kernel.fb

  new v4th.com   Onlyforth Target definitions

  4 &110 thru          \ Standard 8088-System

  &112 &146 thru      \ MS-DOS interface

  : forth-83 ;     \ last word in Dictionary

  0 ' limit >body !   $DFF6 s0 !    $E77C r0 !
  s0 @ s0 2- !   here dp !

  Host  tudp @       Target   udp !
  Host  tvoc-link @  Target   voc-link !
  Host  tnext-link @ Target   next-link !
  Host  tfile-link @ Target Forth  file-link !
  Host  T move-threads H
  save-buffers cr .( unresolved: )  .unresolved



  logclose
  flush                 \ close n4th.com
  logreopen

  cr .( new kernel written as v4th.com) cr

  logclose
