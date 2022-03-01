
  logopen output.log

  Onlyforth

  2 loadfrom META.fb
  use kernel.fb

  new v4th.com   Onlyforth Target definitions

  4 &111 thru          \ Standard 8088-System

  logclose
  flush                 \ close n4th.com
  logreopen

  cr .( new kernel written as v4th.com) cr

  logclose
