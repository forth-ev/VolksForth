\ This file contains commands to create an Atari Portfolio adapted
\ version of volksFORTH from KERNEL.COM

\ The new system will be saved as "v4thpofo.com".

  Onlyforth warning off
  &08 ' c/col >body !   &40 ' c/row >body !

  cr .( creating Atari Portfolio System... ) cr
  include src\86asm.fs
  include src\extend2.fs
  cr .( Atari Portfolio Text-Screen I/O... ) cr
  include pofo\pofovid.fs
  cr .( Atari Portfolio specific extensions ... ) cr
  include pofo\pofo.fs 
  cr .( Atari Portfolio status line config... ) cr
  ' pofologo is 'cold
  savesystem v4thpofo.com
  cr .( new kernel written as v4thpofo.com) cr







