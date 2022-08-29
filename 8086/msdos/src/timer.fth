
\ *** Block No. 0, Hexblock 0

\                                                 ks 22 dez 87

The timer utilizes the memory cell at $46C that is incremented
by an interrupt. A couple of words allow this timer to be
used for time delays.

time-of-day and date are accessed via MS-DOS calls.










\ *** Block No. 1, Hexblock 1

\ BIMomat BIOS Timer                              ks 03 apr 88
  Onlyforth \needs Assembler    2 loadfrom asm.scr

  $46C >label Counter

\ 1193180 / 65536 = 18,206 Hz

  1 2 +thru  .( Timer geladen) cr









\ *** Block No. 2, Hexblock 2

\ BIMomat BIOS Timer                              ks 22 dez 87

  Code ticks ( -- n )   D push   D: C mov   A A xor
     A D: mov   Counter #) D mov   C D: mov   Next   end-code

  : timeout? ( ticks -- ticks f )  pause dup ticks - 0< ;

  : till     ( n -- )        BEGIN  timeout? UNTIL  drop ;

  : time     ( n -- time )   ticks + ;

  : wait     ( n -- )        time till ;

  : seconds  ( sec -- ticks )  &18206 &1000 */ ;

  : minutes  ( min -- ticks )  &1092 * ;

\ *** Block No. 3, Hexblock 3

\ MS-DOS time and date                            ks 22 dez 87

  Code date@  ( -- dd mm yy )
     D push   $2A # A+ mov   $21 int   A A xor   D+ A- xchg
     D push   A push   C D mov   &1900 # D sub   Next
  end-code

  Code time@  ( -- ss mm hh )
     D push   $2C # A+ mov   $21 int   D+ D- mov   0 # D+ mov
     D push   D+ D- mov   C+ D- xchg   C push   Next
  end-code






\ *** Block No. 4, Hexblock 4

















