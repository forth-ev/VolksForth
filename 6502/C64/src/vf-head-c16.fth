\ The head of C16 VolkForth

\ Initial part of load screen

Onlyforth

$1001 dup  displace !
Target definitions   here!


\ *** Block No. 16, Hexblock 10
10 fthpage

\ FORTH Preamble and ID       clv06aug87

$D c, 10 c, $A c, 00 c, 9E c, 28 c, 34 c, 31 c,
31 c, 32 c, 29 c, 00 c, 00 c, 00 c, 00 c, \ SYS(4112)

Assembler
  nop  0 jmp  here 2- >label >cold
  nop  0 jmp  here 2- >label >restart

here dup origin!
\ Here are coldstart- and Uservariables
\
0 jmp  0 jsr  here 2- >label >wake
 end-code
$100 allot

Create logo
 (C16+ ," volksFORTH-83 3.9.5-C16+ " )
 (C16- ," volksFORTH-83 3.9.5-C16- " )
