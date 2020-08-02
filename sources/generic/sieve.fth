\ Sieve benchmark

CR .( Loading Sieve Benchmark... ) CR
Onlyforth

: allot  ( u --)
 dup sp@ here - $180 -  u>
 abort" no room" allot ;

&8192 Constant size
Create flags   size allot
: do-prime  ( -- #primes )
 flags size 1 fill    0
 size    0 DO flags I + c@
           IF  I 2* 3+ dup I +
             BEGIN  dup size <
             WHILE  0 over flags + c!
                    over +
             REPEAT  2drop 1+
           THEN
        LOOP ;
: benchmark  
 do-prime . ." Primzahlen" ;
: .primes   size 0 DO  flags I + c@
 IF  I 2* 3+ .  THEN ?cr
 stop? IF  LEAVE  THEN  LOOP ;

CR .( Start Benchmark ) CR
benchmark CR

.primes CR

.( Benchmark finished ) CR
