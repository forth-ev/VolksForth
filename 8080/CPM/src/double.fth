
\ *** Block No. 0, Hexblock 0

\\ Double words                                          11Nov86

Dieses File enthaelt Worte fuer 32-Bit Objekte.

Im Kern bereits enthalten sind:

   2@ 2! 2dup 2drop 2swap dnegate d+

Hier werden definiert:

   2Variable 2Constant 2over d*






\ *** Block No. 1, Hexblock 1

\ 2over  2@  2! 2Variable 2Constant                   UH 30Oct86

: 2Variable   Variable  2 allot ;
: 2Constant   Create , ,  does>  2@ ;

Code 2over   ( 32b1 32b2  -- 32b1 32b2 32b1 )  7 H lxi
   SP dad    M D mov   H dcx   M E mov  D push
   H dcx     M D mov   H dcx   M E mov  D push Next   end-code
--> \\
Code 2@   ( addr -- 32b )   H pop   H push
   H inx  H inx   M E mov   H inx   M D mov   H pop   D push
   M E mov   H inx   M D mov  D push    Next   end-code

Code 2!   ( 32b addr -- )   H pop
   D pop    E M mov   H inx   D M mov   H inx
   D pop    E M mov   H inx   D M mov   Next   end-code

\ *** Block No. 2, Hexblock 2

\ d* d-                                                  29Jun86

: d*   ( d1 d2 -- d1*d2 )
    rot  2over  rot  um*  2swap um* d+ 2swap um* d+ ;

: d-  ( d1 d2 -- d1-d2 ) dnegate d+ ;










