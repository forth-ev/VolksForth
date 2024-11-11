\ *** Block No. 84, Hexblock 54

Target

\ .status push load                                      20Oct86

Defer .status   ' noop Is .status

| Create: pull  r> r> ! ;

: push   ( addr -- )   r> swap dup >r @ >r pull >r >r ;
                       restrict

: rdepth   ( -- +n)           r0 @ rp@ 2+   - 2/ ;
: depth    ( -- +n)           sp@ s0 @ swap - 2/ ;






\ *** Block No. 86, Hexblock 56

\ quit (quit abort                                    UH 25Jan88

: (prompt ( -- )
    state @ IF cr ." ] " ELSE ."  ok" cr THEN .status ;

Defer prompt    ' (prompt Is prompt

: (quit      clear-tibstash  BEGIN prompt query interpret REPEAT ;

Defer 'quit     ' (quit Is 'quit
: quit   r0 @ rp!  level off  [compile] [ 'quit ;

: standardi/o     [ output ] Literal output 4 cmove ;

Defer 'abort   ' noop Is 'abort
: abort      end-trace clearstack 'abort standardi/o quit ;

\ *** Block No. 87, Hexblock 57

\ (error Abort" Error"                         20Oct86   18Nov87

Variable scr    1 scr !       Variable r#    0 r# !

: (error ( string -- )     standardi/o space here .name
    count type space ?cr
    blk @ ?dup IF scr ! >in @ r# ! THEN quit ;
' (error errorhandler !

: (abort"     "lit swap IF >r clearstack r>
              errorhandler perform exit THEN drop ; restrict

| : (err"      "lit swap IF errorhandler perform exit THEN
               drop ; restrict
: Abort"       compile (abort" ,"  align  ; immediate restrict
: Error"       compile (err"   ,"  align  ; immediate restrict

\ *** Block No. 88, Hexblock 58

\ -trailing                                    30Jun86   18Nov87

Code -trailing   ( addr n1 -- addr n2 )
   D pop   H pop   H push
   D dad   xchg   D dcx
Label -trail   H A mov   L ora    hpush jz
   D ldax   BL cpi   hpush jnz
   H dcx   D dcx   -trail jmp end-code

\ \\
\ : -trailing ( addr n1 -- addr n2)
\     2dup bounds ?DO  2dup + 1- c@ bl - IF LEAVE THEN  1-  LOOP ;





\ *** Block No. 89, Hexblock 59

\ space spaces                                           30Jun86

$20 Constant bl

: space                      bl emit ;
: spaces   ( u --)           0 ?DO space LOOP ;











\ *** Block No. 90, Hexblock 5a

\ hold <# #> sign # #s                                   17Oct86

| : hld   ( -- addr)            pad 2- ;

: hold    ( char -- )           -1 hld +! hld @ c! ;

: <#                            hld hld ! ;

: #>      ( 32b -- addr +n )    2drop hld @ hld over - ;

: sign    ( n -- )              0< IF Ascii - hold THEN ;

: #       ( +d1 -- +d2)         base @ ud/mod rot 9 over <
   IF [ Ascii A Ascii 9 - 1- ] Literal + THEN Ascii 0 + hold ;

: #s      ( +d -- 0 0 )         BEGIN # 2dup d0= UNTIL ;

\ *** Block No. 91, Hexblock 5b

\ print numbers                                          24Dec83

: d.r      -rot under dabs <# #s rot sign #>
           rot over max over - spaces type ;

: .r       swap extend rot d.r ;

: u.r      0 swap d.r ;

: d.       0 d.r space ;

: .        extend d. ;

: u.       0 d. ;



\ *** Block No. 92, Hexblock 5c

\ .s list c/l l/s                                        05Oct87

: .s    sp@ s0 @ over - $20 umin bounds ?DO I @ u. 2 +LOOP ;

$40 Constant c/l        \ Screen line length
$10 Constant l/s        \ lines per screen






\ *** Block No. 93, Hexblock 5d

\ multitasker primitives                                 20Nov87

Code end-trace    \ patch Next to its original state
   $0A A mvi   ( IP ldax )          >next sta
   $6F03 H lxi ( IP inx   A L mov ) >next 1+ shld  Next end-code

Code pause   >next here 2- !  end-code

: lock ( addr -- )  dup @  up@  = IF  drop exit  THEN
   BEGIN  dup  @ WHILE  pause  REPEAT  up@  swap  ! ;

: unlock ( addr -- )   dup lock off ;

Label wake   H pop   H dcx   UP shld
   6 D lxi   D dad   M A mov   H inx   M H mov  A L mov   sphl
   H pop   RP shld   IP pop   Next   end-code

\ file related definitions moved here from vf-bufs.fth

User isfile          0 isfile !   \ addr of file control block
Variable fromfile    0 fromfile !

Code isfile@ ( -- addr )  user' isfile  D lxi
   UP lhld   D dad   fetch jmp   end-code

$FF00 Constant limit

Defer save-buffers  ' noop IS save-buffers
Defer init-buffers  ' noop IS init-buffers

$400 Constant b/blk
