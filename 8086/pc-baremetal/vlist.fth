: .flags ( cntf --  )
     dup $80 and if [char] R emit else space then
     dup $40 and if [char] I emit else space then
         $20 and if [char] N emit else space then ;

: vlist  ( -- )  base @ cr
     ." Word" &25 spaces ." Flags CFA  Length" cr
     [compile] char capital >r   context @
     BEGIN  @ dup  stop? 0=  and
     WHILE  ?cr dup 2+  r@ bl = over 1+ c@ r@ = or
         IF  dup .name
            dup c@ $F and &20 swap - spaces ( Name )
            dup c@     .flags space       ( Count Field )
            dup name>  hex u. space       ( CFA )
            2- 2- @ decimal 3 u.r space   ( Block )
            cr
         ELSE  drop  THEN
     REPEAT drop rdrop base ! ;
