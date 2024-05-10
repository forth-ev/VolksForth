\ utility words for handling linked lists.
\ The first word of each list node contains the link to the next node.

: end-of-list  ( list-node -- last-node )
  BEGIN dup @ WHILE @ REPEAT ;

: reverse-list  ( list-root-var -- )
  dup >r  @ 0       ( node[0] 0 )
  BEGIN over WHILE  ( node[i] node[i-1] )
    over @          ( node[i] node[i-1] node[i+1] )
    >r over ! r>    ( node[i] node[i+1] )
  swap REPEAT       ( 0 node[n] )
  r> !  drop ;

