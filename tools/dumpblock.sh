#!/bin/sh
# small tool to dump all screens of a block-file
# on screen. Used to create source files for fossil 
# checkin. Depends on GNU-Forth (gforth)

gforth -e ": bdump 0 do i list loop ; use $1 get-block-fid file-size drop drop 1024 / bdump bye"
