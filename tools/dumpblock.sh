#!/bin/sh
# small tool to dump all screens of a block-file
# on screen. Used to create source files for git/fossil 
# checkin. Depends on GNU-Forth (gforth)

gforth -e "use ${1} require dumpblock.fth"
