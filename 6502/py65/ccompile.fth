
\ *** Block No. 0, Hexblock 0

\ Crosscompile Script for 6502 Target                cas 26jan06
















\ *** Block No. 1, Hexblock 1

\ loadscreen for cross-compiler                      cas 26jan06

include assemble.fb                \ load 68000 assembler
2 loadfrom as65.fb page            \ load  6502 assembler
include crostarg.fb page           \ load target compiler
include 6502f83.fb                 \ load Forth Kernel Source

save-target f6502.com              \ save new forth as f6502.com
key drop page .( Ready ) cr        \ wait for keypress
bye                                \ and exit forth






