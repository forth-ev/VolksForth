
include log2file.fb
logopen output.log

.( hello, world) cr
: test-hello ." hello, world, from test-hello" cr ;
test-hello

logclose
