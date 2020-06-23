# A virtual C-implemented VM as core for a portable VolksForth

Basic idea is a Forth VM with direct or indirect threaded code,
operating on a 64kB byte array core, implemented in C.
For compatibility with the other VolksForth flavours we would likely
let data stack and return stack live inside the core, not in separate
C data structures.

Forth primitives like nest, unnest, dup, emit, + etc. are implemnted
as C functions. Each primitive has a virtual bytecode opcode associated
with it. These virtual opcodes usually sit in the CFA of Forth words.

Yet TBD is whether it is worth enhancing the execution of these opcodes
to a virtual bytecode CPU allowing CODE words in Forth code using
these virtual opcodes, or whether each opcode is considered to have
an implicit NEXT call at its end, i.e. only predefined CODE words of
length 1 are supported.
