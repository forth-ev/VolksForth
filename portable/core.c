
// This is only a sketch of an idea yet.


byte core[0x10000];

uint16 ip;
uint16 w;

void run() {
  while (true) {
    next();
  }
}

void next() {
  w = *((uint16*) core + ip);
  ip += 2;
  byte opcode = core[w];
  // on second thought, the above looks more like direct threaded,
  // which is maybe not what we want in a C core for VolksForth.
  // Or is it?
  process(opcode);
}

void process(byte opcode) {
  if (opcode & 0xc0) {
    // handle special opcode
    // This is assuming we won’t have more than 64 C primitives,
    // and might have use for a few
    // opcodes with inlined parameters. Not sure about this, though.
  } else {
    jumptable[opcode]();
  }
}

void jumptable[0x40]() = {
  nest, unnest, dup, drop, plus, emit
};

void nest() {
  rs[rp++] = ip;
  ip = w + 1;  // + 1 assuming direct threaded code where at core[w]
  // is sitting the 1 byte virtual opcode for "next".
}

void unnest() {
  ip = rs[--rp];
}

void dup() {
  uint16 = s[sp];
  s[++sp] = uint16;
}

void emit() {
  putc(s[sp--]);
}

void plus() {
  a = s[sp--];
  s[sp] += a;
}
