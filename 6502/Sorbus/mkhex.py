#!/usr/bin/env python

import sys

# first command line argument can be load address in hex
if len(sys.argv) > 1:
    addr = int(sys.argv[1], 16)
else:
    addr = 0x300

while True:
    bytes = sys.stdin.read(16)
    if bytes == '':
        break
    print "%04x: %s" % (addr, " ".join("%02x" % ord(b) for b in bytes))
    addr += len(bytes)
