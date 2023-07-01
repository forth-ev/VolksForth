#!/usr/bin/python3

import sys

inFileName, outFileName = sys.argv[1], sys.argv[2]
inFile = open(inFileName, "rb")
source = inFile.read()
destination = bytearray()
for b in source:
  if b == 26:
    break
  destination.append(b)
# result.append('')
outFile = open(outFileName, "wb")
outFile.write(destination)
