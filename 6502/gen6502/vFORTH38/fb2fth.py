#!/usr/bin/python3

import sys

def readToString(inFile):
  blockNo = 0
  result = []
  while(True):
    block = inFile.read(1024)
    if len(block) == 0:
      break
    result.append("\n\\ *** Block No. %d, Hexblock %x\n"
        % (blockNo, blockNo));
    offset = 0
    while(offset < len(block)):
      # sys.stderr.write("block %d offset %d\n" % (blockNo, offset))
      line = block[offset:offset+64].decode(encoding="cp437")
      result.append(line.rstrip())
      offset += 64
    blockNo += 1
  return result

inFileName, outFileName = sys.argv[1], sys.argv[2]
inFile = open(inFileName, "rb")
result = readToString(inFile)
result.append('')
outFile = open(outFileName, "w")
outFile.write("\n".join(result))
