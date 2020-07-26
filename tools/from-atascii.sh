#!/bin/sh
# converts from ATASCII (Atari 400/800/XL/XE) to ASCII

tr '\233\177' '\12\11' < ${1} > ${2}
