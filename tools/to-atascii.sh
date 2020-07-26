#!/bin/sh
# converts from ASCII to ATASCII (Atari 400/800/XL/XE)

tr '\12\11' '\233\177' < ${1} > ${2}
