#!/bin/bash
# build script for the resident part of the target compiler running
# on the C64.

set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/tcbase"

keybuf="3 drive 20 load\n3 drive 10 load\nsave\n\
2 drive 4 load\ninclude tc-base.fth\n\
savesystem tcbase\ndos s0:notdone"

DISK10=tc38q DISK11=file-words "${emulatordir}/run-in-vice.sh" \
  "v4th-c64-4tc" "${keybuf}"
