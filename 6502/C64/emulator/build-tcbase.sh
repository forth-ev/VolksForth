#!/bin/bash
# build script for the resident part of the target compiler running
# on the C64.

set -e

test -n "${TCVF}" || TCVF="v4th-c64-4tc"
test -n "${TCBASE}" || TCBASE="tcbase"

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/${TCBASE}"

keybuf="3 drive 20 load\n3 drive 10 load\nsave\n\
2 drive 4 load\ninclude tc-base.fth\n\
savesystem ${TCBASE}\ndos s0:notdone\n"

DISK10=tc38q DISK11=file-words "${emulatordir}/run-in-vice.sh" \
  "${TCVF}" "${keybuf}"
