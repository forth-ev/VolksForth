#!/bin/bash
# build script for the resident part of the target compiler running
# on the C64.

set -e

test -n "${TCVF}" || TCVF="v4th-c64-4tc"
test -n "${TCBASE}" || TCBASE="tcbase"

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/${TCBASE}"

keybuf="include savesys-noed.fth\ninclude dir-dos-cat.fth\nsave\n\
include tc-relocate.fth\ninclude tc-base.fth\n\
savesystem ${TCBASE}\ndos s0:notdone"

DISK10=tc38q "${emulatordir}/run-in-vice.sh" \
  "${TCVF}" "${keybuf}"
