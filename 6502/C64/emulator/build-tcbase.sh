#!/bin/bash
# build script for the resident part of the target compiler running
# on the C64.

set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/tcbase"

keybuf="2 drive 4 load\ninclude tc-base.fth\n\
savesystem tcbase\ndos s0:notdone\n"

DISK10=tc38q "${emulatordir}/run-in-vice.sh" \
  "c64-testbase" "${keybuf}"
