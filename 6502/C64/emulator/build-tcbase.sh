#!/bin/bash
set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/tcbase"

keybuf="2 drive 4 load\ninclude mk-tcbase.fth\n\
savesystem tcbase\ndos s0:notdone\n"

DISK9=vforth4_2 DISK10=tc38q "${emulatordir}/run-in-vice.sh" \
  "c64-testbase" "${keybuf}"
