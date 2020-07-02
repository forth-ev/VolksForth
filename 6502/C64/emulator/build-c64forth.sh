#!/bin/bash
set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/vf83-c64"

keybuf="include mk-c64forth.fth\n\
save-target vf83-c64\ndos s0:notdone\n"

DISK9=vforth4_2 DISK10=tc38q "${emulatordir}/run-in-vice.sh" \
  "tcbase" "${keybuf}"
