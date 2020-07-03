#!/bin/bash
set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/c64-vf-latest"

keybuf="include mk-c64forth.fth\n\
save-target c64-vf-latest\ndos s0:notdone\n"

DISK9=vforth4_2 DISK10=tc38q "${emulatordir}/run-in-vice.sh" \
  "tcbase" "${keybuf}"
