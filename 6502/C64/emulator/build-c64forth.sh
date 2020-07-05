#!/bin/bash

# Script for target-compiling the C64 version of VolksForth.
# It uses the tcbase binary, i.e. the resident part of the target
# compiler, adds the transient part of the target compiler, and then
# loads the C64 VolksForth sources, and saves the target.

set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/c64-vf-latest"

keybuf="include vf-c64-main.fth\n\
save-target c64-vf-latest\ndos s0:notdone\n"

DISK9=vforth4_2 DISK10=tc38q "${emulatordir}/run-in-vice.sh" \
  "tcbase" "${keybuf}"
