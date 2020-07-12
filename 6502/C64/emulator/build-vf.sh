#!/bin/bash

# Script for target-compiling the CBM versions of VolksForth.
# It uses the tcbase binary, i.e. the resident part of the target
# compiler, adds the transient part of the target compiler, then
# loads the VolksForth sources, and saves the target.

set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

source="$1"
target="$2"

test -n "$target" && rm -f "${basedir}/cbmfiles/${target}"

keybuf="include ${source}\nsave-target ${target}\ndos s0:notdone\n"
test -z "$target" && keybuf="include ${source}\n"

DISK9=vforth4_2 DISK10=tc38q "${emulatordir}/run-in-vice.sh" \
  "tcbase" "${keybuf}"
