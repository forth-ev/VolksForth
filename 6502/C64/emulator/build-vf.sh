#!/bin/bash

# Script for target-compiling the CBM versions of VolksForth.
# It uses the tcbase binary, i.e. the resident part of the target
# compiler, adds the transient part of the target compiler, then
# loads the VolksForth sources, and saves the target.

set -e

test -n "${TCBASE}" || TCBASE="tcbase"

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

target="${1}"
source="${target}.fth"
logfile="${target}.log"
nosave="${2}"

test -z "${nosave}" && rm -f "${basedir}/cbmfiles/${target}"
rm -f "${basedir}/cbmfiles/${logfile}"

keybuf="include ${source}\nsave-target ${target}\ndos s0:notdone"
test -n "${nosave}" && keybuf="include ${source}\n"

DISK10=tc38q "${emulatordir}/run-in-vice.sh" \
  "${TCBASE}" "${keybuf}"

petscii2ascii "${basedir}/cbmfiles/${logfile}" | \
  grep -F 'target compile complete' || \
  (echo "check logfile ${basedir}/cbmfiles/${logfile}" && exit 1)
