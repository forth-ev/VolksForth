#!/bin/bash
set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

rm -f "${basedir}/cbmfiles/devenv"

# load editor from vforth4_3.d64 block 19 in drive 10.
# load savesystem from vforth4_1.d64 block 26 in drive 9.
# load include and dos from file-words.d64 block 10 in drive 11.
# savesystem and then scratch file notdone to exit emulator.
keybuf="2 drive 19 load\n47 load\n1 drive 26 load\n3 drive 10 load\n\
savesystem devenv\ndos s0:notdone\n"

DISK9=vforth4_1 DISK10=vforth4_3 DISK11=file-words \
  "${emulatordir}/run-in-vice.sh" "c64-volksforth83" "${keybuf}"
