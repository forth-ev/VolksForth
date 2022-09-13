#!/bin/bash
set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

platform="$1"
version="$2"
source="${platform}-${version}"
target="${platform}-testbase"

rm -f "${basedir}/cbmfiles/${target}"

# load savesystem w/o editor from file-words.d64 block 20.
# load include and dos from file-words.d64 block 10.
# savesystem and then scratch file notdone to exit emulator.
keybuf="3 drive 20 load\n3 drive 10 load\n\
savesystem ${target}\ndos s0:notdone"

DISK11=file-words "${emulatordir}/run-in-vice.sh" \
  "${source}" "${keybuf}"
