#!/bin/bash
set -e

emulatordir="$(dirname "${BASH_SOURCE[0]}")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

cd "${basedir}"

make emulator/c64-volksforth83.T64

rm -f cbmfiles/devenv

keybuf="2 drive 19 load\n1 drive 26 load\nsavesystem devenv\n"

"${emulatordir}/build-in-vice.sh" "c64-volksforth83" "${keybuf}"

make emulator/devenv.T64
