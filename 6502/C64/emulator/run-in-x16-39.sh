#!/bin/bash

set -e

emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"

export PATH="${HOME}/x16-r39:${PATH}"
echo "PATH = ${PATH}"
"${emulatordir}/run-in-x16emu.sh" "$@"
