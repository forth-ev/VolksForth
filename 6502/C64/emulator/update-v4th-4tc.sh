#!/bin/bash
set -e

# Script to update the base binary on which the target compiler
# is run.

# The updating of v4th-c64-4tc from the newly built binaries
# is intentionally not automatically via Makefile, to ensure a
# certain stability for the target compiler.

emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"
cbmfilesdir="${basedir}/cbmfiles"

cp "${cbmfilesdir}/v4thblk-c64" "${cbmfilesdir}/v4th-c64-4tc"
