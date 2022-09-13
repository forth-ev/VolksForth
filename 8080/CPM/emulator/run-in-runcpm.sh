#!/bin/bash

set -e

emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"
cpmfilesdir="${basedir}/cpmfiles"
runcpmdir="${basedir}/runcpm"
runcpm_a0="${runcpmdir}/A/0"
toolsdir="${basedir}/../../tools"

logfile="${runcpmdir}/output.log"
scriptfile="${runcpmdir}/input.script"
rm -f "${logfile}"

test -d "${runcpm_a0}" || mkdir -p "${runcpm_a0}"

for pathname in ${cpmfilesdir}/*
do
  echo $pathname
  filename="$(realpath --relative-to="${cpmfilesdir}" "${pathname}")"
  echo $filename
  uppercase_filename="$("${toolsdir}/echo-toupper.py" "${filename}")"
  echo $uppercase_filename
  cp "${pathname}" "${runcpm_a0}/${uppercase_filename}"
done

if [[ -f "${scriptfile}" ]]; then
  "${runcpmdir}/RunCPM" -i "${scriptfile}" -o "${logfile}"
else
  "${runcpmdir}/RunCPM" -o "${logfile}"
fi