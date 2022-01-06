#!/bin/bash

set -e

emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

forth="$1"
include_basename="$2"
forthcmd=""
exit=""
bye=""
if [ -n "${include_basename}" ]; then
  forthcmd="include ${include_basename}.fb"
  logname="${include_basename}.log"
  doslogname="$(echo ${logname}|tr '[:lower:]' '[:upper:]')"
  rm -f "${logname}" "${doslogname}"
  if [ -z "${KEEPEMU}" ]; then
    exit="-c exit"
    bye="bye"
  fi
fi

auto_c=""
autocmd=""
if [ -n "${forth}" ]; then
  auto_c="-c"
  autocmd="${forth} path f:\\;f:\\tests ${forthcmd} ${bye}"
fi

dosbox -c "mount f ${basedir}" -c "f:" "${auto_c}" "${autocmd}" $exit

if [ -n "${include_basename}" ]; then
  dos2unix -n "${doslogname}" "${logname}"
fi