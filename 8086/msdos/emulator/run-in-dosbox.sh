#!/bin/bash

set -e
set -x

emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

forth="$1"
include_filename="$2"
include_basename="${include_filename%.*}"
forthcmd=""
exit=""
bye=""
if [ -n "${include_basename}" ]; then
  forthcmd="include ${include_filename}"
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
  autocmd="${forth} path f:\\;f:\\src;f:\\tests ${forthcmd} ${bye}"
fi

dosbox -c "mount f ${basedir}" -c "f:" "${auto_c}" "${autocmd}" $exit

if [ -n "${include_basename}" ]; then
  dos2unix -n "${doslogname}" "${logname}"
fi
