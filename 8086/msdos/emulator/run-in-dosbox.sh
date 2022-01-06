#!/bin/bash

set -e

emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

forth="$1"
srcbasename="$2"
forthcmd=""
if [ -n "${srcbasename}" ]; then
  forthcmd="include ${srcbasename}.fb"
  logname="${srcbasename}.log"
  doslogname="$(echo ${logname}|tr '[:lower:]' '[:upper:]')"
  rm -f "${logname}" "${doslogname}"
fi

exit=""
bye=""
if [ -z "${KEEPEMU}" ]; then
  exit="-c exit"
  bye="bye"
fi

auto_c=""
autocmd=""
if [ -n "${forth}" ]; then
  auto_c="-c"
  autocmd="${forth} path f:\\;f:\\tests ${forthcmd} ${bye}"
fi

dosbox -c "mount f ${basedir}" -c "f:" "${auto_c}" "${autocmd}" $exit

if [ -n "${srcbasename}" ]; then
  dos2unix -n "${doslogname}" "${logname}"
fi
