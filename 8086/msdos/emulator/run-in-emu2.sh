#!/bin/bash

set -e

forth="$1"
forthcmd="$2"
pathcmd=""
if [ -n "${FORTHPATH}" ]; then
  pathcmd="path ${FORTHPATH}"
fi
f_mountpoint="."
if [ -n "${F_MOUNTPOINT}" ]; then
  f_mountpoint="${F_MOUNTPOINT}"
fi

exit=""
bye=""
if [ -n "${forthcmd}" ]; then
  logname="output.log"
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
  autocmd="${forthcmd} ${bye}"
fi

# dosbox -c "mount f ${f_mountpoint}" -c "f:" "${auto_c}" "${autocmd}" $exit
emu2 "${forth}" "${autocmd}"
