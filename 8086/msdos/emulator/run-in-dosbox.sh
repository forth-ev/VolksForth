#!/bin/bash

set -e

forth="$1"
forthcmd="$2"
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
pathcmd=""
if [ -n "${forth}" ]; then
  auto_c="-c"
  if [ -n "${FORTHPATH}" ]; then
    pathcmd="path ${FORTHPATH}"
  fi
  autocmd="${forth} ${pathcmd} ${forthcmd} ${bye}"
fi

dosbox -c "mount f ." -c "f:" "${auto_c}" "${autocmd}" $exit
