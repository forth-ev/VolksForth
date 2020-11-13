#!/bin/bash
set -e

emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"
cbmfilesdir="${basedir}/cbmfiles"
sdcard="${emulatordir}/sdcard.img"

mformat -i "${sdcard}" -F
for asciifile in  $(cd "${cbmfilesdir}" && ls *.fth *fr)
do
  # Convert filename to PETSCII, remove trailing CR.
  petsciifile="$(echo ${asciifile} | ascii2petscii - |tr -d '\r')"
  mcopy -i "${sdcard}" "${cbmfilesdir}/$asciifile" "::${petsciifile}"
done

autostart=""
if [ -n "$1" ]
then
  autostart="-prg ${cbmfilesdir}/${1} -run"
fi

keybuf=""
warp=""
scale=""
debug=""
if [ -n "$2" ]
then
  keybuf="${2}"
  mcopy -i "${sdcard}" "${emulatordir}/notdone" "::NOTDONE"
  warp="-warp"
else
  scale="-scale 2"
  debug="-debug"
fi

# The -keybuf flag is added in https://github.com/pzembrod/x16-emulator
x16emu \
  -keymap de \
  -sdcard "${sdcard}" \
  $autostart \
  -keybuf "$keybuf" \
  $warp \
  $scale \
  $debug \
  &

if [ -n "$keybuf" ]
then
  while mtype -i "${sdcard}" "::NOTDONE" > /dev/null
    do sleep 1
  done
  sleep 0.5

  kill9log="${basedir}/kill-9.log"
  x16emupid="$(jobs -p %1)"
  kill %1
  (sleep 20; ps -q "${x16emupid}" -f --no-headers && \
      (kill -9 "${x16emupid}" ; date)) >> "${kill9log}" 2>&1 &
fi

wait %1 || echo "x16emu returned $?"
