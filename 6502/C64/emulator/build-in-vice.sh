#!/bin/bash
set -e

emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"

autostart=""
if [ -n "$1" ]
then
  autostart="-autostart ${emulatordir}/${1}.T64"
fi

keybuf=""
warp=""
if [ -n "$2" ]
then
  keybuf="${2}" # dos s0:notdone\n"
  # The following could also just be a cp.
  ascii2petscii "${emulatordir}/notdone" "${basedir}/cbmfiles/notdone"
  warp="-warp"
fi

x64 \
  -virtualdev \
  +truedrive \
  -drive8type 1541 \
  -drive9type 1541 \
  -drive10type 1541 \
  -drive11type 1541 \
  -fs8 "${basedir}/cbmfiles" \
  -9 "${basedir}/disks/vforth4_1.d64" \
  -10 "${basedir}/disks/vforth4_3.d64" \
  -11 "${basedir}/disks/vforth4_4.d64" \
  -symkeymap "${emulatordir}/x11_sym_vf_de.vkm" \
  -keymap 2 \
  $autostart \
  -keybuf "$keybuf" \
  $warp \
  &


if [ -n "$keybuf" ]
then
  while [ -f "${basedir}/cbmfiles/notdone" ]
    do sleep 1
  done
  sleep 0.5

  kill %1
fi

wait %1 || echo "x64 returned $?"
