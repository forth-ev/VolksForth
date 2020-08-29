#!/bin/bash
set -e

test -n "$VICE" || VICE=x64
test -n "$DISK9" || DISK9=empty
test -n "$DISK10" || DISK10=empty
test -n "$DISK11" || DISK11=empty
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

"$VICE" \
  -virtualdev \
  +truedrive \
  -drive8type 1541 \
  -drive9type 1541 \
  -drive10type 1541 \
  -drive11type 1541 \
  -fs8 "${basedir}/cbmfiles" \
  -9 "${basedir}/disks/${DISK9}.d64" \
  -10 "${basedir}/disks/${DISK10}.d64" \
  -11 "${basedir}/disks/${DISK11}.d64" \
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

  kill9log="${basedir}/kill-9.log"
  vicepid=$(jobs -p %1)
  kill %1
  (sleep 20; ps -q "${vicepid}" -f --no-headers && \
      (kill -9 "${vicepid}" ; date)) >> "${kill9log}" 2>&1 &
fi

wait %1 || echo "x64 returned $?"
