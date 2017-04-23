#!/bin/sh
d=$(dirname $0)
mkdir -p $d/../sources/msdos
for i in $d/../msdos/*.{fb,vid,sys}; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/msdos/$b.src ..."
  $d/dumpblock.sh "$i" > $d/../sources/msdos/$b.src 
  echo " Done."
done

