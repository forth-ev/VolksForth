#!/bin/sh
d=$(dirname $0)
mkdir -p $d/../sources/msdos
for i in $d/../msdos/*.{fb,vid,sys}; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/msdos/$b.src ..."
  $d/dumpblock.sh "$i" > $d/../sources/msdos/$b.src
  echo " Done."
done

mkdir -p $d/../sources/cpm
for i in $d/../cpm/*.FB; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/cpm/$b.src ..."
  $d/dumpblock.sh "$i" > $d/../sources/cpm/$b.src
  echo " Done."
done

mkdir -p $d/../sources/AtariST
for i in $d/../AtariST/*.FB; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/AtariST/$b.src ..."
  $d/dumpblock.sh "$i" > $d/../sources/AtariST/$b.src
  echo " Done."
done

mkdir -p $d/../sources/AtariST/GEM
for i in $d/../AtariST/GEM/*.FB; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/AtariST/GEM/$b.src ..."
  $d/dumpblock.sh "$i" > $d/../sources/AtariST/GEM/$b.src
  echo " Done."
done

