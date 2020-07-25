#!/bin/sh
d=$(dirname $0)
mkdir -p $d/../sources/msdos
for i in $d/../msdos/*.{fb,vid,sys}; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/msdos/"${b%.*}.fth" ..."
  $d/dumpblock.sh "$i" > $d/../sources/msdos/${b%.*}.fth
  echo " Done."
done

mkdir -p $d/../sources/cpm
for i in $d/../8080/CPM/*.fb; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/cpm/${b%.*}.fth ..."
  $d/dumpblock.sh "$i" > $d/../sources/cpm/${b%.*}.fth
  echo " Done."
done

mkdir -p $d/../sources/AtariST
for i in $d/../AtariST/*.FB; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/AtariST/${b%.*}.fth ..."
  $d/dumpblock.sh "$i" > $d/../sources/AtariST/${b%.*}.fth
  echo " Done."
done

mkdir -p $d/../sources/AtariST/GEM
for i in $d/../AtariST/GEM/*.FB; do
  b=$(basename $i)
  /bin/echo -n "write $d/../sources/AtariST/GEM/${b%.*}.fth ..."
  $d/dumpblock.sh "$i" > $d/../sources/AtariST/GEM/${b%.*}.fth
  echo " Done."
done

mkdir -p $d/../sources/Apple1
for i in $d/../6502/Apple1/source/*.fb; do
  b=$(basename $i)
  /bin/echo -n "write $d/../source/Apple1/${b%.*}.fth ..."
  $d/dumpblock.sh "$i" > $d/../sources/Apple1/${b%.*}.fth
  echo " Done."
done
