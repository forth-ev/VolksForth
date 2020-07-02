#!/bin/bash

testsdir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
basedir="$(realpath --relative-to="$PWD" "${testsdir}/..")"

testname="$1"

diff "${testsdir}/${testname}.golden" "${basedir}/${testname}.log" > tmp.result
exitcode=$?
test $exitcode -eq 0 \
  && echo "PASS: ${testname}" >> tmp.result \
  || echo "FAIL: ${testname}" >> tmp.result
cat tmp.result
mv tmp.result "${basedir}/${testname}.result"
exit $exitcode
