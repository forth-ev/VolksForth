
# This Makefile and the build scripts in the emulator/ subdir require
# the file format conversion tools from the tools/ subdir of
# https://github.com/pzembrod/cc64 to be installed.

# VERSION=volksforth83
VERSION=vf-latest

vf_blk_d64_files = $(wildcard disks/*.d64)
vf_blk_fth_files = $(patsubst %.d64, %.fth, $(vf_blk_d64_files))
vf_fth_files = $(wildcard src/vf-*.fth)
vf_fth_files_petscii = $(patsubst src/%, cbmfiles/%, $(vf_fth_files))

test_files = $(wildcard tests/*.f*)
test_files_petscii = $(patsubst tests/%, cbmfiles/%, $(test_files))


# Target to convert all .d64 images into .fth files for easier reading.
vf_blk_fth: $(vf_blk_fth_files)


clean:
	rm -f cbmfiles/*.fr cbmfiles/*.fth cbmfiles/*.log
	rm -f *.log *.result *.golden
	rm -f cbmfiles/c??-testbase
	rm -f disks/scratch.d64


# Convenience targets

test: test-c64.result test-c16.result

test64: test-c64.result

debug-64: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/vforth4_2.d64 disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh vf-c64-main.fth

run-devenv: emulator/devenv.T64
	emulator/run-in-vice.sh devenv

run-testbase: emulator/testbase.T64
	emulator/run-in-vice.sh testbase

run-testbase16: emulator/testbase16.T64
	VICE=xplus4 emulator/run-in-vice.sh testbase16


# Targetcompiler targets

cbmfiles/tcbase: emulator/c64-vf-390.T64 emulator/build-tcbase.sh \
    disks/tc38q.d64 disks/file-words.d64 cbmfiles/tc-base.fth
	emulator/build-tcbase.sh

cbmfiles/vf-full-c64: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh vf-full-c64.fth vf-full-c64

# C16 with 64 kB RAM or Plus4 - called (C16+ in the sources.
cbmfiles/vf-full-c16+: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh vf-full-c16+.fth vf-full-c16+

# C16 with 32 kB RAM - called (C16- in the sources.
cbmfiles/vf-full-c16-: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh vf-full-c16-.fth vf-full-c16-


# Core test targets

test-c64.result: emulator/vf-full-c64.T64 $(test_files_petscii) \
    emulator/run-in-vice.sh tests/evaluate-test.sh test-c64.golden
	rm -f test-c64.log test-c64.result
	cp disks/empty.d64 disks/scratch.d64
	DISK9=scratch emulator/run-in-vice.sh vf-full-c64 \
	  "include run-vf-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log test-c64.log
	tests/evaluate-test.sh test-c64

test-c16.result: emulator/vf-full-c16+.T64 $(test_files_petscii) \
    emulator/run-in-vice.sh tests/evaluate-test.sh test-c16.golden
	rm -f test-c16.log test-c16.result
	VICE=xplus4 emulator/run-in-vice.sh vf-full-c16+ \
	  "include run-vf-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log test-c16.log
	tests/evaluate-test.sh test-c16

c64_golden_parts = prelim core coreext double block report-blk
c64_golden_files = $(patsubst %, tests/golden/%.golden, \
  $(c64_golden_parts))
test-c64.golden: $(c64_golden_files)
	cat $? > $@

c16_golden_parts = prelim core
c16_golden_files = $(patsubst %, tests/golden/%.golden, \
  $(c16_golden_parts))
test-c16.golden: $(c16_golden_files)
	cat $? > $@

# Rules for building Forth binaries on top of the plain vanilla
# c64-volksforth83.

cbmfiles/devenv: emulator/run-in-vice.sh emulator/build-devenv.sh \
    emulator/c64-volksforth83.T64 \
     disks/vforth4_1.d64  disks/vforth4_3.d64  disks/file-words.d64
	emulator/build-devenv.sh

# Deprecated
# cbmfiles/c64-testbase: emulator/run-in-vice.sh emulator/build-testbase.sh \
#    emulator/c64-$(VERSION).T64 disks/file-words.d64
#	emulator/build-testbase.sh c64 $(VERSION)

# cbmfiles/c16-testbase: emulator/run-in-vice.sh emulator/build-testbase.sh \
#    emulator/c16-$(VERSION).T64 disks/file-words.d64
#	VICE=xplus4 emulator/build-testbase.sh c16 $(VERSION)


# Generic T64 tape image rule

emulator/%.T64: cbmfiles/%
	bin2t64 $< $@


# Generic rule for populating cbmfiles/ with PETSCII text files

cbmfiles/%.fth: src/%.fth
	ascii2petscii $< $@

cbmfiles/%.fth: tests/%.fth
	ascii2petscii $< $@

cbmfiles/%.fr: tests/%.fr
	ascii2petscii $< $@


# Generic rule for converting .d64 blk sources into .fth files.

disks/%.fth: disks/%.d64
	ufscr2file $< $@
