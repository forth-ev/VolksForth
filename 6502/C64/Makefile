
# This Makefile and the build scripts in the emulator/ subdir require
# the file format conversion tools from the tools/ subdir of
# https://github.com/pzembrod/cc64 to be installed.

vf_blk_d64_files = $(wildcard disks/*.d64)
vf_blk_fth_files = $(patsubst %.d64, %.fth, $(vf_blk_d64_files))

test_files = $(wildcard tests/*.f*)
test_files_petscii = $(patsubst tests/%, cbmfiles/%, $(test_files))


# Target to convert all .d64 images into .fth files for easier reading.
vf_blk_fth: $(vf_blk_fth_files)


# Convenience targets

run-devenv: emulator/devenv.T64
	emulator/run-in-vice.sh devenv

run-testbase: emulator/testbase.T64
	emulator/run-in-vice.sh testbase

test: emulator/testbase.T64 $(test_files_petscii)
	emulator/run-in-vice.sh testbase \
	  "include run-vf-tests.fth\n"

run-testbase16: emulator/testbase16.T64
	VICE=xplus4 emulator/run-in-vice.sh testbase16

test16: emulator/testbase.T64 $(test_files_petscii)
	VICE=xplus4 emulator/run-in-vice.sh testbase16 \
	  "include run-vf-tests.fth\n"


# Rules for building Forth binaries on top of the plain vanilla
# c64-volksforth83.

cbmfiles/devenv: emulator/run-in-vice.sh emulator/build-devenv.sh \
    emulator/c64-volksforth83.T64 \
     disks/vforth4_1.d64  disks/vforth4_3.d64  disks/file-words.d64
	emulator/build-devenv.sh

cbmfiles/testbase: emulator/run-in-vice.sh emulator/build-testbase.sh \
    emulator/c64-volksforth83.T64 disks/file-words.d64
	emulator/build-testbase.sh

cbmfiles/testbase16: emulator/run-in-vice.sh emulator/build-testbase.sh \
    emulator/c16-volksforth83.T64 disks/file-words.d64
	emulator/build-testbase16.sh


# Generic T64 tape image rule

emulator/%.T64: cbmfiles/%
	bin2t64 $< $@


# Generic rule for populating cbmfiles/ with PETSCII text files

cbmfiles/%.fth: tests/%.fth
	ascii2petscii $< $@

cbmfiles/%.fr: tests/%.fr
	ascii2petscii $< $@


# Generic rule for converting .d64 blk sources into .fth files.

disks/%.fth: disks/%.d64
	ufscr2file $< $@
