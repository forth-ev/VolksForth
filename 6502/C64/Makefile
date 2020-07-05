
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
	rm -f cbmfiles/*.fr cbmfiles/*.fth cbmfiles/*.log *.log *.result
	rm -f cbmfiles/c??-testbase


# Convenience targets

test: test-c64.result test-c16.result

test64: test-c64.result

# Temporary bincmp target while the old and the new binaries are still
# expected to be binary identical.
bincmp:
	cmp cbmfiles/c64-vf-latest cbmfiles/c64-volksforth83
	cmp cbmfiles/c16p-vf-latest cbmfiles/c16-volksforth83

run-devenv: emulator/devenv.T64
	emulator/run-in-vice.sh devenv

run-testbase: emulator/testbase.T64
	emulator/run-in-vice.sh testbase

run-testbase16: emulator/testbase16.T64
	VICE=xplus4 emulator/run-in-vice.sh testbase16


# Targetcompiler targets

cbmfiles/tcbase: emulator/c64-testbase.T64 emulator/build-tcbase.sh \
    disks/tc38q.d64 cbmfiles/tc-base.fth
	emulator/build-tcbase.sh

cbmfiles/c64-vf-latest: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/vforth4_2.d64 disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh vf-c64-main.fth c64-vf-latest

# C16 with 32 kB RAM - called (C16- in the sources.
cbmfiles/c16-vf-latest: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/vforth4_2.d64 disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh vf-c16-main.fth c16-vf-latest

# C16 with 64 kB RAM or Plus4 - called (C16+ in the sources.
cbmfiles/c16p-vf-latest: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/vforth4_2.d64 disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh vf-c16p-main.fth c16p-vf-latest


# Core test targets

test-c64.result: emulator/c64-testbase.T64 $(test_files_petscii) \
    emulator/run-in-vice.sh tests/evaluate-test.sh tests/test-c64.golden
	rm -f test-c64.log test-c64.result
	emulator/run-in-vice.sh c64-testbase \
	  "include run-vf-tests.fth\n1234567890\ndos s0:notdone\n"
	petscii2ascii cbmfiles/test.log test-c64.log
	tests/evaluate-test.sh test-c64

test-c16.result: emulator/c16-testbase.T64 $(test_files_petscii) \
    emulator/run-in-vice.sh tests/evaluate-test.sh tests/test-c16.golden
	rm -f test-c16.log test-c16.result
	VICE=xplus4 emulator/run-in-vice.sh c16-testbase \
	  "include run-vf-tests.fth\n1234567890\ndos s0:notdone\n"
	petscii2ascii cbmfiles/test.log test-c16.log
	tests/evaluate-test.sh test-c16


# Rules for building Forth binaries on top of the plain vanilla
# c64-volksforth83.

cbmfiles/devenv: emulator/run-in-vice.sh emulator/build-devenv.sh \
    emulator/c64-volksforth83.T64 \
     disks/vforth4_1.d64  disks/vforth4_3.d64  disks/file-words.d64
	emulator/build-devenv.sh

cbmfiles/c64-testbase: emulator/run-in-vice.sh emulator/build-testbase.sh \
    emulator/c64-$(VERSION).T64 disks/file-words.d64
	emulator/build-testbase.sh c64 $(VERSION)

cbmfiles/c16-testbase: emulator/run-in-vice.sh emulator/build-testbase.sh \
    emulator/c16-$(VERSION).T64 disks/file-words.d64
	VICE=xplus4 emulator/build-testbase.sh c16 $(VERSION)


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
