
# This Makefile and the build scripts in the emulator/ subdir require
# the file format conversion tools from the tools/ subdir of
# https://github.com/pzembrod/cc64 to be installed.

vf_blk_d64_files = $(wildcard disks/*.d64)
vf_blk_fth_files = $(patsubst %.d64, %.fth, $(vf_blk_d64_files))
vf_fth_files = $(wildcard src/vf-*.fth)
vf_fth_files_petscii = $(patsubst src/%, cbmfiles/%, $(vf_fth_files))
vf_flavours = full-c64 full-c16+ full-c16- lite-c64 lite-c16+ lite-c16-
vf_binaries = $(patsubst %, cbmfiles/vf-%, $(vf_flavours))

test_files = $(wildcard tests/*.f*)
test_files_petscii = $(patsubst tests/%, cbmfiles/%, $(test_files))
test_logs = $(patsubst %, test-%.log, $(vf_flavours))
test_resuls = $(patsubst %, test-%.result, $(vf_flavours))

# Target to convert all .d64 images into .fth files for easier reading,
# and to update all PETSCII files in cbmfiles to the latest state of
# the corresponding ASCII files in src.
update: $(vf_blk_fth_files) $(vf_fth_files_petscii)


clean:
	rm -f cbmfiles/*.fr cbmfiles/*.fth cbmfiles/*.log
	rm -f *.log *.result *.golden
	rm -f cbmfiles/c??-testbase
	rm -f disks/scratch.d64


# Convenience targets

binaries: $(vf_binaries)

test: $(test_resuls)

test64: full64 lite64

full64: test-full-c64.result

lite64: test-lite-c64.result

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

$(vf_binaries):  emulator/tcbase.T64 emulator/build-vf.sh \
    disks/tc38q.d64 $(vf_fth_files_petscii)

cbmfiles/vf-full-c64:
	emulator/build-vf.sh vf-full-c64.fth vf-full-c64

cbmfiles/vf-full-c16+:
	emulator/build-vf.sh vf-full-c16+.fth vf-full-c16+

cbmfiles/vf-full-c16-:
	emulator/build-vf.sh vf-full-c16-.fth vf-full-c16-

cbmfiles/vf-lite-c64:
	emulator/build-vf.sh vf-lite-c64.fth vf-lite-c64

cbmfiles/vf-lite-c16+:
	emulator/build-vf.sh vf-lite-c16+.fth vf-lite-c16+

cbmfiles/vf-lite-c16-:
	emulator/build-vf.sh vf-lite-c16-.fth vf-lite-c16-


# Core test targets

$(test_logs): $(test_files_petscii) emulator/run-in-vice.sh

test-full-c64.log: emulator/vf-full-c64.T64 disks/scratch.d64
	DISK9=scratch emulator/run-in-vice.sh vf-full-c64 \
	  "include run-full-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-lite-c64.log: emulator/vf-lite-c64.T64
	emulator/run-in-vice.sh vf-lite-c64 \
	  "include run-lite-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-full-c16+.log: emulator/vf-full-c16+.T64 disks/scratch.d64
	VICE=xplus4 emulator/run-in-vice.sh vf-full-c16+ \
	  "include run-full-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-lite-c16+.log: emulator/vf-lite-c16+.T64
	VICE=xplus4 emulator/run-in-vice.sh vf-lite-c16+ \
	  "include run-lite-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-full-c16-.log: emulator/vf-full-c16-.T64
	VICE=xplus4 emulator/run-in-vice.sh vf-full-c16- \
	  "include run-min-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-lite-c16-.log: emulator/vf-lite-c16-.T64
	VICE=xplus4 emulator/run-in-vice.sh vf-lite-c16- \
	  "include run-min-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-%.result: test-%.log test-%.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)

disks/scratch.d64: disks/empty.d64
	cp $< $@


test-full-c64.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double block report-blk)
	cat $? > $@

test-lite-c64.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double report-noblk)
	cat $? > $@

test-full-c16+.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double block report-blk)
	cat $? > $@

test-lite-c16+.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double report-noblk)
	cat $? > $@

test-full-c16-.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core)
	cat $? > $@

test-lite-c16-.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core)
	cat $? > $@

# Rules for building Forth binaries on top of the plain vanilla
# c64-volksforth83.

cbmfiles/devenv: emulator/run-in-vice.sh emulator/build-devenv.sh \
    emulator/c64-volksforth83.T64 \
     disks/vforth4_1.d64  disks/vforth4_3.d64  disks/file-words.d64
	emulator/build-devenv.sh


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
