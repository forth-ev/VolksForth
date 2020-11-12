
# This Makefile and the build scripts in the emulator/ subdir require
# the file format conversion tools from the tools/ subdir of
# https://github.com/pzembrod/cc64 to be installed.

vf_blk_d64_files = $(wildcard disks/*.d64)
vf_blk_fth_files = $(patsubst %.d64, %.fth, $(vf_blk_d64_files))
vf_fth_files = $(wildcard src/vf-*.fth src/v4th-*.fth)
vf_fth_files_petscii = $(patsubst src/%, cbmfiles/%, $(vf_fth_files))
vf_flavours = \
    vf-full-c64 vf-full-c16+ vf-full-c16- \
    v4th-c64 v4th-c16+ v4th-c16- v4th-x16
vf_binaries = $(patsubst %, cbmfiles/%, $(vf_flavours))

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
	rm -f disks/scratch.d64 emulator/sdcard.img


# Convenience targets

binaries: $(vf_binaries)

test: $(test_resuls)

test64: full64 lite64

full64: test-vf-full-c64.result

lite64: test-v4th-c64.result

debug-64: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/vforth4_2.d64 disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh v4th-c64 nosave

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

$(vf_binaries):  cbmfiles/logtofile.fth

cbmfiles/vf-full-c64:
	emulator/build-vf.sh vf-full-c64

cbmfiles/vf-full-c16+:
	emulator/build-vf.sh vf-full-c16+

cbmfiles/vf-full-c16-:
	emulator/build-vf.sh vf-full-c16-

cbmfiles/v4th-c64:
	emulator/build-vf.sh v4th-c64

cbmfiles/v4th-c16+:
	emulator/build-vf.sh v4th-c16+

cbmfiles/v4th-c16-:
	emulator/build-vf.sh v4th-c16-

cbmfiles/v4th-x16:
	emulator/build-vf.sh v4th-x16


# Core test targets

$(test_logs): $(test_files_petscii) emulator/run-in-vice.sh

test-vf-full-c64.log: emulator/vf-full-c64.T64 disks/scratch.d64
	rm -f cbmfiles/test.log
	DISK9=scratch emulator/run-in-vice.sh vf-full-c64 \
	  "include run-full-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-v4th-c64.log: emulator/v4th-c64.T64
	rm -f cbmfiles/test.log
	emulator/run-in-vice.sh v4th-c64 \
	  "include run-lite-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-vf-full-c16+.log: emulator/vf-full-c16+.T64 disks/scratch.d64
	rm -f cbmfiles/test.log
	VICE=xplus4 emulator/run-in-vice.sh vf-full-c16+ \
	  "include run-full-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-v4th-c16+.log: emulator/v4th-c16+.T64
	rm -f cbmfiles/test.log
	VICE=xplus4 emulator/run-in-vice.sh v4th-c16+ \
	  "include run-lite-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-vf-full-c16-.log: emulator/vf-full-c16-.T64
	rm -f cbmfiles/test.log
	VICE=xplus4 emulator/run-in-vice.sh vf-full-c16- \
	  "include run-min-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-v4th-c16-.log: emulator/v4th-c16-.T64
	rm -f cbmfiles/test.log
	VICE=xplus4 emulator/run-in-vice.sh v4th-c16- \
	  "include run-lite-tests.fth\n1234567890\n"
	petscii2ascii cbmfiles/test.log $@

test-v4th-x16.log: cbmfiles/v4th-x16 emulator/sdcard.img
	rm -f cbmfiles/test.log
	emulator/run-in-x16emu.sh v4th-x16 \
	  "INCLUDE RUN-LITE-TESTS.FTH\\X0D1234567890\\X0D"
	mcopy -i emulator/sdcard.img ::TEST.LOG cbmfiles/test.log
	petscii2ascii cbmfiles/test.log $@

test-%.result: test-%.log test-%.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)

disks/scratch.d64: disks/empty.d64
	cp $< $@

emulator/sdcard.img: emulator/sdcard.sfdisk
	rm -f $@ $@.tmp
	dd if=/dev/zero of=$@.tmp count=64 bs=1M
	sfdisk -w always -W always $@.tmp < $<
	mformat -i $@.tmp -F
	mv $@.tmp $@

test-vf-full-c64.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double block report-blk)
	cat $? > $@

test-v4th-c64.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double report-noblk)
	cat $? > $@

test-vf-full-c16+.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double block report-blk)
	cat $? > $@

test-v4th-c16+.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double report-noblk)
	cat $? > $@

test-vf-full-c16-.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core)
	cat $? > $@

test-v4th-c16-.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double report-noblk)
	cat $? > $@

test-v4th-x16.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double report-noblk)
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
