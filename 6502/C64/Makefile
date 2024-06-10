
# This Makefile and the build scripts in the emulator/ subdir require
# the file format conversion tools from the tools/ subdir of
# https://github.com/pzembrod/cc64 to be installed.

vf_blk_d64_files = $(wildcard disks/*.d64)
vf_blk_fth_files = $(patsubst %.d64, %.fth, $(vf_blk_d64_files))
vf_fth_files = $(wildcard src/vf-*.fth src/v4th*.fth)
vf_fth_files_petscii = $(patsubst src/%, cbmfiles/%, $(vf_fth_files))
vf_flavours = \
    v4thblk-c64 v4thblk-c16+ v4thblk-c16- \
    v4th-c64 v4th-c16+ v4th-c16- v4th-x16 v4th-x16e
vf_binaries = $(patsubst %, cbmfiles/%, $(vf_flavours))

all_src_files = $(wildcard src/*.fth)
all_src_files_petscii = $(patsubst src/%, cbmfiles/%, $(all_src_files))

test_files = $(wildcard tests/*.f*)
test_files_petscii = $(patsubst tests/%, cbmfiles/%, $(test_files))
test_logs = $(patsubst %, test-%.log, $(vf_flavours))
test_resuls = $(patsubst %, test-%.result, $(vf_flavours))
test_keyboard_input = some 12345 keys

release_zipfile = volksforth-6502-c64-release.zip


# Target to convert all .d64 images into .fth files for easier reading,
# and to update all PETSCII files in cbmfiles to the latest state of
# the corresponding ASCII files in src.
update: $(vf_blk_fth_files) $(vf_fth_files_petscii)


clean:
	rm -f cbmfiles/*.fr cbmfiles/*.fth cbmfiles/*.log
	rm -rf tmp/* release
	rm -f *.log *.result *.golden
	rm -f cbmfiles/c??-testbase
	rm -f disks/scratch.d64 emulator/sdcard.img
	rm -f tests/golden/mycore*.golden


# Convenience targets

binaries: $(vf_binaries)

test: $(test_resuls)

alltests: test test-v4th-x16-tsk.result

test64: std64 blk64

blk64: test-v4thblk-c64.result

std64: test-v4th-c64.result

debug-64: emulator/tcbase.T64 emulator/build-vf.sh \
    disks/vforth4_2.d64 disks/tc38q.d64 $(vf_fth_files_petscii)
	emulator/build-vf.sh v4th-c64 nosave

run-devenv: emulator/devenv.T64
	emulator/run-in-vice.sh devenv

run-testbase: emulator/testbase.T64
	emulator/run-in-vice.sh testbase

run-testbase16: emulator/testbase16.T64
	VICE=xplus4 emulator/run-in-vice.sh testbase16


release: tmp/$(release_zipfile) COPYING RELEASE_NOTES.md
	rm -rf release
	mkdir release
	cp -p $^ release/

tmp/$(release_zipfile): $(vf_binaries) \
    $(all_src_files) $(all_src_files_petscii) \
    $(test_files) $(test_files_petscii) COPYING RELEASE_NOTES.md
	rm -rf tmp
	mkdir tmp
	mkdir tmp/binaries
	mkdir tmp/src
	mkdir tmp/src_petscii
	mkdir tmp/tests
	mkdir tmp/tests_petscii
	cp -p COPYING RELEASE_NOTES.md tmp/
	cp -p $(vf_binaries) tmp/binaries/
	cp -p $(all_src_files) tmp/src/
	cp -p $(all_src_files_petscii) tmp/src_petscii/
	cp -p $(test_files) tmp/tests/
	cp -p $(test_files_petscii) tmp/tests_petscii/
	(cd tmp &&	zip -r $(release_zipfile) COPYING RELEASE_NOTES.md \
	    binaries src src_petscii tests tests_petscii)


# Targetcompiler targets

cbmfiles/tcbase: emulator/v4th-c64-4tc.T64 emulator/build-tcbase.sh \
    disks/tc38q.d64 disks/file-words.d64 cbmfiles/tc-base.fth
	emulator/build-tcbase.sh

$(vf_binaries): $(vf_fth_files_petscii) emulator/build-vf.sh \
    emulator/tcbase.T64 disks/tc38q.d64 cbmfiles/logtofile.fth

cbmfiles/v4thblk-c64:
	emulator/build-vf.sh v4thblk-c64

cbmfiles/v4thblk-c16+:
	emulator/build-vf.sh v4thblk-c16+

cbmfiles/v4thblk-c16-:
	emulator/build-vf.sh v4thblk-c16-

cbmfiles/v4th-c64:
	emulator/build-vf.sh v4th-c64

cbmfiles/v4th-c16+:
	emulator/build-vf.sh v4th-c16+

cbmfiles/v4th-c16-:
	emulator/build-vf.sh v4th-c16-

cbmfiles/v4th-x16:
	emulator/build-vf.sh v4th-x16

cbmfiles/v4th-x16e:
	emulator/build-vf.sh v4th-x16e


# Core test targets

$(test_logs): $(test_files_petscii) $(wildcard emulator/run-in-*.sh)

test-v4thblk-c64.log: emulator/v4thblk-c64.T64 disks/empty.d64
	rm -f cbmfiles/test.log disks/scratch.d64
	cp disks/empty.d64 disks/scratch.d64
	DISK9=scratch emulator/run-in-vice.sh v4thblk-c64 \
	  "include run-blk-tests.fth\n$(test_keyboard_input)"
	petscii2ascii cbmfiles/test.log $@

test-v4th-c64.log: emulator/v4th-c64.T64
	rm -f cbmfiles/test.log
	emulator/run-in-vice.sh v4th-c64 \
	  "include run-std-tests.fth\n$(test_keyboard_input)"
	petscii2ascii cbmfiles/test.log $@

test-v4thblk-c16+.log: emulator/v4thblk-c16+.T64 disks/empty.d64
	rm -f cbmfiles/test.log disks/scratch.d64
	cp disks/empty.d64 disks/scratch.d64
	VICE=xplus4 DISK9=scratch emulator/run-in-vice.sh v4thblk-c16+ \
	  "include run-blk-tests.fth\n$(test_keyboard_input)"
	petscii2ascii cbmfiles/test.log $@

test-v4th-c16+.log: emulator/v4th-c16+.T64
	rm -f cbmfiles/test.log
	VICE=xplus4 emulator/run-in-vice.sh v4th-c16+ \
	  "include run-std-tests.fth\n$(test_keyboard_input)"
	petscii2ascii cbmfiles/test.log $@

test-v4thblk-c16-.log: emulator/v4thblk-c16-.T64
	rm -f cbmfiles/test.log
	VICE=xplus4 emulator/run-in-vice.sh v4thblk-c16- \
	  "include run-min-tests.fth\n$(test_keyboard_input)"
	petscii2ascii cbmfiles/test.log $@

test-v4th-c16-.log: emulator/v4th-c16-.T64
	rm -f cbmfiles/test.log
	VICE=xplus4 emulator/run-in-vice.sh v4th-c16- \
	  "include run-std-tests.fth\n$(test_keyboard_input)"
	petscii2ascii cbmfiles/test.log $@

test-v4th-x16.log: cbmfiles/v4th-x16 | emulator/sdcard.img
	rm -f cbmfiles/test.log
	emulator/run-in-x16emu.sh v4th-x16 \
	  "include run-std-tests.fth\n$(test_keyboard_input)"
	mcopy -i emulator/sdcard.img ::TEST.LOG cbmfiles/test.log
	petscii2ascii cbmfiles/test.log $@

test-v4th-x16e.log: cbmfiles/v4th-x16e | emulator/sdcard.img
	rm -f cbmfiles/test.log
	emulator/run-in-x16emu.sh v4th-x16e \
	  "include run-std-tests.fth\n$(test_keyboard_input)"
	mcopy -i emulator/sdcard.img ::TEST.LOG cbmfiles/test.log
	petscii2ascii cbmfiles/test.log $@

test-%.result: test-%.log test-%.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)

emulator/sdcard.img: emulator/sdcard.sfdisk
	rm -f $@ $@.tmp
	dd if=/dev/zero of=$@.tmp count=64 bs=1M
	sfdisk -w always -W always $@.tmp < $<
	mformat -i $@.tmp -F
	mv $@.tmp $@

test-v4thblk-c64.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-echo coreext double block report-blk)
	cat $^ > $@

test-v4th-c64.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-echo coreext double report-noblk)
	cat $^ > $@

test-v4thblk-c16+.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-echo coreext double block report-blk)
	cat $^ > $@

test-v4th-c16+.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-echo coreext double report-noblk)
	cat $^ > $@

test-v4thblk-c16-.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-echo)
	cat $^ > $@

test-v4th-c16-.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-echo coreext double report-noblk)
	cat $^ > $@

test-v4th-x16.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-noecho coreext double report-noblk)
	cat $^ > $@

test-v4th-x16e.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-noecho coreext double report-noblk)
	cat $^ > $@

tests/golden/mycore-echo.golden: tests/golden/core-template.golden
	sed -e 's/TMPL_KEYS_ECHO_TMPL/$(test_keyboard_input)/' \
	   -e 's/TMPL_KEYS_RECEIVED_TMPL/$(test_keyboard_input)/' $< >$@

tests/golden/mycore-noecho.golden: tests/golden/core-template.golden
	sed -e 's/TMPL_KEYS_ECHO_TMPL//' \
	   -e 's/TMPL_KEYS_RECEIVED_TMPL/$(test_keyboard_input)/' $< >$@

# Sample test with a changed input that would run the tasker
# even while waiting for keyboard input:
test-v4th-x16-tsk.log: cbmfiles/v4th-x16 \
    $(test_files_petscii) $(wildcard emulator/run-in-*.sh) \
    cbmfiles/6502asm.fth cbmfiles/trns6502asm.fth \
    cbmfiles/vf-lbls-cbm.fth cbmfiles/x16input-tsk.fth \
    | emulator/sdcard.img
	rm -f cbmfiles/test.log
	emulator/run-in-x16emu.sh v4th-x16 \
	  "include x16input-tsk.fth\ninclude run-min-tests.fth\n$(test_keyboard_input)"
	mcopy -i emulator/sdcard.img ::TEST.LOG cbmfiles/test.log
	petscii2ascii cbmfiles/test.log $@

test-v4th-x16-tsk.golden: $(patsubst %, tests/golden/%.golden, \
    prelim mycore-echo)
	cat $^ > $@

# Rules for building Forth binaries on top of the plain vanilla
# c64-volksforth83.

cbmfiles/devenv: emulator/run-in-vice.sh emulator/build-devenv.sh \
    emulator/c64-volksforth83.T64 \
     disks/vforth4_1.d64  disks/vforth4_3.d64  disks/file-words.d64
	emulator/build-devenv.sh


# Generic T64 tape image rule

emulator/%.T64: cbmfiles/%
	bin2t64 $< $@
	touch -m -r $< $@


# Generic rule for populating cbmfiles/ with PETSCII text files

cbmfiles/%.fth: src/%.fth
	ascii2petscii $< $@
	touch -m -r $< $@

cbmfiles/%.fth: tests/%.fth
	ascii2petscii $< $@
	touch -m -r $< $@

cbmfiles/%.fr: tests/%.fr
	ascii2petscii $< $@
	touch -m -r $< $@


# Generic rule for converting .d64 blk sources into .fth files.

disks/%.fth: disks/%.d64
	ufscr2file $< $@
