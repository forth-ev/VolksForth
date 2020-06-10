
# This Makefile and the build scripts in the emulator/ subdir require
# the file format conversion tools from the tools/ subdir of
# https://github.com/pzembrod/cc64 to be installed.

vf_blk_d64_files = $(wildcard disks/*.d64)
vf_blk_fth_files = $(patsubst %.d64, %.fth, $(vf_blk_d64_files))


# Target to convert all .d64 images into .fth files for easier reading.
vf_blk_fth: $(vf_blk_fth_files)


# Convenience targets

run-devenv: emulator/devenv.T64
	emulator/run-in-vice.sh devenv


# Rules for building Forth binaries on top of the plain vanilla
# c64-volksforth83.

cbmfiles/devenv: emulator/run-in-vice.sh emulator/build-devenv.sh \
    emulator/c64-volksforth83.T64
	emulator/build-devenv.sh


# Generic T64 tape image rule

emulator/%.T64: cbmfiles/%
	bin2t64 $< $@


# Generic rule for converting .d64 blk sources into .fth files.

disks/%.fth: disks/%.d64
	ufscr2file $< $@
