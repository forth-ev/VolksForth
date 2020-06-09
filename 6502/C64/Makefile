
# This Makefile and the build scripts in the emulator/ subdir require
# the file format conversion tools from the tools/ subdir of
# https://github.com/pzembrod/cc64 to be installed.

vf_blk_d64_files = $(wildcard disks/*.d64)
vf_blk_fth_files = $(patsubst %.d64, %.fth, $(vf_blk_d64_files))

vf_blk_fth: $(vf_blk_fth_files)

# Generic T64 tape image rule

emulator/%.T64: cbmfiles/%
	bin2t64 $< $@

# Generic rule for visualizing .d64 blk sources

disks/%.fth: disks/%.d64
	ufscr2file $< $@
