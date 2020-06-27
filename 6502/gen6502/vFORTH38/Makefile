
blk_files = $(wildcard *.fb)
fth_files = $(patsubst %.fb, %.fth, $(blk_files))

# Target to convert all .fb blk sources into .fth files.
fth: $(fth_files)

# Generic rule for converting .fb blk sources into .fth files.

%.fth: %.fb
	./fb2fth.py $< $@
