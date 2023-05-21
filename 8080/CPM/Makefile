
fbfiles = $(wildcard src/*.fb)
fthfiles = $(patsubst src/%.fb, src/%.fth, $(fbfiles))

whitch_runcpm = $(shell which RunCPM)
runcpmdir = runcpm
cpmfilesdir = cpmfiles

fth: $(fthfiles)

clean:
	rm -f *.log
	rm -rf $(runcpmdir)
	rm -f msdos

run-editor: | msdos
	FORTHPATH="f:\\src;f:\\msdos" ../../8086/msdos/emulator/run-in-dosbox.sh f:\\msdos\\volks4th.com

msdos:
	ln -s ../../8086/msdos msdos

src/%.fth: src/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

inctest.log: $(patsubst %, $(cpmfilesdir)/%, volks4th.com) \
    $(patsubst src/%, $(cpmfilesdir)/%, \
    src/include.fb src/inctest.fth) \
    | emu
	echo "volks4th" > $(runcpmdir)/input.script
	echo "include include.fb" >> $(runcpmdir)/input.script
	echo "include inctest.fth" >> $(runcpmdir)/input.script
	echo "bye" >> $(runcpmdir)/input.script
	echo "exit" >> $(runcpmdir)/input.script
	./emulator/run-in-runcpm.sh volks4th
	cp $(runcpmdir)/output.log $@

emu: $(runcpmdir)/RunCPM

$(runcpmdir)/RunCPM: $(whitch_runcpm)
	test -d $(runcpmdir) || mkdir -p $(runcpmdir)
	cp $< $@

$(cpmfilesdir)/%: src/%
	test -d $(cpmfilesdir) || mkdir -p $(cpmfilesdir)
	cp $< $@

$(cpmfilesdir)/%: %
	test -d $(cpmfilesdir) || mkdir -p $(cpmfilesdir)
	cp $< $@
