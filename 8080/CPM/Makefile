
srcfbfiles = $(wildcard src/*.fb)
srcfthfiles = $(patsubst src/%.fb, src/%.fth, $(srcfbfiles))
testsfbfiles = $(wildcard tests/*.fb)
testsfthfiles = $(patsubst tests/%.fb, tests/%.fth, $(testsfbfiles))
fthfiles = $(srcfthfiles) $(testsfthfiles)

whitch_runcpm = $(shell which RunCPM)
runcpmdir = runcpm
cpmfilesdir = cpmfiles

fth: $(fthfiles)

clean:
	rm -f *.log *.golden *.result
	rm -rf $(runcpmdir)
	rm -f msdos

run-editor: | msdos
	FORTHPATH="f:\\src;f:\\tests;f:\\msdos" \
	../../8086/msdos/emulator/run-in-dosbox.sh f:\\msdos\\volks4th.com

msdos:
	ln -s ../../8086/msdos msdos

src/%.fth: src/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

tests/%.fth: tests/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

inctest.log: \
    $(patsubst %, $(cpmfilesdir)/%, volks4th.com) \
    $(patsubst src/%, $(cpmfilesdir)/%, src/include.fb) \
    $(patsubst tests/%, $(cpmfilesdir)/%, tests/inctest.fth) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "volks4th" \
	    "include include.fb" \
	    "include inctest.fth" \
	    "bye" \
	    "exit"
	cp $(runcpmdir)/output.log $@

logtest.log: \
    $(patsubst %, $(cpmfilesdir)/%, volks4th.com) \
    $(patsubst tests/%, $(cpmfilesdir)/%, tests/log2file.fb) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "volks4th" \
	    "include log2file.fb" \
	    "logopen" \
	    ".( hello world) cr" \
	    "logclose" \
	    "bye" \
	    "exit"
	cp $(runcpmdir)/output.log output.log
	../../tools/trunc-ctrl-z.py $(runcpmdir)/A/0/LOGFILE.TXT \
	    $(runcpmdir)/logfile.txt
	dos2unix -n $(runcpmdir)/logfile.txt $@

emu: $(runcpmdir)/RunCPM

%.golden: tests/golden/%.golden
	cp -p $< $@

%.result: %.log %.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)

$(runcpmdir)/RunCPM: $(whitch_runcpm)
	test -d $(runcpmdir) || mkdir -p $(runcpmdir)
	cp $< $@

$(cpmfilesdir)/%: src/%
	test -d $(cpmfilesdir) || mkdir -p $(cpmfilesdir)
	cp $< $@

$(cpmfilesdir)/%: tests/%
	test -d $(cpmfilesdir) || mkdir -p $(cpmfilesdir)
	cp $< $@

$(cpmfilesdir)/%: %
	test -d $(cpmfilesdir) || mkdir -p $(cpmfilesdir)
	cp $< $@
