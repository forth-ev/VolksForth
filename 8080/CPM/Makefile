
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

test: logtest.result inctest.result test-min.result

run-editor: | msdos
	FORTHPATH="f:\\src;f:\\tests;f:\\msdos" \
	../../8086/msdos/emulator/run-in-dosbox.sh f:\\msdos\\volks4th.com

msdos:
	ln -s ../../8086/msdos msdos

src/%.fth: src/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

tests/%.fth: tests/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

run-volks4th: \
    $(patsubst %, $(cpmfilesdir)/%, volks4th.com) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "volks4th"

logtest.log: \
    $(patsubst %, $(cpmfilesdir)/%, kernel.com fileint.fb log2file.fb) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "kernel fileint.fb" \
	    "1 load onlyforth" \
	    "include log2file.fb" \
	    "logopen" \
	    ".( hello world) cr" \
	    ".( 0123456789abcdef0123456789abcdef) cr" \
	    ".( 1123456789abcdef0123456789abcdef) cr" \
	    ".( 2123456789abcdef0123456789abcdef) cr" \
	    ".( 3123456789abcdef0123456789abcdef) cr" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

inctest.log: \
    $(patsubst %, $(cpmfilesdir)/%, kernel.com fileint.fb \
    include.fb log2file.fb inctest.fth) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "kernel fileint.fb" \
	    "1 load onlyforth" \
	    "include include.fb" \
	    "include log2file.fb" \
	    "logopen" \
	    "include inctest.fth" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

target.log: \
    $(patsubst %, $(cpmfilesdir)/%, volks4th.com \
    include.fb log2file.fb target.fb source.fb) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "volks4th" \
	    "include log2file.fb" \
	    "logopen" \
	    "include target.fb" \
	    "include source.fb" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

test-min.log: \
    $(patsubst %, $(cpmfilesdir)/%, kernel.com fileint.fb \
    include.fb log2file.fb \
    ans-shim.fth prelim.fth tester.fth core.fr test-min.fth) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "kernel fileint.fb" \
	    "1 load onlyforth" \
	    "include include.fb" \
	    "include test-min.fth" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

emu: $(runcpmdir)/RunCPM

test-min.golden: $(patsubst %, tests/golden/%.golden, prelim core)
	cat $^ > $@

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
