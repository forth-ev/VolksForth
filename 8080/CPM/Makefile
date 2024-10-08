
srcfbfiles = $(wildcard src/*.fb)
srcfthfiles = $(patsubst src/%.fb, src/%.fth, $(srcfbfiles))
testsfbfiles = $(wildcard tests/*.fb)
testsfthfiles = $(patsubst tests/%.fb, tests/%.fth, $(testsfbfiles))
fthfiles = $(srcfthfiles) $(testsfthfiles)

whitch_runcpm = $(shell which RunCPM)
runcpmdir = runcpm
cpmfilesdir = cpmfiles

bin: $(cpmfilesdir)/v4th.com

fth: $(fthfiles)

clean:
	rm -f *.log *.golden *.result
	rm -rf $(runcpmdir)
	rm -f msdos

veryclean: clean
	rm -rf $(cpmfilesdir)

test: test-v4th.result

alltests: logtest.result inctest.result test-min.result test-v4th.result

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

$(cpmfilesdir)/v4th.com: \
    $(patsubst %, $(cpmfilesdir)/%, volks4th.com \
    include.fb log2file.fb target.fb source.fb v4th.fth vf-core.fth) \
    Makefile | emu
	rm -f $(runcpmdir)/A/0/V4TH.COM $@
	./emulator/run-in-runcpm.sh \
	    "volks4th" \
	    "include log2file.fb" \
	    "logopen" \
	    "include include.fb" \
	    "include target.fb" \
	    "include v4th.fth" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@.log
	cp -f $(runcpmdir)/A/0/V4TH.COM $@

v4th2.com: \
    $(patsubst %, $(cpmfilesdir)/%, volks4th.com \
    include.fb log2file.fb target.fb source.fb) \
    | emu
	rm -f $(runcpmdir)/A/0/KERNEL.COM $@
	./emulator/run-in-runcpm.sh \
	    "volks4th" \
	    "include log2file.fb" \
	    "logopen" \
	    "include target.fb" \
	    "include source.fb" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@.log
	cp $(runcpmdir)/A/0/KERNEL.COM $@

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

test-v4th.log: \
    $(patsubst %, $(cpmfilesdir)/%, v4th.com fileint.fb \
    include.fb log2file.fb \
    ans-shim.fth prelim.fth tester.fth core.fr test-min.fth) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "v4th fileint.fb" \
	    "1 load onlyforth" \
	    "include include.fb" \
	    "include test-min.fth" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

emu: $(runcpmdir)/RunCPM

test-min.golden: $(patsubst %, tests/golden/%.golden, prelim core)
	cat $^ > $@

test-v4th.golden: $(patsubst %, tests/golden/%.golden, prelim core)
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
