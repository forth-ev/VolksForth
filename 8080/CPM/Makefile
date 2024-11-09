
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
	rm -f $(cpmfilesdir)/empty.f*

veryclean: clean
	rm -rf $(cpmfilesdir)

test: test-min.result test-std.result test-blk.result

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

$(cpmfilesdir)/tc-base.com: \
$(patsubst %, $(cpmfilesdir)/%, kernel.com \
    fileint.fb ass8080.fb include.fb log2file.fb xinout.fb savesys.fb) \
    Makefile | emu
	rm -f $(runcpmdir)/A/0/TC-BASE.COM $@
	./emulator/run-in-runcpm.sh \
	    "kernel fileint.fb" \
	    "1 load" \
	    "include log2file.fb" \
	    "logopen" \
	    "include ass8080.fb" \
	    ".( include xinout.fb) cr" \
	    "include savesys.fb" \
	    "$50 constant /tib" \
	    "include include.fb" \
	    "cr   decimal   caps on" \
	    "scr off  r# off     savesystem tc-base.com" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@.log
	cp -f $(runcpmdir)/A/0/TC-BASE.COM $@

$(cpmfilesdir)/v4th.com: \
    $(patsubst %, $(cpmfilesdir)/%, tc-base.com \
    target.fb v4th.fth vf-core.fth vf-file.fth \
    vf-io.fth vf-bufs.fth vf-sys.fth vf-end.fth vf-bdos.fth) \
    Makefile | emu
	rm -f $(runcpmdir)/A/0/V4TH.COM $(runcpmdir)/A/0/LOGFILE.TXT $@
	./emulator/run-in-runcpm.sh \
	    "tc-base" \
	    "logopen" \
	    "include target.fb" \
	    "include v4th.fth" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@.log
	cp -f $(runcpmdir)/A/0/V4TH.COM $@

$(cpmfilesdir)/v4th3.com: \
    $(patsubst %, $(cpmfilesdir)/%, tc-base.com \
    target.fb v4th3.fth vf-core.fth vf-io.fth \
    vf-bufs.fth vf-sys.fth vf-file1.fth vf-end.fth vf-bdos.fth) \
    Makefile | emu
	rm -f $(runcpmdir)/A/0/V4TH.COM $(runcpmdir)/A/0/LOGFILE.TXT $@
	./emulator/run-in-runcpm.sh \
	    "tc-base" \
	    "logopen" \
	    "include target.fb" \
	    "include v4th3.fth" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@.log
	cp -f $(runcpmdir)/A/0/V4TH.COM $@

test-kernel.log: \
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

test-min.log: \
    $(patsubst %, $(cpmfilesdir)/%, v4th.com sfileint.fth \
    log2file.fb \
    ans-shim.fth prelim.fth tester.fth core.fr test-min.fth) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "v4th sfileint.fth" \
	    "include-isfile" \
	    "onlyforth" \
	    "include test-min.fth" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

test-std.log: \
    $(patsubst %, $(cpmfilesdir)/%, v4th.com sfileint.fth \
    log2file.fb core.fr) \
    $(patsubst tests/%, $(cpmfilesdir)/%, $(wildcard tests/*.fth)) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "v4th sfileint.fth" \
	    "include-isfile" \
	    "onlyforth" \
	    "include test-std.fth" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

test-blk.log: \
    $(patsubst %, $(cpmfilesdir)/%, v4th.com sfileint.fth \
    log2file.fb core.fr) \
    $(patsubst tests/%, $(cpmfilesdir)/%, $(wildcard tests/*.fth) \
    tests/empty.fb) | emu
	./emulator/run-in-runcpm.sh \
	    "v4th sfileint.fth" \
	    "include-isfile" \
	    "onlyforth" \
	    "include test-blk.fth" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

test3.log: \
    $(patsubst %, $(cpmfilesdir)/%, v4th3.com sfileint.fth \
    log2file.fb core.fr) \
    $(patsubst tests/%, $(cpmfilesdir)/%, $(wildcard tests/*.fth) \
    tests/empty.fb) | emu
	./emulator/run-in-runcpm.sh \
	    "v4th3 sfileint.fth" \
	    "include-isfile" \
	    "onlyforth" \
	    "include test-blk.fth" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

emu: $(runcpmdir)/RunCPM

test-min.golden: $(patsubst %, tests/golden/%.golden, prelim core)
	cat $^ > $@

test-std.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext doubltst report-noblk)
	cat $^ > $@

test-blk.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext doubltst block report-blk)
	cat $^ > $@

test3.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext doubltst block report-blk)
	cat $^ > $@

test-kernel.golden: $(patsubst %, tests/golden/%.golden, prelim core)
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
