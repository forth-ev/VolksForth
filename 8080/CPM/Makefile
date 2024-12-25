
srcfbfiles = $(wildcard src/*.fb)
srcfbtxtfiles = $(patsubst src/%.fb, src/%.fb.txt, $(srcfbfiles))
testsfbfiles = $(wildcard tests/*.fb)
testsfbtxtfiles = $(patsubst tests/%.fb, tests/%.fb.txt, $(testsfbfiles))
fb_txt_files = $(srcfbtxtfiles) $(testsfbtxtfiles)

whitch_runcpm = $(shell which RunCPM)
runcpmdir = runcpm
cpmfilesdir = cpmfiles

bin: $(cpmfilesdir)/v4th.com

fb.txt: $(fb_txt_files)

clean:
	rm -f *.log *.golden *.result
	rm -rf $(runcpmdir)
	rm -f msdos
	rm -f $(cpmfilesdir)/empty.f*

veryclean: clean
	rm -rf $(cpmfilesdir)

test: test-min.result test-std.result test-blk.result

alltests: test-min.result test-std.result test-blk.result \
    logtest.result inctest.result test-kernel.result

run-editor: | msdos
	FORTHPATH="f:\\src;f:\\tests;f:\\msdos" \
	../../8086/msdos/emulator/run-in-dosbox.sh f:\\msdos\\volks4th.com

msdos:
	ln -s ../../8086/msdos msdos

$(srcfbtxtfiles): src/%.fb.txt: src/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

$(testsfbtxtfiles): tests/%.fb.txt: tests/%.fb ../../tools/fb2fth.py
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
    include.fb log2file.fb inctest.fs) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "kernel fileint.fb" \
	    "1 load onlyforth" \
	    "$50 constant /tib" \
	    "include include.fb" \
	    "include log2file.fb" \
	    "logopen" \
	    "include inctest.fs" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

$(cpmfilesdir)/tc-base.com: \
$(patsubst %, $(cpmfilesdir)/%, v4th-4tc.com \
    fileint.fb ass8080.fb include.fb log2file.fb xinout.fb savesys.fb) \
    Makefile | emu
	rm -f $(runcpmdir)/A/0/TC-BASE.COM $@
	./emulator/run-in-runcpm.sh \
	    "v4th-4tc fileint.fb" \
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

$(cpmfilesdir)/tc-base2.com: \
$(patsubst %, $(cpmfilesdir)/%, v4thblk.com sfileint.fs \
    ass8080.fb include.fb log2file.fb xinout.fb savesys.fb) \
    Makefile | emu
	rm -f $(runcpmdir)/A/0/TC-BASE.COM $@
	./emulator/run-in-runcpm.sh \
	    "v4thblk sfileint.fs" \
	    "include-isfile" \
	    "include log2file.fb" \
	    "logopen" \
	    "include ass8080.fb" \
	    ".( include xinout.fb) cr" \
	    "include savesys.fb" \
	    "cr   decimal   caps on" \
	    "scr off  r# off     savesystem tc-base.com" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@.log
	cp -f $(runcpmdir)/A/0/TC-BASE.COM $@

$(cpmfilesdir)/v4th.com: \
    $(patsubst %, $(cpmfilesdir)/%, tc-base.com \
    target.fb v4th.fs vf-core.fs vf-file.fs \
    vf-io.fs vf-sys.fs vf-end.fs vf-bdos.fs) \
    Makefile | emu
	rm -f $(runcpmdir)/A/0/V4TH.COM $(runcpmdir)/A/0/LOGFILE.TXT $@
	./emulator/run-in-runcpm.sh \
	    "tc-base" \
	    "logopen" \
	    "include target.fb" \
	    "include v4th.fs" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@.log
	cp -f $(runcpmdir)/A/0/V4TH.COM $@

$(cpmfilesdir)/v4thblk.com: \
    $(patsubst %, $(cpmfilesdir)/%, tc-base.com \
    target.fb v4thblk.fs vf-core.fs vf-file.fs \
    vf-io.fs vf-bufs.fs vf-sys.fs vf-end.fs vf-bdos.fs) \
    Makefile | emu
	rm -f $(runcpmdir)/A/0/V4THBLK.COM $(runcpmdir)/A/0/LOGFILE.TXT $@
	./emulator/run-in-runcpm.sh \
	    "tc-base" \
	    "logopen" \
	    "include target.fb" \
	    "include v4thblk.fs" \
	    "logclose" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@.log
	cp -f $(runcpmdir)/A/0/V4THBLK.COM $@

test-kernel.log: \
    $(patsubst %, $(cpmfilesdir)/%, kernel.com fileint.fb \
    include.fb log2file.fb core.fs) \
    $(patsubst tests/%, $(cpmfilesdir)/%, $(wildcard tests/*.fs)) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "kernel fileint.fb" \
	    "1 load onlyforth" \
	    "$50 constant /tib" \
	    "include include.fb" \
	    "include test-krn.fs" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

test-min.log: \
    $(patsubst %, $(cpmfilesdir)/%, v4th.com sfileint.fs \
    logfile.fs \
    ans-shim.fs prelim.fs tester.fs core.fs test-min.fs) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "v4th sfileint.fs" \
	    "include-isfile" \
	    "onlyforth" \
	    "include test-min.fs" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

test-std.log: \
    $(patsubst %, $(cpmfilesdir)/%, v4th.com sfileint.fs \
    logfile.fs core.fs) \
    $(patsubst tests/%, $(cpmfilesdir)/%, $(wildcard tests/*.fs)) \
    | emu
	./emulator/run-in-runcpm.sh \
	    "v4th sfileint.fs" \
	    "include-isfile" \
	    "onlyforth" \
	    "include test-std.fs" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@

test-blk.log: \
    $(patsubst %, $(cpmfilesdir)/%, v4thblk.com sfileint.fs \
    sblkint.fs log2file.fb core.fs) \
    $(patsubst tests/%, $(cpmfilesdir)/%, $(wildcard tests/*.fs) \
    tests/empty.fb) | emu
	./emulator/run-in-runcpm.sh \
	    "v4thblk sfileint.fs" \
	    "include-isfile" \
	    "include sblkint.fs" \
	    "onlyforth" \
	    "include test-blk.fs" \
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

test-kernel.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext report-nodbl)
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
