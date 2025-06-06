
# Hatari setup:
# sudo apt install hatari
# sudo cp emutos-512k-1.3/etos512us.img /usr/share/hatari/tos.img

srcfbfiles = $(wildcard src/*.fb) $(wildcard src/gem/*.fb)
srcfbtxtfiles = $(patsubst src/%.fb, src/%.fb.txt, $(srcfbfiles))
# srcfbfiles = $(wildcard *.FB)
# srcfbtxtfiles = $(patsubst %.FB, src/%.fb.txt, $(srcfbfiles))
testsfbfiles = $(wildcard tests/*.fb)
testsfbtxtfiles = $(patsubst tests/%.fb, tests/%.fb.txt, $(testsfbfiles))
fb_txt_files = $(srcfbtxtfiles) $(testsfbtxtfiles)

stfilesdir = stfiles

fb.txt: $(fb_txt_files)

clean:
	rm -f *.log *.LOG *.result *.golden
	rm -rf $(stfilesdir)/*
	# rm -f tests/empty.fs tests/empty.fb.txt

test: logtest.result incltest.result test-org.result


run-4thcore: \
    $(patsubst %, $(stfilesdir)/%, 4thcore.prg \
    assemble.fb editor.fb ediicon.fb edwindow.fb \
    gem/gemdefs.fb gem/supergem.fb)
	hatari --mono --sound off --cpuclock 32 --auto 4thcore.prg \
	--confirm-quit false $(stfilesdir)

run-volks4th: \
    $(patsubst %, $(stfilesdir)/%, volks4th.prg ediicon.rsc \
    assemble.fb editor.fb ediicon.fb edwindow.fb include.fb \
    gem/gemdefs.fb gem/supergem.fb)
	hatari --mono --sound off --cpuclock 32 --auto volks4th.prg \
	--confirm-quit false $(stfilesdir)

manual-logtest: \
    $(patsubst %, $(stfilesdir)/%, volks4th.prg log2file.fb \
    assemble.fb editor.fb ediicon.fb edwindow.fb \
    gem/gemdefs.fb gem/supergem.fb)
	hatari --mono --sound off --cpuclock 32 --auto volks4th.prg \
	--confirm-quit false $(stfilesdir)

run-hconsole:
	./emulator/run-in-hatari.py 4thcore.prg ".( hello world)" "1 2 + ."

run-editor: | msdos
	FORTHPATH="f:\\src;f:\\tests;f:\\msdos" \
	../8086/msdos/emulator/run-in-dosbox.sh f:\\msdos\\volks4th.com

logtest.log: \
    $(patsubst %, $(stfilesdir)/%, volks4th.prg ediicon.rsc \
    log2file.fb assemble.fb editor.fb ediicon.fb edwindow.fb \
    gem/gemdefs.fb gem/supergem.fb) \
    Makefile
	rm -f $(stfilesdir)/output.log
	./emulator/run-in-hatari.py volks4th.prg \
	"include assemble.fb" \
	"include log2file.fb" \
	"logopen" \
	".( hello world)" \
	"1 2 + ." \
	"logclose"
	dos2unix -n $(stfilesdir)/output.log $@

logtest2.log: \
    $(patsubst %, $(stfilesdir)/%, volks4th.prg ediicon.rsc \
    log2file.fb assemble.fb editor.fb ediicon.fb edwindow.fb \
    gem/gemdefs.fb gem/supergem.fb) \
    Makefile
	rm -f $(stfilesdir)/output.log
	./emulator/run-in-hatari.py volks4th.prg \
	"use log2file.fb dos also 4 5 thru" \
	"logopen" \
	".( hello world)" \
	"1 2 + ." \
	"logclose"
	dos2unix -n $(stfilesdir)/output.log $@

incltest.log: \
    $(patsubst %, $(stfilesdir)/%, volks4th.prg ediicon.rsc \
    log2file.fb assemble.fb include.fb hello.fs) \
    Makefile
	rm -f $(stfilesdir)/output.log
	./emulator/run-in-hatari.py volks4th.prg \
	"include assemble.fb" \
	"include log2file.fb" \
	"logopen" \
	".( hello world)" \
	"1 2 + ." \
	"include include.fb" \
	"' .blk|tib Is .status" \
	"include hello.fs" \
	"logclose"
	dos2unix -n $(stfilesdir)/output.log $@

msdos:
	ln -s ../8086/msdos msdos

$(stfilesdir)/4thimg.prg: \
    $(patsubst %, $(stfilesdir)/%, volks4th.prg ediicon.rsc \
    assemble.fb c.fb forth83.fb target.fb) \
    Makefile
	./emulator/run-in-hatari.py volks4th.prg "include c.fb"

$(stfilesdir)/editor.prg: \
    $(patsubst %, $(stfilesdir)/%, 4thcore.prg \
    allocate.fb assemble.fb editor.fb ediicon.fb edwindow.fb \
    misc.fb strings.fb gem/aes.fb \
    gem/basics.fb gem/gemdefs.fb gem/supergem.fb gem/vdi.fb) \
    Makefile
	./emulator/run-in-hatari.py 4thcore.prg \
	"include assemble.fb" "include allocate.fb" "include strings.fb" \
	"include misc.fb" "include gem\\basics.fb" "include gem\\aes.fb" \
	"include editor.fb" \
	"save-system editor.prg"

run-4thimg.prg:
	hatari --mono --sound off --cpuclock 32 --auto 4thimg.prg \
	--confirm-quit false $(stfilesdir)

test-org.log: \
    $(patsubst %, $(stfilesdir)/%, volks4th.prg ediicon.rsc \
    include.fb log2file.fb \
    ans-shim.fs prelim.fs tester.fs core.fs test-min.fs) \
    Makefile
	./emulator/run-in-hatari.py volks4th.prg \
	"onlyforth" \
	"use log2file.fb dos also 4 5 thru" \
	"include include.fb" \
	"include test-min.fs"
	dos2unix -n $(stfilesdir)/output.log $@

test-min.log: \
    $(patsubst %, $(stfilesdir)/%, v4th.prg sfileint.fs \
    logfile.fs \
    ans-shim.fs prelim.fs tester.fs core.fs test-min.fs) \
    Makefile
	./emulator/run-in-hatari.py \
	    "v4th.prg sfileint.fs" \
	    "include-isfile" \
	    "onlyforth" \
	    "include test-min.fs" \
	    "bye" \
	    "exit"
	dos2unix -n $(runcpmdir)/logfile.txt $@


test-org.golden: test-min.golden
	cp $< $@

test-min.golden: $(patsubst %, tests/golden/%.golden, prelim core)
	cat $^ > $@

test-std.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext doubltst report-noblk)
	cat $^ > $@

test-blk.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext doubltst block report-blk)
	cat $^ > $@


%.golden: tests/golden/%.golden
	cp -p $< $@

%.result: %.log %.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)



# $(srcfbtxtfiles): src/%.fb.txt: %.FB ../tools/fb2fth.py
# 	../tools/fb2fth.py $< $@

$(srcfbtxtfiles): src/%.fb.txt: src/%.fb ../tools/fb2fth.py
	../tools/fb2fth.py $< $@

$(testsfbtxtfiles): tests/%.fb.txt: tests/%.fb ../tools/fb2fth.py
	../tools/fb2fth.py $< $@

$(stfilesdir)/%: src/%
	mydir=$$(dirname $@) && test -d "$$mydir" || mkdir -p "$$mydir"
	cp $< $@

$(stfilesdir)/%: tests/%
	mydir=$$(dirname $@) && test -d "$$mydir" || mkdir -p "$$mydir"
	cp $< $@

$(stfilesdir)/%: %
	test -d $(stfilesdir) || mkdir -p $(stfilesdir)
	cp $< $@
