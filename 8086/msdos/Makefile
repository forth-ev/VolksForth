
srcfbfiles = $(wildcard src/*.fb)
srcfbtxtfiles = $(patsubst src/%.fb, src/%.fb.txt, $(srcfbfiles))
testsfbfiles = $(wildcard tests/*.fb)
testsfbtxtfiles = $(patsubst tests/%.fb, tests/%.fb.txt, $(testsfbfiles))
fb_txt_files = $(srcfbtxtfiles) $(testsfbtxtfiles)

test: incltest.result logtest.result test-std.result test-blk.result \
    incltest-volks4th.result test-volks4th-min.result test-stdi.result

fb.txt: $(fb_txt_files)

clean:
	rm -f *.log *.LOG *.result *.golden
	rm -f dosfiles/*
	rm -f tests/empty.fth tests/empty.fb.txt

*.log: emulator/run-in-dosbox.sh

metafile.com: v4thfile.com src/meta.fb src/mk-meta.fth tests/log2file.fb
	rm -f METAFILE.COM OUTPUT.LOG
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh \
	    v4thfile.com "include mk-meta.fth"
	dos2unix -n OUTPUT.LOG metafile.log
	grep -F 'Metacompiler saved as metafile.com' metafile.log

v4th.com: metafile.com src/meta.fb src/v4th.fth src/vf86core.fth \
    src/vf86dkey.fth src/vf86dos.fth src/vf86file.fth src/vf86end.fth
	rm -f v4th.com V4TH.COM OUTPUT.LOG
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh \
	    metafile.com "include v4th.fth"
	dos2unix -n OUTPUT.LOG v4th.log
	mv V4TH.COM v4th.com
	grep -F 'unresolved:' v4th.log
	grep -F 'new kernel written as v4th.com' v4th.log
	grep -i 'unresolved:.*[^ ]' v4th.log && exit 1 || true

v4thblk.com: metafile.com src/meta.fb src/v4thblk.fth src/vf86core.fth \
    src/vf86dkey.fth src/vf86dos.fth src/vf86file.fth src/vf86bufs.fth \
    src/vf86end.fth
	rm -f v4thblk.com V4THBLK.COM OUTPUT.LOG
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh \
	    metafile.com "include v4thblk.fth"
	dos2unix -n OUTPUT.LOG v4thblk.log
	mv V4THBLK.COM v4thblk.com
	grep -F 'unresolved:' v4thblk.log
	grep -F 'new kernel written as v4thblk.com' v4thblk.log
	grep -i 'unresolved:.*[^ ]' v4thblk.log && exit 1 || true

v4thi.com: metafile.com src/meta.fb src/v4thi.fth src/vf86core.fth \
    src/vf86ikey.fth src/vf86dos.fth src/vf86file.fth src/vf86end.fth
	rm -f v4th.com V4TH.COM OUTPUT.LOG
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh \
	    metafile.com "include v4thi.fth"
	dos2unix -n OUTPUT.LOG v4thi.log
	mv V4THI.COM v4thi.com
	grep -F 'unresolved:' v4thi.log
	grep -F 'new kernel written as v4thi.com' v4thi.log
	grep -i 'unresolved:.*[^ ]' v4thi.log && exit 1 || true

# o4th for old volks4th - the new v4th is built with precompiled
# metacompiler metafile.com and mk-v4th.fth which writes a compile log.
o4th.com o4th.log: volks4th.com  src/kernel.fb
	rm -f FORTH.COM forth.com o4th.com
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh \
	    volks4th.com "include kernel.fb"
	dos2unix -n OUTPUT.LOG o4th.log
	mv FORTH.COM o4th.com

v4thfile.com: volks4th.com  src/include.fb src/v4thfile.fb \
    emulator/run-in-dosbox.sh
	rm -f V4THFILE.COM v4thfile.com
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh volks4th.com "include v4thfile.fb"
	mv V4THFILE.COM v4thfile.com

logtest.log: volks4th.com tests/log2file.fb tests/logtest.fb
	rm -f OUTPUT.LOG
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh volks4th.com "include logtest.fb"
	dos2unix -n OUTPUT.LOG $@

logappendtest.log: v4thfile.com tests/logapp.fth
	rm -f OUTPUT.LOG
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh v4thfile.com "include logapp.fth"
	dos2unix -n OUTPUT.LOG $@

prepsrcs = asm.fb extend.fb multi.vid dos.fb include.fb

prepfths = asm.fb extend.fb multi.vid dos.fb include.fb 86asm.fth \
    t86asm.fth extend2.fth multivid.fth dos2.fth dos3.fth

incltest.log: \
    $(patsubst %, dosfiles/%, v4thblk.com $(prepsrcs) log2file.fb \
        incltest.fth)
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh \
	    v4thblk.com "include incltest.fth")
	dos2unix -n dosfiles/OUTPUT.LOG $@

test-std.log: \
    $(patsubst %, dosfiles/%, v4th.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4th.com \
	    "include logprep.fth include test-std.fth")
	dos2unix -n dosfiles/OUTPUT.LOG $@

test-blk.log: \
    $(patsubst %, dosfiles/%, v4thblk.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4thblk.com \
	    "include logprep.fth include test-blk.fth")
	dos2unix -n dosfiles/OUTPUT.LOG $@

test-stdi.log: \
    $(patsubst %, dosfiles/%, v4thi.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4thi.com \
	    "include logprep.fth include test-std.fth")
	dos2unix -n dosfiles/OUTPUT.LOG $@

forthblkdos: v4thblk.dos v4thblk.forth

forthdos: forthblkdos v4thfile.dos v4thfile.forth v4th0.dos v4th0.forth

v4th0.dos: \
    $(patsubst %, dosfiles/%, v4th.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4th.com \
	    "include logprep.fth include vocdos.fth")
	dos2unix -n dosfiles/OUTPUT.LOG output.log
	tr " " "\n" <output.log | sort >$@

v4th0.forth: \
    $(patsubst %, dosfiles/%, v4th.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4th.com \
	    "include logprep.fth include vocforth.fth")
	dos2unix -n dosfiles/OUTPUT.LOG output.log
	tr " " "\n" <output.log | sort >$@

v4thblk.dos: \
    $(patsubst %, dosfiles/%, v4thblk.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4thblk.com \
	    "include logprep.fth include vocdos.fth")
	dos2unix -n dosfiles/OUTPUT.LOG output.log
	tr " " "\n" <output.log | sort >$@

v4thblk.forth: \
    $(patsubst %, dosfiles/%, v4thblk.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4thblk.com \
	    "include logprep.fth include vocforth.fth")
	dos2unix -n dosfiles/OUTPUT.LOG output.log
	tr " " "\n" <output.log | sort >$@

v4thfile.dos: \
    $(patsubst %, dosfiles/%, v4thfile.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4thfile.com \
	    "include log2file.fth include vocdos.fth")
	dos2unix -n dosfiles/OUTPUT.LOG output.log
	tr " " "\n" <output.log | sort >$@

v4thfile.forth: \
    $(patsubst %, dosfiles/%, v4thfile.com $(prepfths)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4thfile.com \
	    "include log2file.fth include vocforth.fth")
	dos2unix -n dosfiles/OUTPUT.LOG output.log
	tr " " "\n" <output.log | sort >$@

incltest-volks4th.log: v4thfile.com tests/log2file.fb tests/incltest.fth
	rm -f OUTPUT.LOG
	FORTHPATH="f:\\;f:\\tests" ./emulator/run-in-dosbox.sh \
	    v4thfile.com "include incltest.fth"
	dos2unix -n OUTPUT.LOG $@

test-volks4th-min.log: v4thfile.com tests/* emulator/run-in-dosbox.sh
	rm -f OUTPUT.LOG
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh \
	    v4thfile.com "include test-min.fth"
	dos2unix -n OUTPUT.LOG $@


run-editor: volks4th.com emulator/run-in-dosbox.sh
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh volks4th.com


test-min.golden: $(patsubst %, tests/golden/%.golden, prelim core)
	cat $^ > $@

test-std.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext doubltst report-noblk)
	cat $^ > $@

test-blk.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext doubltst block report-blk)
	cat $^ > $@

test-volks4th-min.golden: $(patsubst %, tests/golden/%.golden, \
    volks4th-prelim core)
	cat $^ > $@

test-stdi.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreplus coreext doubltst report-noblk)
	cat $^ > $@


%.golden: tests/golden/%.golden
	cp -p $< $@

%-volks4th.golden: tests/golden/%.golden
	cp -p $< $@

%.result: %.log %.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)


dosfiles/%: % | dosfiles
	cp $< $@

dosfiles/%: src/% | dosfiles
	cp $< $@

dosfiles/%: tests/% | dosfiles
	cp $< $@

dosfiles:
	mkdir $@

$(srcfbtxtfiles): src/%.fb.txt: src/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

$(testsfbtxtfiles): tests/%.fb.txt: tests/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@
