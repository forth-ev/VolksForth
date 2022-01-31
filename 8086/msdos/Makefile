
fbfiles = $(wildcard src/*.fb tests/*.fb)
fthfiles = $(patsubst %.fb, %.fth, $(fbfiles))

fbfiles_uppercase = $(wildcard src/*.FB tests/*.FB)
fthfiles_caseconverted = $(patsubst %.fb, %.fth, \
    $(shell ../../tools/echo-tolower.py $(fbfiles_uppercase)))

test: incltest.result logtest.result test-min.result \
    incltest-volks4th.result test-volks4th-min.result

fth: $(fthfiles) $(fthfiles_caseconverted)

clean:
	rm -f *.log *.LOG *.result *.golden
	rm -f dosfiles/*

*.log: emulator/run-in-dosbox.sh

# TODO: Make v4th.log contain something and check its contents
v4th.com v4th.log:  volks4th.com  kernel.fb tests/log2file.fb
	rm -f FORTH.COM forth.com v4th.com
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh \
	    volks4th.com "include kernel.fb"
	dos2unix -n OUTPUT.LOG v4th.log
	mv FORTH.COM v4th.com

v4thfile.com: volks4th.com  src/include.fb src/v4thfile.fb \
    emulator/run-in-dosbox.sh
	rm -f V4THFILE.COM v4thfile.com
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh volks4th.com "include v4thfile.fb"
	mv V4THFILE.COM v4thfile.com

logtest.log: volks4th.com tests/log2file.fb tests/logtest.fb
	rm -f OUTPUT.LOG
	FORTHPATH="f:\\;f:\\src;f:\\tests" ./emulator/run-in-dosbox.sh volks4th.com "include logtest.fb"
	dos2unix -n OUTPUT.LOG $@

prepsrcs = asm.fb extend.fb multi.vid dos.fb include.fb

incltest.log: \
    $(patsubst %, dosfiles/%, v4th.com $(prepsrcs) log2file.fb \
        incltest.fth)
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh \
	    v4th.com "include include.fb include incltest.fth")
	dos2unix -n dosfiles/OUTPUT.LOG $@

test-min.log: \
    $(patsubst %, dosfiles/%, v4th.com $(prepsrcs)) \
    $(patsubst tests/%, dosfiles/%, $(wildcard tests/*.*))
	rm -f dosfiles/OUTPUT.LOG
	(cd dosfiles && ../emulator/run-in-dosbox.sh v4th.com \
	    "include testprep.fb include test-min.fth")
	dos2unix -n dosfiles/OUTPUT.LOG $@


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
	cat $? > $@

test-std.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double report-noblk)
	cat $? > $@

test-volks4th-min.golden: $(patsubst %, tests/golden/%.golden, \
    volks4th-prelim core)
	cat $? > $@


%.golden: tests/golden/%.golden
	cp -p $< $@

%-volks4th.golden: tests/golden/%.golden
	cp -p $< $@

%.result: %.log %.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)


dosfiles/%: %
	test -d dosfiles || mkdir dosfiles
	cp $< $@

dosfiles/%: src/%
	test -d dosfiles || mkdir dosfiles
	cp $< $@

dosfiles/%: tests/%
	test -d dosfiles || mkdir dosfiles
	cp $< $@


src/%.fth: src/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

tests/%.fth: tests/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

# Collective rule for converting uppercase *.FB to lowercase *.fth.
# Because make doesn't provide case changing pattern matching,
# file-by-file dependencies as with the src/%.fth and tests/%.fth
# rules doesn't seem feasible here, hence the one collective rule.

.ONESHELL:
$(fthfiles_caseconverted): $(fbfiles_uppercase)
	set -x
	for fb in $^
	do
	  echo fb: $$fb
	  fth=$$(../../tools/echo-tolower.py $$fb | sed -e 's/fb$$/fth/')
	  ../../tools/fb2fth.py $$fb $$fth
	done
