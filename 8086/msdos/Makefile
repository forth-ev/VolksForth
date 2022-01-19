
fbfiles = $(wildcard src/*.fb tests/*.fb)
fthfiles = $(patsubst %.fb, %.fth, $(fbfiles))

fbfiles_uppercase = $(wildcard src/*.FB tests/*.FB)
fthfiles_caseconverted = $(patsubst %.fb, %.fth, \
    $(shell ../../tools/echo-tolower.py $(fbfiles_uppercase)))

test: incltest.result logtest.result test-min.result

fth: $(fthfiles) $(fthfiles_caseconverted)

clean:
	rm -f *.log *.LOG *.result *.golden

v4th.com:  volks4th.com  kernel.fb \
    emulator/run-in-dosbox.sh
	rm -f FORTH.COM forth.com v4th.com
	./emulator/run-in-dosbox.sh volks4th.com kernel.fb
	mv FORTH.COM v4th.com

v4thfile.com: volks4th.com  src/include.fb src/v4thfile.fb \
    emulator/run-in-dosbox.sh
	rm -f V4THFILE.COM v4thfile.com
	./emulator/run-in-dosbox.sh volks4th.com v4thfile.fb
	mv V4THFILE.COM v4thfile.com

logtest.log: volks4th.com tests/log2file.fb tests/logtest.fb \
    emulator/run-in-dosbox.sh
	./emulator/run-in-dosbox.sh volks4th.com logtest.fb

incltest.log: v4thfile.com tests/log2file.fb tests/incltest.fth \
    emulator/run-in-dosbox.sh
	./emulator/run-in-dosbox.sh v4thfile.com incltest.fth

test-min.log: v4thfile.com tests/* emulator/run-in-dosbox.sh
	rm -f TEST.LOG
	./emulator/run-in-dosbox.sh v4thfile.com test-min.fth
	mv TEST.LOG $@


test-min.golden: $(patsubst %, tests/golden/%.golden, prelim core)
	cat $? > $@

test-std.golden: $(patsubst %, tests/golden/%.golden, \
    prelim core coreext double report-noblk)
	cat $? > $@


%.golden: tests/golden/%.golden
	cp -p $< $@

%.result: %.log %.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)

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
