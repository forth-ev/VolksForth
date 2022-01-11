
test: logtest.result

clean:
	rm -f *.log *.LOG *.result *.golden

v4thfile.com: volks4th.com  src/V4THFILE.FB \
    emulator/run-in-dosbox.sh
	rm -f V4THFILE.COM v4thfile.com
	./emulator/run-in-dosbox.sh volks4th.com v4thfile.fb
	mv V4THFILE.COM v4thfile.com

incltest.log: v4thfile.com tests/LOG2FILE.FB tests/incltest.fth \
    emulator/run-in-dosbox.sh
	./emulator/run-in-dosbox.sh v4thfile.com incltest.fth

incltest.golden: tests/golden/incltest.golden
	cp -p $< $@

logtest.log: volks4th.com tests/LOG2FILE.FB tests/LOGTEST.FB \
    emulator/run-in-dosbox.sh
	./emulator/run-in-dosbox.sh volks4th.com logtest.fb

logtest.golden: tests/golden/logtest.golden
	cp -p $< $@

%.result: %.log %.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)

src/%.fth: src/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

tests/%.fth: tests/%.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@

fbfiles = $(wildcard src/*.FB tests/*.FB)
fthfiles = $(patsubst %.fb, %.fth, \
    $(shell ../../tools/echo-tolower.py $(fbfiles)))

fth: $(fthfiles)

.ONESHELL:
$(fthfiles): $(fbfiles)
	set -x
	for fb in $^
	do
	  echo fb: $$fb
	  fth=$$(../../tools/echo-tolower.py $$fb | sed -e 's/fb$$/fth/')
	  ../../tools/fb2fth.py $$fb $$fth
	done
