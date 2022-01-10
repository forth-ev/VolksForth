
test: logtest.result

clean:
	rm -f *.log *.LOG *.result *.golden


logtest.log: volks4th.com tests/LOG2FILE.FB tests/LOGTEST.FB \
    emulator/run-in-dosbox.sh
	./emulator/run-in-dosbox.sh volks4th.com logtest

logtest.golden: tests/golden/logtest.golden
	cp -p $< $@

%.result: %.log %.golden tests/evaluate-test.sh
	rm -f $@
	tests/evaluate-test.sh $(basename $@)

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
