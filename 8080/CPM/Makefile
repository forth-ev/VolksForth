
fth: include.fth

run-editor: msdos
	FORTHPATH="f:\\;f:\\msdos" ../../8086/msdos/emulator/run-in-dosbox.sh f:\\msdos\\volks4th.com

msdos:
	ln -s ../../8086/msdos msdos

%.fth: %.fb ../../tools/fb2fth.py
	../../tools/fb2fth.py $< $@
