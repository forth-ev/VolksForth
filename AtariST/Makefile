
# Hatari setup:
# sudo apt install hatari
# sudo cp emutos-512k-1.3/etos512us.img /usr/share/hatari/tos.img

srcfbfiles = $(wildcard src/*.fb) $(wildcard src/gem/*.fb)
srcfbtxtfiles = $(patsubst src/%.fb, src/%.fb.txt, $(srcfbfiles))
# srcfbfiles = $(wildcard *.FB)
# srcfbtxtfiles = $(patsubst %.FB, src/%.fb.txt, $(srcfbfiles))
# testsfbfiles = $(wildcard tests/*.fb)
# testsfbtxtfiles = $(patsubst tests/%.fb, tests/%.fb.txt, $(testsfbfiles))
fb_txt_files = $(srcfbtxtfiles) # $(testsfbtxtfiles)

stfilesdir = stfiles

fb.txt: $(fb_txt_files)

run-4thcore: \
    $(patsubst %, $(stfilesdir)/%, 4thcore.prg \
    assemble.fb editor.fb ediicon.fb edwindow.fb \
    gem/gemdefs.fb gem/supergem.fb)
	hatari --mono --sound off --cpuclock 32 --auto 4thcore.prg \
	--confirm-quit false stfiles

run-volks4th: \
    $(patsubst %, $(stfilesdir)/%, volks4th.prg \
    assemble.fb editor.fb ediicon.fb edwindow.fb \
    gem/gemdefs.fb gem/supergem.fb)
	hatari --mono --sound off --cpuclock 32 --auto volks4th.prg \
	--confirm-quit false stfiles

run-hconsole:
	./emulator/run-in-hatari.py 4thcore.prg ".( hello world)" "1 2 + ."

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
	--confirm-quit false stfiles

# $(srcfbtxtfiles): src/%.fb.txt: %.FB ../tools/fb2fth.py
# 	../tools/fb2fth.py $< $@

$(srcfbtxtfiles): src/%.fb.txt: src/%.fb ../tools/fb2fth.py
	../tools/fb2fth.py $< $@

# $(testsfbtxtfiles): tests/%.fb.txt: tests/%.fb ../tools/fb2fth.py
# 	../tools/fb2fth.py $< $@

$(stfilesdir)/%: src/%
	mydir=$$(dirname $@) && test -d "$$mydir" || mkdir -p "$$mydir"
	cp $< $@

$(stfilesdir)/%: tests/%
	mydir=$$(dirname $@) && test -d "$$mydir" || mkdir -p "$$mydir"
	cp $< $@

$(stfilesdir)/%: %
	test -d $(stfilesdir) || mkdir -p $(stfilesdir)
	cp $< $@
