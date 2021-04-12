TARGET = forth.com
BASE = ../..
BLKPACK = $(BASE)/tools/blkpack
BOOTPRG = ./bootdisk

.PHONY: all
all: $(TARGET)

%.fb: %.fth $(BLKPACK)
	$(BLKPACK) < $< > $@

$(TARGET): kernel.fb meta.fb
	emu2 $(BASE)/8086/msdos/volks4th.com "include kernel.fb bye"

.PHONY: floppy
floppy:
	$(BOOTPRG)/mkimg144 -bs $(BOOTPRG)/flp144.bin -o floppy.img -us $(TARGET)

.PHONY: qemu
qemu:
	qemu-system-i386 -curses -drive file=floppy.img,if=floppy,format=raw  -monitor telnet:127.0.0.1:1234,server,nowait

.PHONY: clean
clean:
	rm -f $(TARGET) meta.com *.fb
