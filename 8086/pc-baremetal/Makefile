TARGET = forth.com
BASE = ../..
BLKPACK = $(BASE)/tools/blkpack

.PHONY: all
all: $(TARGET)

%.fb: %.fth $(BLKPACK)
	$(BLKPACK) < $< > $@

$(TARGET): kernel.fb meta.fb
	emu2 $(BASE)/8086/msdos/volks4th.com "include kernel.fb"

.PHONY: clean
clean:
	rm -f $(TARGET) meta.com *.fb
