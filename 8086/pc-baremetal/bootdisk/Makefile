TARGET = mkimg144 flp144.bin

.PHONY: all
all: $(TARGET)

flp144.bin: flp144.asm
	nasm $< -f bin -o $@

mkimg144: mkimg144.c
	$(CC) -o $@ $<

.PHONY: clean
clean:
	rm -f $(TARGET)
