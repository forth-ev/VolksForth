BLKPACK_TGT = blkpack
BLKUNPACK_TGT = blkunpack
TARGETS = $(BLKUNPACK_TGT) $(BLKPACK_TGT)

all: $(TARGETS)

.SUFFIXES: .c .o
.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

$(BLKPACK_TGT): $(BLKPACK_TGT).c
$(BLKUNPACK_TGT): $(BLKUNPACK_TGT).c
$(TARGETS):
	$(CC) $(CFLAGS) $@.c -o $@

.PHONY: clean
clean:
	rm -f $(TARGETS) $(OBJS)
