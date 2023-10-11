PROGS	= tig

all: $(PROGS)
all-debug: CFLAGS += $(DFLAGS)

clean:
	rm -rf manual.html-chunked
	rm -f $(PROGS) $(DOCS) core

.PHONY: all

tig: tig.c

