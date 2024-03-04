PROGS	= tig

all: $(PROGS)
all-debug: CFLAGS += $(DFLAGS)

clean:
	rm -rf manual.html-chunked
	rm -f $(PROGS) $(DOCS) core

.PHONY: all

tig: tig.c
	gcc -o tig tig.c -lncurses

graph: tig.c rtl2dot.py
	gcc -o tig -fdump-rtl-expand tig.c -lncurses
	python3 rtl2dot.py tig.c.233r.expand --root main --ignore "report" --local | dot -Tsvg > tig.svg

