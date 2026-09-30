RACKET ?= racket
RACO ?= raco

SOURCES := emu.rkt scheduler.rkt config.rkt gui.rkt
TEST_SOURCES := emu.rkt scheduler.rkt config.rkt

.PHONY: all clean run test

all:
	$(RACO) make $(SOURCES)

run: all
	$(RACKET) gui.rkt

test:
	$(RACO) test $(TEST_SOURCES)

clean:
	$(RM) -r compiled
