MMC=mmc
MCFLAGS=--use-grade-subdirs -O3
MLLIBS=--ml mercury_misc

SKAT_SUBS := $(wildcard *.m)

.PHONY: test
test: test_skat
	@./$<

.PHONY: libskat
libskat: skat.m $(SKATS_SUBS)
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

test_skat: libskat test_skat.m
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

.PHONY: install
install: libskat
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS) $<.install

.PHONY: clean
clean:
	rm -f *.init
	rm -f *.mh
	rm -f *.err
	rm -f *.a
	rm -f *.so
	rm -f *.dylib
	rm -f *.jar
	rm -f *.beams
	rm -f test_skat

.PHONY: realclean
realclean: clean
	rm -fR Mercury/
