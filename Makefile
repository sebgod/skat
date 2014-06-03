MMC=mmc
SUDO=sudo
DIFF=diff -d
MCFLAGS=--use-grade-subdirs -O3
MLLIBS=--ml mercury_misc

.PHONY: test
test: skat.out

.PHONY: exps
exps: skat.exp

%.exp: test_%
	@./$< >$@

%.out: test_%
	@./$< >$@
	@printf "Testing $*: "
	@( $(DIFF) $*.exp $@ && echo success ) || ( rm $@ && echo failed )

skat.m: card.m suit.m rank.m deck.m

.PHONY: libskat
libskat: skat.m
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

test_skat: libskat test_skat.m
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

.PHONY: install
install: libskat
	@for lib in $^ ; do \
		$(SUDO) $(MMC) $(MCFLAGS) -m $@ $(MLLIBS) $$lib.install ; \
	done

.PHONY: clean
clean:
	rm -f *.out
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
