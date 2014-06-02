MMC=mmc
SUDO=sudo
MCFLAGS=--use-grade-subdirs -O3
MLLIBS=--ml mercury_misc

.PHONY: test
test: test_skat
	@for test_case in $^ ; do \
		./$$test_case ; \
	done

.PHONY: libskat
libskat: skat.m suit.m
	@echo $^
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
