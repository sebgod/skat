MMC=mmc
SUDO=sudo
DIFF=diff -d --strip-trailing-cr
MCFLAGS=--use-grade-subdirs -O3 -E
MLLIBS=--ml mercury_misc
CLASSPATH=--java-classpath $(MERCURY_HOME)/lib/mercury/lib/java/mercury_misc.jar
TESTS=test_skat
ifdef GRADE
    MCFLAGS += -s $(GRADE) --no-detect-libgrades
endif
# this is a work-around for using make on MinGW32 when building with grade=java,
# as it creates the bash shell script instead of the Windows batch file
ifeq ($(OS),Windows_NT)
    MCFLAGS += --target-env-type windows
endif
.PRECIOUS: $(TESTS)

.PHONY: test
test: skat.out

.PHONY: exps
exps: skat.exp

%.exp: test_%
	@./$< >$@

%.out: test_%
	@test -r $<.bat && ( exec $$COMSPEC /c $<.bat >$@ ) || ( ./$< >$@ )
	@rm -f $<.bat
	@printf "Testing $*: "
	@( $(DIFF) $*.exp $@ && echo success ) || ( rm $@ && echo failed )

.PHONY: libskat
libskat:
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS) $(CLASSPATH)

test_%: libskat test_%.m
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS) $(CLASSPATH)

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
	rm -f $(TESTS)

.PHONY: realclean
realclean: clean
	rm -fR Mercury/
