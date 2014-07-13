include ../Make.options
include ../Project.options

MODULES:=$(wildcard *.m) $(wildcard ../src/*.m)

.PHONY: runtests
runtests: test_$(PROJ_CNAME) $(PROJ_CNAME).out

.PHONY: runtests-verbose
runtests-verbose: test_$(PROJ_CNAME)

Mercury.modules: $(MODULES)
	$(MMC) -f $^

tags: $(MODULES)
	$(MTAGS) $^

test_$(PROJ_CNAME): Mercury.modules
	$(MMC) $(MCFLAGS) -m $@ $(MLLIBS)

.PHONY: exps
exps: $(PROJ_CNAME).exp

%.exp: test_%
	@./$< >$@

%.out: test_%
	@test -r $<.bat && ( exec $$COMSPEC /c $<.bat >$@ ) || ( ./$< >$@ )
	@rm -f $<.bat
	@printf "Testing $*: "
	@( $(DIFF) $(DIFF_FLAGS) $*.exp 1>$*.res $@ && echo success ) || \
		( rm $@ && echo failed )

.PHONY: clean
clean:
	$(MMC) --make test_$(PROJ_CNAME).clean

.PHONY: realclean
realclean:
	$(MMC) --make test_$(PROJ_CNAME).realclean
	rm -rf Mercury
	rm -f $(wildcard *.err) $(wildcard *.mh)
	rm -f $(wildcard *.out) $(wildcard *.res)
	rm -f Mercury.modules
	rm -f tags
