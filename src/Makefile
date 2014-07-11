include ../Make.options
include ../Project.options

.PHONY: default
default:
	$(MMC) $(MCFLAGS) -m $(PROJ_LIBNAME) $(MLLIBS) $(CLASSPATH)

.PHONY: install
install: $(PROJ_LIBNAME)
	@for lib in $^ ; do \
		$(SUDO) $(MMC) $(MCFLAGS) -m $@ $(MLLIBS) $$lib.install ; \
	done

.PHONY: clean
clean:
	$(MMC) --make $(PROJ_CNAME).clean

.PHONY: realclean
realclean: clean
	$(MMC) --make $(PROJ_CNAME).realclean
	rm -fR Mercury/
	rm -f $(wildcard *.err) $(wildcard *.mh)
	rm -f tags
