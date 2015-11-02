cleaner1=tr '\n' ' ' | tr -s ' ' | sed -e 's/ $$//' -e 's/ / '
cleaner2=' /g'

cairoCFLAGS=-C $(shell pkg-config --cflags cairo | $(cleaner1)-C$(cleaner2))
cairoLIBS=-L $(shell pkg-config --libs cairo | $(cleaner1)-L$(cleaner2))

CSC=csc
CSCCC=$(CSC) $(cairoCFLAGS)
CSCLD=$(CSC) $(cairoCFLAGS) $(cairoLIBS)

TARGETS=\
	cairo.so

EGG_CONTENTS=\
	README \
	COPYING \
	Makefile \
	cairo.scm \
	cairo.setup \
	test-cairo.scm \

all: $(TARGETS)

extension: cairo.so

install: all
	chicken-setup cairo

uninstall:
	chicken-setup -uninstall cairo

egg: cairo.egg

cairo.egg:
	tar -zcvf $@ $(EGG_CONTENTS)

%.so: %.scm
	$(CSCLD) -s -o $@ $<
#	strip $@

clean:
	rm -f $(TARGETS)
	rm -f STACKTRACE
	rm -f cairo.egg
