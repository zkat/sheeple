# This file has been placed in the public domain.

# The code here is heavily adapted from the Makefile
# used to build the documentation for SLIME.

THING = sheeple

TEXI = $(THING).texinfo

default: pdf htmldir

all: dvi ps info html htmldir tgz pdf

dvi ps info html tgz pdf: %: $(THING).%

$(THING).dvi: $(TEXI)
	texi2dvi --build-dir=.dvi $<

$(THING).ps: $(THING).dvi
	dvips -o $@ $<

$(THING).info: $(TEXI)
	makeinfo $<

$(THING).html: $(TEXI)
	texi2html $<

$(THING).tgz: htmldir
	tar -czf $@ sheeple

htmldir: $(TEXI)
	makeinfo -o sheeple --html $<

$(THING).pdf: $(TEXI)
	texi2pdf --build-dir=.pdf $<

clean:
	rm -rf .{pdf,dvi} $(THING){,.{info,pdf,dvi,ps,html,tgz}}
