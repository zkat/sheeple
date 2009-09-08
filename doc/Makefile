# This file has been placed in the public domain.

# The code here is heavily adapted from the Makefile
# used to build the documentation for SLIME.

THING = sheeple

TEXI = $(THING).texinfo logo.png

default: pdf htmldir

all: dvi ps info html htmldir tgz pdf

dvi ps info html tgz pdf: %: $(THING).%

$(THING).dvi: $(TEXI) logo.eps
	texi2dvi --build-dir=.dvi $<

$(THING).ps: $(THING).dvi
	dvips -o $@ $<

$(THING).info: $(TEXI)
	makeinfo $<

$(THING).html: $(TEXI)
	texi2html $<

htmldir: $(TEXI)
	makeinfo -o sheeple --html $<
	cp logo.png sheeple

$(THING).tgz: htmldir
	tar -czf $@ sheeple

$(THING).pdf: $(TEXI)
	texi2pdf --build-dir=.pdf $<

logo.eps: logo.png
	convert logo.png logo.eps

clean:
	rm -rf .{pdf,dvi} $(THING){,.{info,pdf,dvi,ps,html,tgz}} logo.eps
