# This file has been placed in the public domain.

# This code is adapted from the Makefile for the SLIME docs.

THING = sheeple

TEXI = $(THING).texinfo

default: pdf $(THING)/

all: dvi ps info html $(THING)/ tgz pdf

dvi ps info html tgz pdf: %: $(THING).%

$(THING).dvi: $(TEXI) logo.eps
	texi2dvi --build-dir=.dvi $<

$(THING).ps: $(THING).dvi
	dvips -o $@ $<

$(THING).info: $(TEXI) logo.png
	makeinfo $<

$(THING).html: $(TEXI) logo.svg
	texi2html $<

$(THING)/: $(TEXI) logo.png
	makeinfo -o $@ --html $<
	cp logo.png $@

$(THING).tgz: $(THING)/
	tar -czf $@ $<

$(THING).pdf: $(TEXI) logo.png
	texi2pdf --build-dir=.pdf $<

logo.png logo.eps: logo.svg
	convert $< -trim $@

clean:
	rm -rf $(THING){,.{info,pdf,dvi,ps,html,tgz}} logo.{eps,png}

fullclean: clean
	rm -rf .{pdf,dvi}