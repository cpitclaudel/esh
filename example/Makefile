CASK := $(shell command -v cask 2> /dev/null)
ARGS ?=

.PHONY: all xelatex pdflatex dep clean reference

all: xelatex

html: dep example.tex
	../bin/esh2html --debug-on-error ${ARGS} example.html

xelatex: dep example.tex
	../bin/esh2tex --debug-on-error ${ARGS} example.tex
	xelatex -jobname=example example.esh.tex

pdflatex: dep example.tex
	env ESH_PDFLATEX=true ../bin/esh2tex --debug-on-error ${ARGS} example.tex
	pdflatex -jobname=example example.esh.tex

dep:
ifndef CASK
	$(warning >> Cask not found; some languages will not be available <<)\
	$(warning > You can install Cask from https://github.com/cask/cask <)
else
	cask install
	cask update
endif

clean:
	git clean -Xf

reference: xelatex
	cp example.pdf reference.pdf
