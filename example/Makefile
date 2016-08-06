# %.esh.tex: %.tex
# 	../bin/esh2tex $< > $@

CASK := $(shell command -v cask 2> /dev/null)

all: xelatex

xelatex: example.tex
	../bin/esh2tex example.tex > example.esh.tex
	xelatex -jobname=example example.esh.tex

pdflatex: example.tex
	env PDFLATEX=true ../bin/esh2tex example.tex > example.esh.tex
	pdflatex -jobname=example example.esh.tex

dep:
ifndef CASK
    $(error "cask: Not found: Please install Cask from https://github.com/cask/cask.")
endif
	cask install
	cask update

clean:
	git clean -Xf

reference: example.pdf
	cp example.pdf example.reference.pdf
