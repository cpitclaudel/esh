# %.esh.tex: %.tex
# 	../bin/esh2tex $< > $@

all: xelatex

xelatex: example.tex
	../bin/esh2tex example.tex > example.esh.tex
	xelatex -jobname=example example.esh.tex

pdflatex: example.tex
	env PDFLATEX=true ../bin/esh2tex example.tex > example.esh.tex
	pdflatex -jobname=example example.esh.tex

dep:
	if [ -f
	cask install
	cask update

clean:
	git clean -Xf

reference: example.pdf
	cp example.pdf example.reference.pdf
