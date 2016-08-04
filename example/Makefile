%.esh.tex: %.tex
	../bin/esh2tex $< > $@

%.pdf: %.esh.tex
	xelatex -jobname=$* $<

all: example.pdf
	gnome-open example.pdf

reference: example.pdf
	cp example.pdf example.reference.pdf

clean:
	rm -f example.pdf example.esh.tex
