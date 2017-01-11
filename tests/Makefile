ESH2TEX=$(realpath ../bin/esh2tex)

all: clean cli minimal standalone strip

clean:
	git clean -Xfd

cli:
	mkdir -p cli
	cd cli; ${ESH2TEX} --init
	make -C cli
	cp cli/main.pdf cli.pdf

minimal:
	${ESH2TEX} minimal.tex
	xelatex --jobname=minimal minimal.esh.tex

standalone:
	${ESH2TEX} --write-preamble
	${ESH2TEX} --standalone standalone.py
	xelatex standalone.tex

strip:
	for f in *.pdf; do exiftool '-all:all=' -overwrite_original "$$f"; pdf2ps "$$f"; done
	for f in *.ps; do sed --in-place -e '/^%%CreationDate: .*/d' "$$f"; done
	rm *.pdf

.PHONY: all clean cli minimal standalone
