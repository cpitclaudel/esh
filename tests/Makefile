ESH2TEX="$(realpath ../bin/esh2tex)" --debug-on-error

all: clean cli minimal misc tricky pv standalone lua reference strip

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

misc:
	${ESH2TEX} misc.tex
	xelatex --jobname=misc misc.esh.tex

tricky:
	cd tricky; ${ESH2TEX} main.tex
	cd tricky; xelatex --jobname=main main.esh.tex
	mv tricky/main.pdf ./tricky.pdf

pv:
	${ESH2TEX} --write-preamble
	${ESH2TEX} --precompute-verbs-map pv.tex
	xelatex pv-incomplete.tex
	xelatex pv.tex

standalone:
	${ESH2TEX} --write-preamble
	${ESH2TEX} --standalone standalone.py
	xelatex standalone.tex

lua:
	cd lua; ${ESH2TEX} lualatex.tex
	cd lua; lualatex --jobname=lualatex lualatex.esh.tex
	mv lua/lualatex.pdf ./lualatex.pdf

reference:
	make -C ../example/ reference
	cp ../example/reference.pdf reference.pdf

strip:
	for f in *.pdf; do exiftool '-all:all=' -overwrite_original "$$f"; pdf2ps "$$f"; done
	for f in *.ps; do sed --in-place -e '/^%%CreationDate: .*/d' "$$f"; done
	rm *.pdf

.PHONY: all clean cli lua minimal misc tricky standalone reference
