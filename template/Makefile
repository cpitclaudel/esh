CASK := $(shell command -v cask 2> /dev/null)

%.esh.tex: %.tex
	../bin/esh2tex "$<"

%.pdf: %.esh.tex
	rubber --module xelatex --jobname="$@" "$<"

%.pdf: %.esh.tex
	xelatex -jobname="$@" "$<"

all: main.pdf

dep:
ifndef CASK
    $(error >> Cask not found; You can install Cask from https://github.com/cask/cask <<)
else
	cask install
	cask update
endif

clean:
	rm *.esh.*

git-clean:
	git clean -Xf
