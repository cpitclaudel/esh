CASK := $(shell command -v cask 2> /dev/null)

.PHONY: dep default clean git-clean

%.esh.tex: %.tex esh-init.el Cask
	${ESH2TEX} "$<"

# %.pdf: %.esh.tex
# 	rubber --module xelatex --jobname="$*" "$<"

%.pdf: %.esh.tex
	xelatex -jobname="$*" "$<"

default: main.pdf

dep:
ifndef CASK
    $(error >> Cask not found; You can install Cask from https://github.com/cask/cask <<)
else
	cask install
	cask update
endif

clean:
	rm -f *.esh.*

git-clean:
	git clean -Xf
