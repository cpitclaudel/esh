EMACS ?= emacs
CASK = env --unset INSIDE_EMACS EMACS=$(EMACS) cask

.PHONY: example

default: elc

pkg-file:
	$(CASK) pkg-file

update:
	$(CASK) install
	$(CASK) update

clean: clean-elc

clean-elc:
	$(CASK) clean-elc

elc: update clean-elc pkg-file
	$(CASK) build

test:
	make -C tests

reference:
	make -C example reference
