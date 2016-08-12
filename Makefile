EMACS ?= emacs
CASK = env --unset INSIDE_EMACS EMACS=$(EMACS) cask

default: elc

pkg-file:
	$(CASK) pkg-file

update:
	$(CASK) install
	$(CASK) update

clean-elc:
	$(CASK) clean-elc

elc: update clean-elc pkg-file
	$(CASK) build
