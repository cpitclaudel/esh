(load-theme 'tango t)

(when (functionp 'set-fontset-font)
  (set-face-attribute 'default nil :family "Ubuntu Mono")
  (dolist (ft (fontset-list))
    (set-fontset-font ft 'unicode (font-spec :name "Ubuntu Mono"))
    (set-fontset-font ft 'unicode (font-spec :name "Symbola monospacified for Ubuntu Mono") nil 'append)))

(setq-default proof-splash-seen t)
(load-file "~/.emacs.d/lisp/PG/generic/proof-site.el")

(require 'company-coq)
(add-hook 'coq-mode-hook #'company-coq-mode)
