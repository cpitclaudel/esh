;;; Use a palette suitable for red/green color-blind users.
;; The color palette is from B. Wong, Nature Methods 8, 441 (2011).
;; It is intended to provide good variability while being easily
;; differentiated by individuals with protanopia or deuteranopia.
(load-theme 'dichromacy t)

;;; Define a variant of Emacs-Lisp-mode that prettifies symbols
(defun pretty-emacs-lisp-mode ()
  (emacs-lisp-mode)
  (setq-local prettify-symbols-alist '(("<=" . ?≤)
                                       ("or" . ?∨)
                                       ("/+/" . ?⊕)
                                       ("lambda" . ?λ)
                                       ("approx=" . ?≈)))
  (prettify-symbols-mode))

;;; Uncomment this if you use custom fonts and/or Unicode symbols
;; (set-face-attribute 'default nil :family "Ubuntu Mono")
;; (dolist (ft (fontset-list))
;;   (set-fontset-font ft 'unicode (font-spec :name "Ubuntu Mono"))
;;   (set-fontset-font ft 'unicode (font-spec :name "Symbola monospacified for Ubuntu Mono") nil 'append))

;;; For the following to work, you'll need to modify the Makefile to use the
;;; --cask option, and run ``cask install`` in this directory.

;;; Uncomment this to load Proof General and company-coq
;;
;; (setq-default proof-splash-seen t)
;; (load-file "~/.emacs.d/lisp/PG/generic/proof-site.el")
;;
;; (require 'company-coq)
;; (add-hook 'coq-mode-hook #'company-coq-mode)

;;; Uncomment this to load Ur/Web-mode
;;
;; (defun my-urweb-setup ()
;;   (setq-local prettify-symbols-alist '(("::" . ?∷) ("=>" . ?⇒)))
;;   (prettify-symbols-mode))
;;
;; (add-hook 'urweb-mode-hook #'my-urweb-setup)
