;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use a palette suitable for red/green color-blind users. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The color palette is from B. Wong, Nature Methods 8, 441 (2011).
;; It is intended to provide good variability while being easily
;; differentiated by individuals with protanopia or deuteranopia.
(load-theme 'dichromacy t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a variant of Emacs-Lisp-mode that prettifies symbols ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pretty-emacs-lisp-mode ()
  (emacs-lisp-mode)
  (setq-local prettify-symbols-alist '(("<=" . ?≤)
                                       ("or" . ?∨)
                                       ("/+/" . ?⊕)
                                       ("lambda" . ?λ)
                                       ("approx=" . ?≈)))
  (prettify-symbols-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For the following to work, you'll need to modify the Makefile to  ;;
;; use the --cask option, and run ``cask install`` in this directory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See https://github.com/cask/cask for more on managing Emacs packages
;; with Cask; it's a bit like virtualenvs for Python.

;;; Coq

(add-to-list 'load-path "~/.emacs.d/lisp/PG/generic/")
(when (and (require 'proof-site nil t)
           (require 'company-coq nil t))
  (setq-default proof-splash-seen t)
  (add-hook 'coq-mode-hook #'company-coq-mode))

;;; Ur/Web

(when (require 'urweb-mode)
  (defun my-urweb-setup ()
    (setq-local prettify-symbols-alist '(("::" . ?∷) ("=>" . ?⇒)))
    (prettify-symbols-mode))
  (add-hook 'urweb-mode-hook #'my-urweb-setup))

;;; Haskell

(setq-default haskell-font-lock-symbols t)

;;; Racket

(when (require 'racket-mode)
  (defun my-racket-setup ()
    (setq-local prettify-symbols-alist '(("lambda" . ?λ)))
    (prettify-symbols-mode))
  (add-hook 'racket-mode-hook #'my-racket-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uncomment and edit the following to improve handling of fallback ;;
;; fonts (see the manual for more information)                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-face-attribute 'default nil :family "Ubuntu Mono")
;; (dolist (ft (fontset-list))
;;   (set-fontset-font ft 'unicode (font-spec :name "Ubuntu Mono"))
;;   (set-fontset-font ft 'unicode (font-spec :name "Symbola monospacified for Ubuntu Mono") nil 'append))
