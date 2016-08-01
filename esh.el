;;; esh.el --- Highlight a file using emacs and export the results  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Package-Requires: ((stream "2.2") (seq "2.18") (dash "2.12"))
;; Package-Version: 0.1
;; Keywords: faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'seq)
(require 'stream)
(require 'dash)
(require 'subr-x)

;;; Misc utils

(defun esh--normalize-color (color)
  "Return COLOR as a hex string."
  (if (= (aref color 0) ?#) color
    (apply #'color-rgb-to-hex (color-name-to-rgb color))))

;;; Segmenting a buffer

(defun esh--buffer-spans-from (start)
  "Create a STREAM of buffer spans from START.
Buffer spans are chunks of text in which all characters have the
same properties."
  (let ((end (next-char-property-change start)))
    (when (< start end)
      (stream-cons (buffer-substring start end)
                   (esh--buffer-spans-from end)))))

(defun esh--buffer-spans ()
  "Create a STREAM of buffer spans.
Buffer spans are chunks of text in which all characters have the
same properties."
  (let ((source-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring source-buffer 1 (buffer-size source-buffer))
      (esh--buffer-spans-from 1))))

;;; Merging faces

(defun esh--face-get (face attribute)
  (cond ((facep face)
         (face-attribute face attribute))
        ((listp face)
         (if (plist-member face attribute)
             (plist-get face attribute)
           'unspecified))))

(defun esh--single-face-attribute (face attribute)
  (let* ((attr (esh--face-get face attribute))
         (rel-p (face-attribute-relative-p attribute attr))
         (inherit (esh--face-get face :inherit)))
    (if (and rel-p (or (listp inherit) (facep inherit)))
        (let ((merge-with (esh--single-face-attribute inherit attribute)))
          (merge-face-attribute attribute attr merge-with))
      attr)))

(defun esh--faces-attribute (faces attribute)
  (let ((attr 'unspecified))
    ;; (setq faces (-snoc faces 'default))
    (while (and faces (face-attribute-relative-p attribute attr))
      (let ((merge-with (esh--single-face-attribute (pop faces) attribute)))
        (setq attr (merge-face-attribute attribute attr merge-with))))
    attr))

(defun esh--str-face-attribute (str attribute)
  "Look at STR and find out value of face ATTRIBUTE."
  (let ((face-or-faces (get-text-property 0 'face str)))
    (unless (listp face-or-faces)
      (setq face-or-faces (list face-or-faces)))
    (cons attribute (esh--faces-attribute face-or-faces attribute))))

(defun esh--extract-face-attributes (face-attributes text)
  "Extract FACE-ATTRIBUTES from TEXT."
  (seq-map (apply-partially #'esh--str-face-attribute text)
           face-attributes))

(defun esh--remove-unspecified (alist)
  "Remove conses in ALIST whose `cdr' is `unspecified'."
  (seq-filter (lambda (pair) (not (eq (cdr pair) 'unspecified))) alist))

;; (defun esh--buffer-font-spans (face-attributes)
;;   "Create a stream of cons cells (text . face-attributes).
;; Which face attributes to return is decided by the list
;; FACE-ATTRIBUTES."
;;   (seq-map
;;    (apply-partially #'esh--extract-face-attributes face-attributes)
;;    (esh--buffer-spans)))

;;; Producing LaTeX

(defvar esh--latex-props '(display))
(defvar esh--latex-face-attrs '(:foreground :weight :slant))

(defvar esh--latex-substitutions '(("\\\\" . "\\\\textbackslash\0")
                                ("\n" . "\\\\\\\\\n")
                                ("%" . "\\\\%")
                                ("{" . "\\\\{")
                                ("}" . "\\\\}")
                                ("_" . "\\\\_")
                                ("#" . "\\\\#")
                                (" " . "\\\\-~")
                                ("\0" . "{}")))

(defun esh--escape-for-latex (str)
  "Escape LaTeX special characters in STR."
  (setq str (substring-no-properties str))
  (pcase-dolist (`(,from . ,to) esh--latex-substitutions)
    (setq str (replace-regexp-in-string from to str t)))
  str)

(defun esh--latexify-1 (span)
  "Render SPAN as a LaTeX string."
  (let ((latex-str (esh--escape-for-latex span))
        (props-alist (esh--extract-props esh--latex-props span))
        (attrs-alist (esh--extract-face-attributes esh--latex-face-attrs span)))
    (pcase-dolist (`(,attribute . ,val)
                   (append attrs-alist props-alist))
      (setq latex-str
            (pcase attribute
              (:foreground
               (format "\\textcolor[HTML]{%s}{%s}"
                       (substring (esh--normalize-color val) 1)
                       latex-str))
              (:weight
               (format (pcase val
                         ((or `thin `ultralight `ultra-light `extralight
                              `extra-light `light `book `demilight
                              `semilight `semi-light)
                          "\\textlf{%s}")
                         ((or `normal `medium `regular)
                          "\\textmd{%s}")
                         ((or `demi `demibold `semibold `semi-bold
                              `bold `extrabold `extra-bold `black
                              `ultrabold `ultra-bold)
                          "\\textbf{%s}"))
                       latex-str))
              (:slant
               (format (pcase val
                         (`italic "\\textit{%s}")
                         (`oblique "\\textsl{%s}")
                         ((or `normal `roman) "\textrm{%s}"))
                       latex-str))
              (`display
               (format (pcase val
                         (`(raise ,amount) ;; FIXME raisebox?
                          (if (> amount 0) "\\textsuperscript{%s}"
                            "\\textsubscript{%s}"))
                         (_ (error "Unexpected display property %S" val)))))
              (_ (error "Unexpected attribute %S" attribute)))))
    latex-str))

(defun esh--latexify-current-buffer ()
  "Export current buffer to LaTeX."
  (string-join (seq-into-sequence (seq-map #'esh--latexify-1 (esh--buffer-spans)))))

(defun esh--latexify-in-buffer (str buffer)
  "Insert STR in BUFFER, fontify it, and latexify it."
  (with-current-buffer buffer
    (erase-buffer)
    (insert str)
    (font-lock-ensure)
    (esh--latexify-current-buffer)))

(defun esh-latexify (env)
  "Fontify contents of all ENV environments.
More precisely, find all pairs of \\begin{ENV}..\\end{env},
fontify their contents according to Coq mode, and insert the
corresponding LaTeX code."
  (interactive (list "coq"))
  (setq env (regexp-quote env))
  (let ((env-start (format "^\\s-*\\\\begin{%s}\\s-*\n" env))
        (env-end (format "\n\\s-*\\\\end{%s}\\s-*$" env))
        (temp-buffer (generate-new-buffer " *temp*")))
    (with-current-buffer temp-buffer
      (coq-mode))
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward env-start nil t)
            (let* ((start (match-end 0))
                   (end (progn (re-search-forward env-end)
                               (match-beginning 0)))
                   (str (buffer-substring-no-properties start end))
                   (latexified (esh--latexify-in-buffer str temp-buffer)))
              (goto-char start)
              (delete-region start end)
              (insert latexified))))
      (kill-buffer temp-buffer))))

;; (let ((latex (esh--latexify)))
;;   (with-current-buffer (get-buffer-create "test.tex")
;;     (erase-buffer)
;;     (when (eq major-mode 'fundamental)
;;       (LaTeX-mode))
;;     (insert "\\documentclass{article}

;; \\usepackage{xcolor}
;; \\usepackage{fontspec}

;; \\newfontfamily{\\UbuntuMonoCode}[Mapping=tex-ansi]{Ubuntu Mono}
;; \\newenvironment{emacslisting}{%
;;   \\UbuntuMonoCode
;; }{%
;;   \par
;; }

;; \\begin{document}
;; ")
;;     (insert latex)
;;     (insert "\\end{document}")
;;     (pop-to-buffer (current-buffer))))

;; (seq-map (apply-partially )
;;          (seq-into-sequence (buffer-spans)))

(provide 'esh)
;;; esh.el ends here
