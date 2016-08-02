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
(require 'color)
(require 'subr-x)

;;; Misc utils

(defun esh--normalize-color (color)
  "Return COLOR as a hex string."
  (upcase (if (= (aref color 0) ?#) color
            (apply #'color-rgb-to-hex (color-name-to-rgb color)))))

(defun esh--filter-cdr (val alist)
  "Remove conses in ALIST whose `cdr' is VAL."
  (seq-filter (lambda (pair) (not (eq (cdr pair) val))) alist))

(defun esh--extract-props (props str)
  (seq-map (lambda (prop) (cons prop (get-text-property 0 prop str))) props))

(defun esh--font-for-char (char)
  "Compute which font displays CHAR."
  (car (internal-char-font nil char)))

;;; Segmenting a buffer

;; FIXME this splits by properties including overlays, but then it ignores them

(defun esh--buffer-ranges-from (start prop)
  "Create a stream of buffer ranges from START.
Ranges are pairs of START..END positions in which all characters
have the same value of PROP or, if PROP is nil, of all
properties."
  (let ((end (if prop (next-single-char-property-change start prop)
               (next-char-property-change start))))
    (if (< start end)
        (stream-cons (cons start end) (esh--buffer-ranges-from end prop))
      (stream-empty))))

(defun esh--buffer-spans (&optional prop)
  "Create a stream of buffer spans.
Buffer spans are ranges of text in which all characters have the
same value of PROP or, if PROP is nil, of all properties."
  (seq-map (lambda (pair)
             (buffer-substring (car pair) (cdr pair)))
           (esh--buffer-ranges-from 1 prop)))

;;; Merging faces

(defun esh--face-get (face attribute)
  "Read ATTRIBUTE from (potentially anonymous) FACE.
Does not take inheritance into account."
  (cond ((facep face)
         (face-attribute face attribute))
        ((listp face)
         (if (plist-member face attribute)
             (plist-get face attribute)
           'unspecified))))

(defun esh--single-face-attribute (face attribute)
  "Read ATTRIBUTE from (potentially anonymous) FACE.
Takes inheritance into account."
  (let* ((attr (esh--face-get face attribute))
         (rel-p (face-attribute-relative-p attribute attr))
         (inherit (esh--face-get face :inherit)))
    (if (and rel-p (or (listp inherit) (facep inherit)))
        (let ((merge-with (esh--single-face-attribute inherit attribute)))
          (merge-face-attribute attribute attr merge-with))
      attr)))

(defun esh--faces-attribute (faces attribute)
  "Read ATTRIBUTE from FACES.
Faces is a list of (possibly anonymous) faces."
  (let ((attr 'unspecified))
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

;;; Removing composition

(defun esh--commit-compositions ()
  "Apply compositions in current buffer.
This replaced each composed string by its composition, forgetting
the original string."
  (let ((composed-ranges (esh--buffer-ranges-from 1 'composition)))
    (seq-doseq (pair (seq-reverse (seq-into-sequence composed-ranges)))
      (pcase pair
        (`(,from . ,to)
         (pcase (get-text-property from 'composition)
           ((or `((,_ . ,char)) `(,_ ,_ [,char]))
            (goto-char from)
            (let ((props (text-properties-at from)))
              (delete-region from to)
              (insert char)
              (set-text-properties from (1+ from) props)
              (remove-text-properties from (1+ from) '(composition))))))))))

;;; Producing LaTeX

(defvar esh--latex-props '(display))
(defvar esh--latex-face-attrs '(:foreground :weight :slant)) ;; FIXME :underline

(defvar esh--latex-substitutions '(("\\\\" . "\\\\textbackslash\0")
                                ("\n" . "\\\\\\\\\n")
                                ("%" . "\\\\%")
                                ("&" . "\\\\&")
                                ("{" . "\\\\{")
                                ("}" . "\\\\}")
                                ("_" . "\\\\_")
                                ("#" . "\\\\#")
                                (" " . "\\\\-~")
                                ("\0" . "{}")))

(defun esh--wrap-symbols (str)
  "Wrap characters of STR that use a fallback font in \\SpecialChar{}."
  (let ((ref-fonts (list nil (esh--font-for-char ?a))))
    (seq-mapcat (lambda (chr)
                  (if (memq (esh--font-for-char chr) ref-fonts)
                      (char-to-string chr)
                    (format "\\SpecialChar{%c}" chr)))
                str 'string)))

(defun esh--escape-for-latex (str)
  "Escape LaTeX special characters in STR."
  (setq str (substring-no-properties str))
  (pcase-dolist (`(,from . ,to) esh--latex-substitutions)
    (setq str (replace-regexp-in-string from to str t)))
  (esh--wrap-symbols str))

(defun esh--latexify-1 (span)
  "Render SPAN as a LaTeX string."
  (let ((latex-str (esh--escape-for-latex span))
        (props-alist (esh--extract-props esh--latex-props span))
        (attrs-alist (esh--extract-face-attributes esh--latex-face-attrs span)))
    (pcase-dolist (`(,attribute . ,val) (esh--filter-cdr 'unspecified attrs-alist))
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
                          "\\textbf{%s}")
                         (_ (error "Unexpected weight %S" val)))
                       latex-str))
              (:slant
               (format (pcase val
                         (`italic "\\textit{%s}")
                         (`oblique "\\textsl{%s}")
                         ((or `normal `roman) "\textrm{%s}")
                         (_ (error "Unexpected slant %S" val)))
                       latex-str))
              (:underline
               (format (pcase val
                         (`t "\\uline{%s}")
                         (_ (error "Unexpected underline %S" val)))
                       latex-str))
              (_ (error "Unexpected attribute %S" attribute)))))
    (pcase-dolist (`(,property . ,val) (esh--filter-cdr nil props-alist))
      (setq latex-str
            (pcase property
              (`display
               (format (pcase val
                         (`(raise ,amount) ;; FIXME raisebox?
                          (if (> amount 0) "\\textsuperscript{%s}"
                            "\\textsubscript{%s}"))
                         (_ (error "Unexpected display property %S" val)))
                       latex-str))
              (_ (error "Unexpected property %S" property)))))
    latex-str))

(defun esh--latexify-current-buffer ()
  "Export current buffer to LaTeX."
  (esh--commit-compositions)
  (string-join (seq-into-sequence (seq-map #'esh--latexify-1 (esh--buffer-spans)))))

(defun esh--latexify-in-buffer (str buffer)
  "Insert STR in BUFFER, fontify it, and latexify it."
  (with-current-buffer buffer
    (erase-buffer)
    (insert str)
    (font-lock-ensure)
    (esh--latexify-current-buffer)))

(defmacro esh--make-temp-buffer (mode buffers)
  "Get temp buffer for MODE from BUFFERS.
If no such buffer exist, create one and add it to BUFFERS."
  `(-if-let* ((buf (alist-get ,mode ,buffers))) buf
     (push (cons ,mode (with-current-buffer (generate-new-buffer " *temp*")
                         (funcall ,mode)
                         (current-buffer)))
           ,buffers)
     (cdar ,buffers)))

(defun esh-latexify ()
  "Fontify contents of all esh environments.
More precisely, find all pairs of \\begin{ENV}..\\end{env},
fontify their contents according to Coq mode, and insert the
corresponding LaTeX code."
  (interactive)
  (let* ((env-start "^\\s-*\\\\begin{HighlightWithEmacs}\\[\\([-a-z]+\\)\\]\\s-*\n")
         (env-end "\n\\s-*\\\\end{HighlightWithEmacs}\\s-*$")
         (temp-buffers nil))
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward env-start nil t)
            (let ((code-start (match-end 0))
                  (mode-function (intern (match-string 1))))
              (re-search-forward env-end)
              (let* ((code-end (match-beginning 0))
                     (code (buffer-substring-no-properties code-start code-end))
                     (temp-buffer (esh--make-temp-buffer mode-function temp-buffers)))
                (goto-char code-start)
                (delete-region code-start code-end)
                (insert (esh--latexify-in-buffer code temp-buffer))))))
      (seq-map (lambda (p) (kill-buffer (cdr p))) temp-buffers))))

(defun esh-latexify-file (path)
  "Fontify contents of all esh environments in PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (esh-latexify)
    (buffer-string)))

(defvar esh--server-frame nil
  "Global variable holding the invisible ESH frame.")

(provide 'esh)
;;; esh.el ends here
