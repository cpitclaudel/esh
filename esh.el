;;; esh.el --- Use Emacs to highlight code snippets in LaTeX documents  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0.1
;; Keywords: faces
;; URL: https://github.com/cpitclaudel/esh2tex

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

;; ESH is a replacement for lstlistings, minted, etc.\ that uses Emacs'
;; major-modes to syntax-highlight code blocks in LaTeX documents.
;;
;; See URL `https://github.com/cpitclaudel/esh2tex' for usage instructions.

;;; Code:

;; Can't depend on anything outside of core Emacs
(require 'color)

(defconst esh--script-full-path
  (or (and load-in-progress load-file-name)
      (bound-and-true-p byte-compile-current-file)
      (buffer-file-name))
  "Full path of this script.")

(defconst esh--directory
  (file-name-directory esh--script-full-path)
  "Full path to directory of this script.")

;;; Misc utils

(defun esh--normalize-color (color)
  "Return COLOR as a hex string."
  (unless (member color '("unspecified-fg" "unspecified-bg" "" nil))
    (let ((color (if (= (aref color 0) ?#) color
                   (apply #'color-rgb-to-hex (color-name-to-rgb color)))))
      (substring (upcase color) 1))))

(defun esh--filter-cdr (val alist)
  "Remove conses in ALIST whose `cdr' is VAL."
  ;; FIXME phase this out once Emacs 25 is everywhere
  (let ((kept nil))
    (while alist
      (let ((top (pop alist)))
        (when (not (eq (cdr top) val))
          (push top kept))))
    (nreverse kept)))

(defun esh--extract-props (props pos)
  "Read PROPS from POS as an ALIST or (PROP . VAL)."
  (mapcar (lambda (prop)
            (cons prop (get-char-property pos prop)))
          props))

(defvar esh-name-to-mode-alist nil
  "Alist of block name → mode function.")

(defun esh-add-language (language mode)
  "Teach ESH about a new LANGUAGE, highlighted with MODE.
For example, calling (esh-add-language \"ocaml\" \\='tuareg-mode)
allows you to use `tuareg-mode' for HTML blocks tagged
“src-ocaml”, or for LaTeX blocks tagged “ESH: ocaml”."
  (unless (stringp language)
    (user-error "`esh-add-language': language %S should be a string" language))
  (unless (symbolp mode)
    (user-error "`esh-add-language': mode %S should be a function" mode))
  (add-to-list 'esh-name-to-mode-alist (cons language mode)))

(defun esh--resolve-mode-fn (fn-name)
  "Translate FN-NAME to a function symbol.
Uses `esh-name-to-mode-alist'."
  (or (cdr (assoc fn-name esh-name-to-mode-alist))
      (intern (concat fn-name "-mode"))))

(defun esh-add-keywords (forms &optional how)
  "Pass FORMS and HOW to `font-lock-add-keywords'.
See `font-lock-keywords' for information about the format of
elements of FORMS.  This function does essentially the same thing
as `font-lock-add-keywords', with nicer indentation, a simpler
call signature, and a workaround for an Emacs bug."
  (declare (indent 0))
  ;; Work around Emacs bug #24176
  (setq font-lock-major-mode major-mode)
  (font-lock-add-keywords nil forms how))

(defun esh--remove-final-newline ()
  "Remove last newline of current buffer, if present."
  (goto-char (point-max))
  ;; There may not be a final newline in standalone mode
  (when (eq (char-before) ?\n)
    (delete-char -1)))

;;; Segmenting a buffer

(defun esh--buffer-ranges-from (start prop)
  "Create a stream of buffer ranges from START.
Ranges are pairs of START..END positions in which all characters
have the same value of PROP or, if PROP is nil, of all
properties."
  (let ((ranges nil)
        (making-progress t))
    (while making-progress
      (let ((end (if prop (next-single-char-property-change start prop)
                   (next-char-property-change start))))
        (if (< start end)
            (push (cons start end) ranges)
          (setq making-progress nil))
        (setq start end)))
    (nreverse ranges)))

(defun esh--buffer-ranges (&optional prop)
  "Create a stream of buffer ranges.
Ranges are pairs of START..END positions in which all characters
have the same value of PROP or, if PROP is nil, of all
properties."
  (esh--buffer-ranges-from 1 prop))

;;; Merging faces

(defun esh--face-get (face attribute)
  "Read ATTRIBUTE from (potentially anonymous) FACE.
Does not take inheritance into account."
  (cond ((facep face)
         (face-attribute face attribute))
        ((listp face)
         (if (plist-member face attribute)
             (plist-get face attribute)
           'unspecified))
        (t (error "Invalid face %S" face))))

(defun esh--single-face-attribute (face attribute)
  "Read ATTRIBUTE from (potentially anonymous) FACE.
Takes inheritance into account."
  (let* ((attr (esh--face-get face attribute))
         (rel-p (face-attribute-relative-p attribute attr))
         (inherit (esh--face-get face :inherit)))
    (if (and rel-p
             (not (eq inherit 'default))
             (or (listp inherit) (facep inherit)))
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

(defun esh--as-list (x)
  "Wrap X in a list, if needed."
  (if (listp x) x (list x)))

(defun esh--pos-face-attribute (pos attribute)
  "Look at POS and find out value of face ATTRIBUTE."
  (esh--faces-attribute (append (esh--as-list (get-text-property pos 'face))
                             (esh--as-list (get-char-property pos 'face)))
                     attribute))

(defun esh--pos-face-attribute-cons (pos attribute)
  "Look at POS and form cons (ATTRIBUTE . ATTRIBUTE-VALUE)."
  (cons attribute (esh--pos-face-attribute pos attribute)))

(defun esh--extract-face-attributes (face-attributes pos)
  "Extract FACE-ATTRIBUTES from POS."
  (mapcar (apply-partially #'esh--pos-face-attribute-cons pos)
          face-attributes))

;;; Massaging properties

(defun esh--commit-compositions ()
  "Apply compositions in current buffer.
This replaced each composed string by its composition, forgetting
the original string."
  (let ((composed-ranges (esh--buffer-ranges 'composition)))
    (dolist (pair (reverse composed-ranges))
      (pcase pair
        (`(,from . ,to)
         (let ((composition (get-char-property from 'composition))
               (char nil))
           (pcase composition
             (`((,_ . ,c))
              (setq char c))
             (`(,_ ,_ ,vc) ;; No support for ,[] QPatterns in 24.5
              (when (and (vectorp vc) (= (length vc) 1))
                (setq char (aref vc 0)))))
           (when char
             (goto-char from)
             (let ((props (text-properties-at from)))
               (delete-region from to)
               (insert char)
               (set-text-properties from (1+ from) props)
               (remove-text-properties from (1+ from) '(composition))))))))))

(defun esh--mark-newlines ()
  "Add a `newline' text property to each \\n character.
The value is either `empty' or `non-empty' (we need this to add a
dummy element on empty lines to prevent LaTeX from complaining
about underful hboxes)."
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (put-text-property (match-beginning 0) (match-end 0) 'newline
                       (cons (point) ;; Prevent collapsing
                             ;; (point-at-bol 0) is beginning of previous line
                             ;; (match-beginning 0) is end of previous line
                             (if (eq (point-at-bol 0) (match-beginning 0))
                                 'empty 'non-empty)))
    ;; Break up boxes spanning more than one line
    (put-text-property (match-beginning 0) (match-end 0) 'face nil)))

(defun esh--mark-boxes ()
  "Replace :box face attributes.
Each buffer span whose face has a :box property is modified so
that its first character has a `begin-box' property, and its last
character has an `end-box' property."
  (goto-char (point-min))
  (let ((prev-start nil)
        (prev-box nil))
    (pcase-dolist (`(,start . ,_) (esh--buffer-ranges))
      (let* ((box-attr (esh--pos-face-attribute start :box))
             (box (unless (eq box-attr 'unspecified) box-attr))
             (box-continues (equal box prev-box)))
        (if box-continues
            (setq start prev-start)
          (when prev-box
            (put-text-property (1- start) start 'esh-end-box prev-box))
          (when box
            (put-text-property start (1+ start) 'esh-begin-box box)))
        (setq prev-box box)
        (setq prev-start start)))
    (when prev-box
      (put-text-property (1- (point-max)) (point-max) 'esh-end-box t))))

;;; Fontifying

(defun esh--font-lock-ensure ()
  "Wrapper around `font-lock-ensure'."
  (if (fboundp 'font-lock-ensure)
      (font-lock-ensure)
    (with-no-warnings (font-lock-fontify-buffer))))

(defconst esh--missing-mode-template
  (concat ">>> (void-function %S); did you forget to `require'"
          " a dependency, or to restart the server? <<<%s"))

(defvar esh--inline nil
  "Dynamic variable indicating the current context.
nil in blocks, t in inline snippets.")

(defun esh--missing-mode-error-msg (mode)
  "Construct an error message about missing MODE.
With non-nil `esh--inline', suffix message with blanks instead of newlines."
  (propertize (format esh--missing-mode-template mode (if esh--inline "  " "\n"))
              'face 'error 'font-lock-face 'error))

(defvar-local esh--temp-buffers nil
  "Alist of (MODE . BUFFER).
These are temporary buffers, used for highlighting.")

(defun esh--kill-temp-buffers ()
  "Kill buffers in `esh--temp-buffers'."
  (mapc #'kill-buffer (mapcar #'cdr esh--temp-buffers))
  (setq esh--temp-buffers nil))

(defun esh--make-temp-buffer (mode)
  "Get temp buffer for MODE from `esh--temp-buffers'.
If no such buffer exist, create one and add it to BUFFERS.  In
all cases, the buffer is erased, and a message is added to it if
the required mode isn't available."
  (save-match-data
    (let ((buf (cdr (assq mode esh--temp-buffers)))
          (mode-boundp (fboundp mode)))
      (unless buf
        (setq buf (generate-new-buffer " *temp*"))
        (with-current-buffer buf
          (funcall (if mode-boundp mode #'fundamental-mode)))
        (push (cons mode buf) esh--temp-buffers))
      (with-current-buffer buf
        (erase-buffer)
        (unless mode-boundp
          (insert (esh--missing-mode-error-msg mode))))
      buf)))

;; (defun esh--prepare-for-export ()
;;   "Prepare current buffer for export.
;; Add final newline, widen, etc."
;;   (widen)
;;   (save-excursion
;;     (goto-char (point-max))
;;     (unless (eq (char-before) ?\n)
;;       (insert "\n"))))

(defvar esh-post-highlight-hook nil) ;; FIXME document this

(defun esh--export-buffer (export-fn)
  "Refontify current buffer, then invoke EXPORT-FN.
EXPORT-FN should do the actual exporting; its return value may
depend on `esh--inline'."
  (esh--font-lock-ensure)
  (run-hook-with-args 'esh-post-highlight-hook)
  (funcall export-fn))

(defun esh--export-str (str mode-fn export-fn)
  "Fontify STR in a MODE-FN buffer, then invoke EXPORT-FN.
EXPORT-FN should do the actual exporting; its return value may
depend on `esh--inline'."
  (with-current-buffer (esh--make-temp-buffer mode-fn)
    (insert str)
    (esh--export-buffer export-fn)))

;;; Producing LaTeX

(defvar esh--latex-props '(display invisible newline esh-begin-box esh-end-box))
(defvar esh--latex-face-attrs '(:underline :background :foreground :weight :slant))

(eval-and-compile
  (defvar esh--latex-specials
    '((?$ . "\\$") (?% . "\\%") (?& . "\\&") (?{ . "\\{") (?} . "\\}") (?_ . "\\_") (?# . "\\#")
      (?` . "{`}") (?' . "{'}") (?< . "{<}") (?> . "{>}") ;; A few ligatures
      (?\\ . "\\textbackslash{}") (?^ . "\\textasciicircum{}") (?~ . "\\textasciitilde{}")
      ;; Explicit replacements instead of catcodes
      ;; Actual replacements are changed using dynamic binding
      (?\s . nil) (?- . nil))))

(defvar esh--latex-specials-re
  (eval-when-compile
    (regexp-opt-charset (mapcar #'car esh--latex-specials))))

(defun esh--latex-substitute-special (m)
  "Get replacement for LaTeX special M."
  ;; If this become slows, use a vector and index by (aref m 0)
  (or (cdr (assq (aref m 0) esh--latex-specials))
      (pcase m
        (" " (if esh--inline "\\ " "~"))
        ("-" (if esh--inline "-" "\\ESHBlockDash{}")))))

(defun esh--latex-substitute-specials (str)
  "Escape LaTeX specials in STR.
Results for “ ” and “-” depend on `esh--inline'."
  (replace-regexp-in-string
   esh--latex-specials-re #'esh--latex-substitute-special str t t))

(defun esh--wrap-non-ascii (str)
  "Wrap non-ASCII characters of STR.
Wraps into \\ESH(Block|Inline)SpecialChar{}."
  ;; TODO benchmark against trivial loop
  (let ((rep (format "\\\\ESH%sSpecialChar{\\&}"
                     (if esh--inline "Inline" "Block"))))
    (replace-regexp-in-string "[^\000-\177]" rep str t)))

(defun esh--mark-non-ascii ()
  "Tag non-ASCII characters of current buffer.
Puts text property `non-ascii' on non-ascii characters."
  (goto-char (point-min))
  (while (re-search-forward "[^\000-\177]" nil t)
    ;; Need property values to be distinct, hence (point)
    (put-text-property (match-beginning 0) (match-end 0) 'non-ascii (point))))

(defun esh--escape-for-latex (str)
  "Escape LaTeX special characters in STR."
  (esh--wrap-non-ascii (esh--latex-substitute-specials str)))

(defun esh--normalize-underline (underline)
  "Normalize UNDERLINE."
  (pcase underline
    (`t '(nil . line))
    ((pred stringp) `(,underline . line))
    ((pred listp) `(,(plist-get underline :color) .
                    ,(or (plist-get underline :style) 'line)))))

(defun esh--normalize-weight (weight)
  "Normalize WEIGHT."
  (pcase weight
    ((or `thin `ultralight `ultra-light) 100)
    ((or `extralight `extra-light) 200)
    ((or `light) 300)
    ((or `demilight `semilight `semi-light `book) 400)
    ((or `normal `medium `regular) 500)
    ((or `demi `demibold `semibold `semi-bold) 600)
    ((or `bold) 700)
    ((or `extrabold `extra-bold `black) 800)
    ((or `ultrabold `ultra-bold) 900)))

(defun esh--normalize-weight-coarse (weight)
  "Normalize WEIGHT to 3 values."
  (let ((weight (esh--normalize-weight weight)))
    (when (numberp weight)
      (cond
       ((< weight 500) 'light)
       ((> weight 500) 'bold)
       (t 'regular)))))

(defun esh--normalize-box (box)
  "Normalize face attribute BOX."
  (pcase box
    (`t `(1 nil nil))
    ((pred stringp) `(1 ,box nil))
    ((pred listp) `(,(or (plist-get box :line-width) 1)
                    ,(plist-get box :color)
                    ,(plist-get box :style)))))

(defun esh--latexify-raise (str amount)
  "Issue a LaTeX command to raise STR by AMOUNT."
  (format "\\ESH%sRaise{%gex}{%s}"
          (if esh--inline "Inline" "Block")
          amount str))

(defun esh--latexify-span (range)
  "Render RANGE as a LaTeX string.
Exact behavior is dependent on value of `esh--inline'."
  (let* ((template "%s")
         (span (buffer-substring-no-properties (car range) (cdr range)))
         (latex-str (esh--escape-for-latex span))
         (props-alist (esh--extract-props esh--latex-props (car range)))
         (attrs-alist (esh--extract-face-attributes esh--latex-face-attrs (car range))))
    (pcase-dolist (`(,attribute . ,val) (esh--filter-cdr 'unspecified attrs-alist))
      (when val
        (setq template
              (pcase attribute
                (:background
                 (setq val (esh--normalize-color val))
                 ;; FIXME: Force all lines to have the the same height?
                 ;; Could use \\vphantom{'g}\\smash{…}
                 (if val (format "\\colorbox[HTML]{%s}{%s}" val template)
                   template))
                (:foreground
                 (setq val (esh--normalize-color val))
                 (if val (format "\\textcolor[HTML]{%s}{%s}" val template)
                   template))
                (:weight
                 (format (pcase (esh--normalize-weight-coarse val)
                           (`light "\\textlf{%s}")
                           (`regular "\\textmd{%s}")
                           (`bold "\\textbf{%s}")
                           (_ (error "Unexpected weight %S" val)))
                         template))
                (:slant
                 (format (pcase val
                           (`italic "\\textit{%s}")
                           (`oblique "\\textsl{%s}")
                           (`normal "\\textup{%s}")
                           (_ (error "Unexpected slant %S" val)))
                         template))
                (:underline
                 (pcase (esh--normalize-underline val)
                   (`(,color . ,type)
                    (setq color (esh--normalize-color color))
                    ;; There are subtle spacing issues with \\ESHUnder, so don't
                    ;; use it unless the underline needs to be colored.
                    (let* ((prefix (if color "\\ESHUnder" "\\u"))
                           (command (format "%s%S" prefix type))
                           (color-arg (if color (format "{\\color[HTML]{%s}}" color) "")))
                      (format "%s%s{%s}" command color-arg template)))
                   (_ (error "Unexpected underline %S" val))))
                (_ (error "Unexpected attribute %S" attribute))))))
    (pcase-dolist (`(,property . ,val) (esh--filter-cdr nil props-alist))
      (pcase property
        (`display
         (setq template
               (pcase val
                 (`(raise ,amount)
                  (esh--latexify-raise template amount))
                 (_ (error "Unexpected display property %S" val)))))
        (`invisible
         ;; FIXME remove template too?
         (when val (setq latex-str "")))
        (`esh-begin-box
         (pcase (esh--normalize-box val)
           (`(,line-width ,color ,style)
            (setq color (esh--normalize-color color))
            (unless (eq style nil)
              (error "Unsupported box style %S" style))
            (setq template
                  (format "\\ESHBox{%s}{%gpt}{%s"
                          (or color ".") (abs line-width) template)))
           (_ (error "Unexpected box %S" val))))
        (`esh-end-box
         (setq template (concat template "}")))
        (`newline
         ;; Ensure that no newlines are added inside commands (instead the
         ;; newline is added to the end of the template), and add an mbox to
         ;; prevent TeX from complaining about underfull boxes.
         (setq latex-str "")
         (setq val (cdr val))
         (let ((mbox (pcase val (`empty "\\mbox{}"))))
           (setq template (concat template mbox "\n"))))
        (_ (error "Unexpected property %S" property))))
    (format template latex-str)))

(defun esh--latexify-protect-bols (str)
  "Prefix each line of STR with a discretionary hyphen.
This used to only be needed for lines starting with whitespace,
but leading dashes also behave strangely due to the \\hbox in
\\ESHObeySpaces; given this, it's simpler (and safer) to prefix
all lines."
  (replace-regexp-in-string "^" "\\-" str t t))

(defun esh--latexify-obeylines (str)
  "Suffix each line of STR with a \\par.
Do this instead of using catcodes, for robustness."
  (replace-regexp-in-string "\n" "\\par\n" str t t))

(defun esh--latexify-current-buffer ()
  "Export current buffer to LaTeX.
With non-nil `esh--inline', protect beginnings of lines
and add “\\par”s."
  (esh--remove-final-newline)
  (esh--commit-compositions)
  (esh--mark-newlines)
  (esh--mark-boxes)
  (let* ((str (mapconcat #'esh--latexify-span (esh--buffer-ranges) "")))
    (if esh--inline str
      (esh--latexify-obeylines (esh--latexify-protect-bols str)))))

(defvar esh-latexify-block-envs
  `(("^[ \t]*%%[ \t]*ESH: \\([^ \t\n]+\\)[ \t]*\n[ \t]*\\\\begin{\\([^}]+\\)}.*\n" .
     ,(lambda () (concat "^[ \t]*\\\\end{" (match-string 2) "}"))))
  "Alist of replaceable environments.")

(defun esh--latexify-inline-verb-matcher (re)
  "Search for a \\verb-like delimiter from point.
That is, a match of the form RE?...? where ? is any
character."
  (when (and re (re-search-forward re nil t))
    (let ((form-beg (match-beginning 0))
          (command (match-string 0))
          (delimiter (char-after))
          (code-beg (1+ (point))))
      (unless delimiter
        (error "No delimiter found after use of `%s'" command))
      (goto-char code-beg)
      (if (search-forward (char-to-string delimiter) (point-at-eol) t)
          (list form-beg (point) code-beg (1- (point)) command)
        (error "No matching delimiter found after use of `%s%s'"
               command delimiter)))))

(defun esh--latexify-beginning-of-document ()
  "Go past \\begin{document}."
  (goto-char (point-min))
  (unless (search-forward "\\begin{document}" nil t)
    (goto-char (point-min))))

(defvar esh-latex-inline-macro-alist nil
  "Alist of inline ESH marker → mode function.

This list maps inline verb-like markers to modes.  For example,
it could contain (\"@ocaml \\\\verb\" . tuareg-mode) to recognize
all instances of “@ocaml \\verb|...|” as OCaml code to be
highlighted with `tuareg-mode'.  This list is ignored in HTML
mode.  See the manual for more information.")

(defun esh-latex-add-inline-macro (macro mode)
  "Teach ESH about an inline MACRO, highlighted with MODE.
For example (esh-latex-add-inline-marker \"\\\\ocaml\" \\='tuareg-mode)
recognizes all instances of “\\ocaml|...|” as OCaml code to be
highlighted with `tuareg-mode'."
  (add-to-list 'esh-latex-inline-macro-alist (cons macro mode)))

(defconst esh--latexify-inline-template "\\ESHInline{%s}")
(defconst esh--latexify-block-template "\\begin{ESHBlock}\n%s\n\\end{ESHBlock}")

(defun esh--latexify-do-inline-envs ()
  "Latexify sources in ESH inline environments."
  (let* ((modes-alist esh-latex-inline-macro-alist)
         (envs-re (when modes-alist (regexp-opt (mapcar #'car modes-alist)))))
    (esh--latexify-beginning-of-document)
    (let ((match-info nil))
      (while (setq match-info (esh--latexify-inline-verb-matcher envs-re))
        (pcase match-info
          (`(,beg ,end ,code-beg ,code-end ,cmd)
           (let* ((esh--inline t)
                  (mode-fn (cdr (assoc cmd modes-alist)))
                  (code (buffer-substring-no-properties code-beg code-end))
                  (tex (esh--export-str code mode-fn #'esh--latexify-current-buffer)))
             (goto-char beg)
             (delete-region beg end)
             (insert (format esh--latexify-inline-template tex)))))))))

(defun esh--latexify-do-block-envs ()
  "Latexify sources in esh block environments."
  (pcase-dolist (`(,start-marker . ,end-marker-function) esh-latexify-block-envs)
    (goto-char (point-min))
    (while (re-search-forward start-marker nil t)
      (when (string= "" (match-string-no-properties 1))
        (error "Invalid ESH header: %S" (match-string-no-properties 0)))
      (let* ((esh--inline nil)
             (block-start (match-beginning 0))
             (code-start (match-end 0))
             (mode-fn (esh--resolve-mode-fn (match-string-no-properties 1))))
        ;; FIXME why not use a plain regexp here?
        (re-search-forward (funcall end-marker-function))
        (let* ((code-end (match-beginning 0))
               (block-end (match-end 0))
               (code (buffer-substring-no-properties code-start code-end))
               (tex (esh--export-str code mode-fn #'esh--latexify-current-buffer)))
          (goto-char block-start)
          (delete-region block-start block-end)
          (insert (format esh--latexify-block-template tex)))))))

(defun esh2tex-current-buffer ()
  "Fontify contents of all ESH environments.
Latexify sources in environments delimited by
`esh-latexify-block-envs' and user-defined inline groups."
  (interactive "P")
  (save-excursion
    (unwind-protect
        (progn
          (esh--latexify-do-inline-envs)
          (esh--latexify-do-block-envs))
      (esh--kill-temp-buffers))))

(defun esh2tex-tex-file (path)
  "Fontify contents of all ESH environments in PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (esh2tex-current-buffer)
    (buffer-string)))

(defun esh--latexify-wrap (code)
  "Wrap CODE in ESHInline or ESHBlock."
  (format (if esh--inline
              esh--latexify-inline-template
            esh--latexify-block-template)
          code))

(defun esh2tex-source-file (source-path)
  "Fontify contents of SOURCE-PATH.
Return result as a LaTeX string.  Non-nil INLINE specifies that
the resulting TeX code should be wrapped in an ESHInline macro
instead of an ESHBlock environment."
  (let ((esh--inline (string-match-p "esh-inline" source-path)))
    (with-temp-buffer
      (insert-file-contents source-path)
      (set-visited-file-name source-path t)
      (set-auto-mode)
      (prog1 (esh--latexify-wrap (esh--export-buffer #'esh--latexify-current-buffer))
        (set-buffer-modified-p nil)))))

;;; Producing HTML

(defconst esh--html-specials '((?< . "&lt;")
                            (?> . "&gt;")
                            (?& . "&amp;")
                            (?\" . "&quot;")))

(defconst esh--html-specials-re
  (regexp-opt-charset (mapcar #'car esh--html-specials)))

(defun esh--html-substitute-special (m)
  "Get replacement for HTML special M."
  (cdr (assq (aref m 0) esh--html-specials)))

(defun esh--html-substitute-specials (str)
  "Escape HTML specials in STR."
  (replace-regexp-in-string
   esh--html-specials-re #'esh--html-substitute-special str t t))

(defconst esh--html-void-tags '(area base br col embed hr img input
                                  link menuitem meta param source track wbr))

(defun esh--htmlify-serialize (node escape-specials)
  "Write NODE as HTML string to current buffer.
With non-nil ESCAPE-SPECIALS, quote special HTML characters in
NODE's body.  If ESCAPE-SPECIALS is nil, NODE must be a string."
  (pcase node
    ((pred stringp)
     (insert (if escape-specials
                 (esh--html-substitute-specials node)
               node)))
    (`(comment nil ,comment)
     ;; "--" isn't allowed in comments, so no need for escaping
     (insert "<!--" comment "-->"))
    (`(,tag ,attributes . ,children)
     (unless escape-specials
       (error "Must escape specials in %S" escape-specials))
     (let ((tag-name (symbol-name tag))
           (escape-specials (not (memq tag '(script style)))))
       (insert "<" tag-name)
       (pcase-dolist (`(,attr . ,val) attributes)
         (insert " " (symbol-name attr) "=\"")
         (esh--htmlify-serialize val escape-specials)
         (insert "\""))
       (if (memq tag esh--html-void-tags)
           (insert " />")
         (insert ">")
         (dolist (c children)
           (esh--htmlify-serialize c escape-specials))
         (insert "</" tag-name ">"))))
    (_ (error "Unprintable node %S" node))))

(defvar esh--html-props '(display invisible non-ascii newline))
(defvar esh--html-face-attrs '(:underline :background :foreground :weight :slant))

(defun esh--htmlify-span (range)
  "Render RANGE as an HTML tree."
  (let ((styles nil)
        (raised nil)
        (non-ascii nil)
        (text (buffer-substring-no-properties (car range) (cdr range)))
        (props-alist (esh--extract-props esh--html-props (car range)))
        (attrs-alist (esh--extract-face-attributes esh--html-face-attrs (car range))))
    (pcase-dolist (`(,attribute . ,val) (esh--filter-cdr 'unspecified attrs-alist))
      (when val
        (pcase attribute
          (:foreground
           (when (setq val (esh--normalize-color val))
             (push (concat "color: #" val) styles)))
          (:background
           (when (setq val (esh--normalize-color val))
             (push (concat "background-color: #" val) styles)))
          (:weight
           (if (setq val (esh--normalize-weight val))
               (push (format "font-weight: %d" val) styles)
             (error "Unexpected weight %S" val)))
          (:slant
           (if (memq val '(italic oblique normal))
               (push (concat "font-style: " (symbol-name val)) styles)
             (error "Unexpected slant %S" val)))
          (:underline
           (pcase (esh--normalize-underline val)
             (`(,color . ,type)
              (push "text-decoration: underline" styles)
              (when (eq type 'wave)
                (push "text-decoration-style: wavy" styles))
              (when (setq color (esh--normalize-color color))
                (push (concat "text-decoration-color: #" color) styles)))
             (_ (error "Unexpected underline %S" val))))
          (_ (error "Unexpected attribute %S" attribute)))))
    (pcase-dolist (`(,property . ,val) (esh--filter-cdr nil props-alist))
      (pcase property
        (`display
         (pcase val
           (`(raise ,amount)
            (setq raised t)
            (push (format "bottom: %gem" amount) styles))
           (_ (error "Unexpected display property %S" val))))
        (`invisible
         (when val (setq text "")))
        (`non-ascii
         (when val (setq non-ascii t)))
        (`newline
         ;; Ensure that no newlines are added inside commands.
         ;; FIXME handle background colors extending past end of line
         (setq styles nil))
        (_ (error "Unexpected property %S" property))))
    (cond
     ((equal text "") "")
     ((and (null styles) (not non-ascii)) text)
     (t
      (let ((attrs (when styles
                     `((style . ,(mapconcat #'identity styles ";"))))))
        (setq text `(span ,attrs ,text)))
      (when non-ascii ;; Need nested divs to align wide character properly
        (setq text `(span ((class . "non-ascii")) (span nil ,text))))
      (when raised
        (setq text `(span ((class . "raised"))
                          (span ((class . "raised-text")) ,text)
                          (span ((class . "raised-phantom")) ,text))))
      text))))

(defun esh--htmlify-current-buffer ()
  "Export current buffer to HTML."
  (esh--commit-compositions)
  (esh--mark-newlines)
  (esh--mark-non-ascii)
  (mapcar #'esh--htmlify-span (esh--buffer-ranges)))

(defvar esh--html-src-class-prefix "src-"
  "HTML class prefix indicating a fontifiable tag.")

(defvar esh--html-src-class-re nil
  "Regexp matching classes of tags to be processed by ESH.
Dynamically set.")

(defvar esh-html-default-languages-alist nil
  "Alist of tag → language string.
For example, (code . \"emacs-lisp\") would highlight all `code'
tags with no ESH attribute as Emacs Lisp.")

(defun esh--htmlify-guess-lang (tag attributes)
  "Guess highlighting language based on TAG and ATTRIBUTES."
  (or (let ((class (cdr (assq 'class attributes))))
        (when (and class (string-match esh--html-src-class-re class))
          (match-string 1 class)))
      (cdr (assq tag esh-html-default-languages-alist))))

(defun esh--htmlify-do-tree (node)
  "Highlight code in annotated descendants of NODE."
  (pcase node
    ((pred stringp) node)
    (`(,tag ,attributes . ,children)
     (let ((lang (esh--htmlify-guess-lang tag attributes)))
       (if lang
           (let* ((mode-fn (esh--resolve-mode-fn lang))
                  (code (car children)))
             (unless (and (stringp code) (null (cdr children)))
               (error "Code block has children: %S" node))
             `(,tag ,attributes
                    ,@(esh--export-str code mode-fn #'esh--htmlify-current-buffer)))
         `(,tag ,attributes
                ,@(mapcar (lambda (c)
                            (esh--htmlify-do-tree c))
                          children)))))))

(defvar esh-html-before-parse-hook nil
  "Hook called before parsing input HTML.
Hook may e.g. make modifications to the buffer.")

(defun esh--html-read-tag (tag)
  "Read HTML TAG at point in current buffer."
  (when (looking-at (format "<%s [^>]+>" (regexp-quote tag)))
    (goto-char (match-end 0))
    (skip-chars-forward " \n\t")
    (buffer-substring-no-properties (match-beginning 0) (point))))

(defun esh2html-current-buffer ()
  "Fontify contents of all ESH blocks in current document.
Highlight sources in any environments containing a class matching
`esh--html-src-class-prefix', such as `src-c', `src-ocaml', etc."
  (interactive)
  (run-hook-with-args 'esh-html-before-parse-hook)
  (goto-char (point-min))
  (unwind-protect
      (let* ((xml-decl (esh--html-read-tag "?xml"))
             (doctype (esh--html-read-tag "!doctype"))
             (tree (libxml-parse-html-region (point) (point-max)))
             (esh--html-src-class-re (format "\\_<%s\\([^ ]+\\)\\_>"
                                          esh--html-src-class-prefix)))
        (erase-buffer)
        (dolist (tag (list xml-decl doctype))
          (when tag (insert tag)))
        (esh--htmlify-serialize (esh--htmlify-do-tree tree) t))
    (esh--kill-temp-buffers)))

(defun esh2html-html-file (path)
  "Fontify contents of all ESH environments in PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (esh2html-current-buffer)
    (buffer-string)))

(provide 'esh)
;;; esh.el ends here

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End:
