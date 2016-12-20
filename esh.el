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

(defun esh--find-auto-mode (fpath)
  "Find mode for FPATH.
There's no way to use the standard machinery (`set-auto-mode')
without also initializing the mode, which prevents us from
reusing the same buffer to process multiple source files.
Instead, go through `auto-mode-alist' ourselves."
  (let ((mode (assoc-default fpath auto-mode-alist 'string-match)))
    (unless mode
      (error "No mode found for %S in auto-mode-alist" fpath))
    (when (consp mode)
      (error "Unexpected auto-mode spec for %S: %S" mode fpath))
    mode))

(defun esh--normalize-color (color)
  "Return COLOR as a hex string."
  (unless (member color '("unspecified-fg" "unspecified-bg" "" nil))
    (let ((color (if (= (aref color 0) ?#) color
                   (apply #'color-rgb-to-hex (color-name-to-rgb color)))))
      (substring (upcase color) 1))))

(defun esh--normalize-color-unless (color attr)
  "Return COLOR as a hex string.
If COLOR matches value of ATTR in default face, return
nil instead."
  (let ((val (esh--normalize-color color))
        (attr-default (face-attribute 'default attr)))
    (unless (and attr (equal val (esh--normalize-color attr-default)))
      val)))

(defun esh--filter-cdr (val alist)
  "Remove conses in ALIST whose `cdr' is VAL."
  ;; FIXME phase this out once Emacs 25 is everywhere
  (let ((kept nil))
    (while alist
      (let ((top (pop alist)))
        (when (not (eq (cdr top) val))
          (push top kept))))
    (nreverse kept)))

(defun esh--append-dedup (&rest seqs)
  "Concatenate SEQS, remove duplicates (wrt `eq') on the way."
  (let ((deduped nil))
    (while seqs
      (let ((seq (pop seqs)))
        (while seq
          (let ((elem (pop seq)))
            (unless (memq elem deduped)
              (push elem deduped))))))
    (nreverse deduped)))

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

(defun esh--insert-file-contents (fname)
  "Like (`insert-file-contents' FNAME), but allow all local variables."
  (let ((enable-local-variables :all))
    (insert-file-contents fname)))

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
  ;; If we don't deduplicate relative font sizes get squared
  (esh--faces-attribute (esh--append-dedup (esh--as-list (get-text-property pos 'face))
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

(defun esh--missing-mode-error-msg (mode)
  "Construct an error message about missing MODE."
  (propertize (format esh--missing-mode-template mode "  ")
              'face 'error 'font-lock-face 'error))

(defvar esh--temp-buffers nil
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

(defvar esh-pre-highlight-hook nil)
(defvar esh-post-highlight-hook nil) ;; FIXME document these

(defun esh--export-buffer (export-fn)
  "Refontify current buffer, then invoke EXPORT-FN.
EXPORT-FN should do the actual exporting."
  (run-hook-with-args 'esh-pre-highlight-hook)
  (esh--font-lock-ensure)
  (run-hook-with-args 'esh-post-highlight-hook)
  (funcall export-fn))

(defun esh--export-str (str mode-fn export-fn)
  "Fontify STR in a MODE-FN buffer, then invoke EXPORT-FN.
EXPORT-FN should do the actual exporting."
  (with-current-buffer (esh--make-temp-buffer mode-fn)
    (insert str)
    (esh--export-buffer export-fn)))

;;; Producing LaTeX

(defvar esh--latex-props '(display invisible line-height esh-begin-box esh-end-box newline))
(defvar esh--latex-face-attrs '(:underline :background :foreground :weight :slant :height))

(eval-and-compile
  (defvar esh--latex-specials
    ;; http://tex.stackexchange.com/questions/67997/escaped-characters-in-typewriter-font/68002#68002
    '((?$ . "\\$") (?% . "\\%") (?& . "\\&") (?{ . "\\{") (?} . "\\}") (?_ . "\\_") (?# . "\\#")
      (?` . "{`}") (?' . "{'}") (?< . "{<}") (?> . "{>}") ;; A few ligatures
      (?\\ . "\\textbackslash{}") (?^ . "\\textasciicircum{}") (?~ . "\\textasciitilde{}")
      (?\s . "\\ESHSpace{}") (?- . "\\ESHDash{}"))))

(defvar esh--latex-specials-re
  (eval-when-compile
    (regexp-opt-charset (mapcar #'car esh--latex-specials))))

(defun esh--latex-substitute-special (m)
  "Get replacement for LaTeX special M."
  ;; If this become slows, use a vector and index by (aref m 0)
  (cdr (assq (aref m 0) esh--latex-specials)))

(defun esh--latex-substitute-specials (str)
  "Escape LaTeX specials in STR."
  (replace-regexp-in-string
   esh--latex-specials-re #'esh--latex-substitute-special str t t))

(defvar esh--latex-escape-alist nil
  "Alist of additional ‘char → LaTeX string’ mappings.")

(defun esh-latex-add-unicode-substitution (char-str latex-cmd)
  "Register an additional ‘unicode char → LaTeX command’ mapping.
CHAR-STR is a one-character string; LATEX-CMD is a latex command."
  (unless (and (stringp char-str) (eq (length char-str) 1))
    (user-error "%S: %S should be a one-character string"
                'esh-latex-add-unicode-substitution char-str))
  (add-to-list 'esh--latex-escape-alist (cons (aref char-str 0) latex-cmd)))

(defun esh--latex-escape-1 (char)
  "Escape CHAR for use with pdfLaTeX."
  (unless (featurep 'esh-latex-escape)
    (load-file (expand-file-name "esh-latex-escape.el" esh--directory)))
  (or (cdr (assq char esh--latex-escape-alist))
      (let ((repl (gethash char (with-no-warnings esh-latex-escape-table))))
        (and repl (format "\\ESHMathSymbol{%s}" repl)))))

(defun esh--latex-escape-unicode-char (char)
  "Replace currently matched CHAR with an equivalent LaTeX command."
  (let* ((translation (esh--latex-escape-1 (aref char 0))))
    (unless translation
      (error "No LaTeX equivalent found for %S.
Use (esh-latex-add-unicode-substitution %S %S) to add one"
             char char "\\someCommand"))
    (format "\\ESHUnicodeSubstitution{%s}" translation)))

(defun esh--latex-wrap-special-char (char)
  "Wrap CHAR in \\ESHSpecialChar{…}."
  (format "\\ESHSpecialChar{%s}" char))

(defvar esh-substitute-unicode-symbols nil
  "If non-nil, attempt to substitute Unicode symbols in code blocks.
Symbols are replaced by their closest LaTeX equivalent.  This
option is most useful with pdfLaTeX; with XeLaTeX or LuaLaTeX, it
should probably be turned off (customize \\ESHFallbackFont
instead).")

(defun esh--latex-wrap-non-ascii (str)
  "Wrap non-ASCII characters of STR.
If `esh-substitute-unicode-symbols' is nil, wrap non-ASCII characters into
\\ESHSpecialChar{}.  Otherwise, replace them by their LaTeX equivalents
and wrap them in \\ESHUnicodeSubstitution{}."
  ;; TODO benchmark against trivial loop
  (let* ((range "[^\000-\177]"))
    (if esh-substitute-unicode-symbols
        (replace-regexp-in-string range #'esh--latex-escape-unicode-char str t t)
      (replace-regexp-in-string range #'esh--latex-wrap-special-char str t t))))

(defun esh--mark-non-ascii ()
  "Tag non-ASCII characters of current buffer.
Puts text property `non-ascii' on non-ascii characters."
  (goto-char (point-min))
  (while (re-search-forward "[^\000-\177]" nil t)
    ;; Need property values to be distinct, hence (point)
    (put-text-property (match-beginning 0) (match-end 0) 'non-ascii (point))))

(defun esh--escape-for-latex (str)
  "Escape LaTeX special characters in STR."
  (esh--latex-wrap-non-ascii (esh--latex-substitute-specials str)))

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

(defun esh--normalize-height (height)
  "Normalize HEIGHT to a relative (float) height."
  (let* ((default-height (face-attribute 'default :height))
         (height (merge-face-attribute :height height default-height)))
    (unless (eq height default-height)
      (/ height (float default-height)))))

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
  (format "\\ESHRaise{%gex}{%s}" amount str))

(defun esh--latexify-span (range)
  "Render RANGE as a LaTeX string."
  (let* ((template "%s")
         (span (buffer-substring-no-properties (car range) (cdr range)))
         (latex-str (esh--escape-for-latex span))
         (props-alist (esh--extract-props esh--latex-props (car range)))
         (attrs-alist (esh--extract-face-attributes esh--latex-face-attrs (car range))))
    (pcase-dolist (`(,attribute . ,val) (esh--filter-cdr 'unspecified attrs-alist))
      (when val
        (setq template
              (pcase attribute
                (:foreground
                 (setq val (esh--normalize-color-unless val :foreground))
                 (if val (format "\\textcolor[HTML]{%s}{%s}" val template)
                   template))
                (:background
                 (setq val (esh--normalize-color-unless val :background))
                 ;; FIXME: Force all lines to have the the same height?
                 ;; Could use \\vphantom{'g}\\smash{…}
                 (if val
                     (format "\\colorbox[HTML]{%s}{%s}" val template)
                   template))
                (:weight
                 (format (pcase (esh--normalize-weight-coarse val)
                           (`light "\\ESHWeightLight{%s}")
                           (`regular "\\ESHWeightRegular{%s}")
                           (`bold "\\ESHWeightBold{%s}")
                           (_ (error "Unexpected weight %S" val)))
                         template))
                (:height
                 (pcase (esh--normalize-height val)
                   (`nil template)
                   (rel-h (format "\\textscale{%0.2g}{%s}" rel-h template))))
                (:slant
                 (format (pcase val
                           (`italic "\\ESHSlantItalic{%s}")
                           (`oblique "\\ESHSlantOblique{%s}")
                           (`normal "\\ESHSlantNormal{%s}")
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
         (pcase val
           (`(raise ,amount)
            (setq template (esh--latexify-raise template amount)))
           ((pred stringp)
            (setq latex-str val))
           (_ (error "Unexpected display property %S" val))))
        (`invisible
         ;; FIXME remove template too?
         (when val (setq latex-str "")))
        (`line-height
         (unless (floatp val)
           (error "Unexpected line-height property %S" val))
         (setq template (format "\\ESHStrut{%.2g}%s" val template)))
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
  "Prefix each line of STR with a call to \\ESHBol{}.
This used to only be needed for lines starting with whitespace,
but leading dashes sometimes behave strangely; it's simpler (and
safer) to prefix all lines.  \\ESHBol is a no-op in inline mode."
  (replace-regexp-in-string "^" "\\ESHBol{}" str t t))

(defun esh--latexify-protect-eols (str)
  "Suffix each line of STR with a call to \\ESHEol.
Do this instead of using catcodes, for robustness.  Including a
brace pair after \\ESHEol would break alignment of continuation
lines in inline blocks."
  (replace-regexp-in-string "\n" "\\ESHEol\n" str t t))

(defun esh--latexify-current-buffer ()
  "Export current buffer to LaTeX."
  (esh--remove-final-newline)
  (esh--commit-compositions)
  (esh--mark-newlines)
  (esh--mark-boxes)
  (let* ((str (mapconcat #'esh--latexify-span (esh--buffer-ranges) "")))
    (esh--latexify-protect-eols (esh--latexify-protect-bols str))))

(defun esh--latex-preamble ()
  "Read ESH's LaTeX preamble from disk."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "esh-preamble.tex" esh--directory))
    (buffer-substring-no-properties (point-min) (point-max))))

(defvar esh--latexify-preamble-marker "^%%[ \t]*ESH-preamble-here[ \t]*$")

(defun esh--latexify-add-preamble ()
  "Expand `esh--latexify-preamble-marker', if present."
  (goto-char (point-min))
  (if (re-search-forward esh--latexify-preamble-marker nil t)
      (replace-match (replace-quote (esh--latex-preamble)) t)))

(defconst esh--latex-block-begin
  (concat "^[ \t]*%%[ \t]*\\(ESH\\(?:InlineBlock\\)?\\): \\([^ \t\n]+\\)[ \t]*\n"
          "[ \t]*\\\\begin{\\(.+?\\)}.*\n"))

(defconst esh--latex-block-end
  "\n[ \t]*\\\\end{%s}")

(defun esh--latex-match-block ()
  "Find the next ESH block, if any."
  (when (re-search-forward esh--latex-block-begin nil t)
    (let* ((beg (match-beginning 0))
           (code-beg (match-end 0))
           (block-type (match-string-no-properties 1))
           (mode (match-string-no-properties 2))
           (env (match-string-no-properties 3))
           (end-re (format esh--latex-block-end (regexp-quote env))))
      (when (string= "" mode)
        (error "Invalid ESH header: %S" (match-string-no-properties 0)))
      (when (re-search-forward end-re nil t)
        (let* ((code-end (match-beginning 0))
               (code (buffer-substring-no-properties code-beg code-end)))
          (list block-type mode code beg (match-end 0)))))))

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
        (error "No matching delimiter found after use of `%s%c'"
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

(defun esh-latex-add-inline-verb (verb mode)
  "Teach ESH about an inline VERB, highlighted with MODE.
For example (esh-latex-add-inline-verb \"\\\\ocaml\" \\='tuareg-mode)
recognizes all instances of “\\ocaml|...|” as OCaml code to be
highlighted with `tuareg-mode'."
  (add-to-list 'esh-latex-inline-macro-alist (cons verb mode)))

(defconst esh--latexify-inline-template "\\ESHInline{%s}")
(defconst esh--latexify-block-template "\\begin{ESHBlock}\n%s\n\\end{ESHBlock}")
(defconst esh--latexify-inline-block-template "\\begin{ESHInlineBlock}\n%s\n\\end{ESHInlineBlock}")

(defvar esh--latex-pv
  "Whether to build and dump a table of highlighted inline code.")

(defvar-local esh--latex-pv-highlighting-map nil
  "List of (VERB CODE TEX) lists.
Each entry corresponds to one code snippet CODE, introduced by
\\VERB, and highlighted into TEX.")

(defun esh--latex-pv-record-snippet (verb code tex)
  "Record highlighting of VERB|CODE| as TEX."
  (when esh--latex-pv
    (unless (string-match "\\\\\\([a-zA-Z]+\\)" verb)
      (error "%S isn't compatible with `esh-latex-generate-inline-map'.
To work reliably, ESH verb macros must match \\[a-zA-Z]+" verb))
    (push (list (match-string 1 verb) code tex) esh--latex-pv-highlighting-map)))

(defconst esh--latex-pv-delimiters
  (mapcar (lambda (c)
            (cons c (regexp-quote (char-to-string c))))
          (string-to-list "|`/!=~+-,;:abcdefghijklmnopqrstuvwxyz"))
  "Alist of character → regexp matching that character.")

(defun esh--latex-pv-find-delimiter (code)
  "Find a delimiter that does not appear in CODE."
  (let ((candidates esh--latex-pv-delimiters)
        (delim nil))
    (while (not delim)
      (unless candidates
        (error "No delimiter found to wrap %S" code))
      (pcase-let* ((`(,char . ,re) (pop candidates)))
        (when (not (string-match-p re code))
          (setq delim char))))
    delim))

(defconst esh--latex-pv-def-template "\\DeclareRobustCommand*{\\%s}{\\ESHpvLookupVerb{%s}}\n")
(defconst esh--latex-pv-push-template "\\ESHpvDefineVerb{%s}%c%s%c{\\ESHInline{%s}}\n")

(defun esh--latex-pv-export-latex (map)
  "Prepare \\ESHpvDefine forms for all records in MAP.
Records must match the format of `esh--latex-pv-highlighting-map'."
  (with-temp-buffer
    (let ((verbs (make-hash-table :test #'equal))
          (decls (make-hash-table :test #'equal)))
      (pcase-dolist (`(,verb ,code ,tex) map)
        (puthash verb t verbs)
        (let* ((dl (esh--latex-pv-find-delimiter code))
               (decl (format esh--latex-pv-push-template verb dl code dl tex)))
          (unless (gethash decl decls) ;; Remove duplicates
            (puthash decl t decls)
            (insert decl))))
      (maphash (lambda (verb _)
                 (insert (format esh--latex-pv-def-template verb verb)))
               verbs))
    (buffer-string)))

(defun esh--latexify-do-inline-macros ()
  "Latexify sources in ESH inline macros."
  (let* ((modes-alist esh-latex-inline-macro-alist)
         (envs-re (when modes-alist (regexp-opt (mapcar #'car modes-alist)))))
    (esh--latexify-beginning-of-document)
    (let ((match-info nil))
      (while (setq match-info (esh--latexify-inline-verb-matcher envs-re))
        (pcase match-info
          (`(,beg ,end ,code-beg ,code-end ,cmd)
           (let* ((mode-fn (cdr (assoc cmd modes-alist)))
                  (code (buffer-substring-no-properties code-beg code-end))
                  (tex (esh--export-str code mode-fn #'esh--latexify-current-buffer)))
             (goto-char beg)
             (delete-region beg end)
             (esh--latex-pv-record-snippet cmd code tex)
             (insert (format esh--latexify-inline-template tex)))))))))

(defconst esh--latex-block-templates
  `(("ESH" . ,esh--latexify-block-template)
    ("ESHInlineBlock" . ,esh--latexify-inline-block-template)))

(defun esh--latexify-do-block-envs ()
  "Latexify sources in esh block environments."
  (goto-char (point-min))
  (let ((match nil))
    (while (setq match (esh--latex-match-block))
      (pcase-let* ((`(,block-type ,mode-str ,code ,beg ,end) match)
                   (mode-fn (esh--resolve-mode-fn mode-str))
                   (template (cdr (assoc block-type esh--latex-block-templates))))
        (delete-region beg end)
        (let* ((tex (esh--export-str code mode-fn #'esh--latexify-current-buffer)))
          (insert (format template tex)))))))

(defun esh2tex-current-buffer ()
  "Fontify contents of all ESH environments.
Replace the ESH-Latexify sources in environments delimited by
`esh-latexify-block-envs' and user-defined inline groups."
  (interactive)
  (save-excursion
    (unwind-protect
        (progn
          (esh--latexify-add-preamble)
          (esh--latexify-do-inline-macros)
          (esh--latexify-do-block-envs))
      (esh--kill-temp-buffers))))

(defun esh2tex-tex-file (path)
  "Fontify contents of all ESH environments in PATH."
  (with-temp-buffer
    (esh--insert-file-contents path)
    (esh2tex-current-buffer)
    (buffer-string)))

(defun esh2tex-tex-file-pv (path)
  "Find and highlight inline ESH macros in PATH.
Return a document consisting of “snippet → highlighted
code” pairs (in \\ESHpvDefine form)."
  (let ((esh--latex-pv t))
    (with-temp-buffer
      (esh--insert-file-contents path)
      (esh2tex-current-buffer)
      (esh--latex-pv-export-latex esh--latex-pv-highlighting-map))))

(defun esh2tex-source-file (source-path)
  "Fontify contents of SOURCE-PATH.
Return result as a LaTeX string."
  (let ((mode-fn (esh--find-auto-mode source-path)))
    (with-current-buffer (esh--make-temp-buffer mode-fn)
      (esh--insert-file-contents source-path)
      (esh--export-buffer #'esh--latexify-current-buffer))))

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
(defvar esh--html-face-attrs '(:underline :background :foreground :weight :slant)) ;; TODO :height :line-height

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
           (when (setq val (esh--normalize-color-unless val :foreground))
             (push (concat "color: #" val) styles)))
          (:background
           (when (setq val (esh--normalize-color-unless val :background))
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
            (setq raised t) ;;FIXME handle amount > 0 case
            (push (format "bottom: %gem" amount) styles))
           ((pred stringp)
            (setq text val))
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
    (esh--insert-file-contents path)
    (esh2html-current-buffer)
    (buffer-string)))

(provide 'esh)
;;; esh.el ends here

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End:
