;;; esh.el --- Use Emacs to highlight code snippets in LaTeX documents  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Package-Requires: ((stream "2.2") (seq "2.18") (dash "2.12"))
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

(require 'seq)
(require 'color)
(require 'subr-x)

;;; Misc utils

(defun esh--normalize-color (color)
  "Return COLOR as a hex string."
  (unless (member color '("unspecified-fg" "unspecified-bg" ""))
    (let ((color (if (= (aref color 0) ?#) color
                   (apply #'color-rgb-to-hex (color-name-to-rgb color)))))
      (substring (upcase color) 1))))

(defun esh--filter-cdr (val alist)
  "Remove conses in ALIST whose `cdr' is VAL."
  (seq-filter (lambda (pair) (not (eq (cdr pair) val))) alist))

(defun esh--extract-props (props str)
  "Read PROPS from STR as an ALIST or (PROP . VAL)."
  (seq-map (lambda (prop)
             (cons prop (get-text-property 0 prop str)))
           props))

(defun esh--font-for-char (char)
  "Compute which font displays CHAR."
  (car (internal-char-font nil char)))

;;; Stream functions

;; We only use streams if available: this way, we don't have external
;; dependencies, and thus we don't directly need Cask.

(require 'stream nil t)

(defmacro esh--stream-cons (hd tl)
  "Make a stream of HD and TL."
  `(if (fboundp 'stream-cons)
       (stream-cons ,hd ,tl)
     (cons ,hd ,tl)))

(defun esh--stream-empty ()
  "Return nil."
  (if (fboundp 'stream-empty)
      (stream-empty)
    nil))

;;; Segmenting a buffer

;; FIXME this splits by properties including overlays, but then it ignores them

;;; As a proper stream, this would probably work; as a list, however, the
;;; recursion quickly exceeds max-lisp-eval-depth
(defun esh--stream-buffer-ranges-from (start prop)
  "Create a stream of buffer ranges from START.
Ranges are pairs of START..END positions in which all characters
have the same value of PROP or, if PROP is nil, of all
properties."
  (let ((end (if prop (next-single-char-property-change start prop)
               (next-char-property-change start))))
    (if (< start end)
        (esh--stream-cons (cons start end) (esh--buffer-ranges-from end prop))
      (esh--stream-empty))))

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

(defun esh--mark-newlines ()
  "Add a `newline' text property to each \\n character.
The value is either `empty' or `non-empty' (we need this to add a
dummy element on empty lines to prevent LaTeX from complaining
about underful hboxes)."
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (put-text-property (match-beginning 0) (match-end 0) 'newline
                       ;; (point-at-bol 0) is beginning of previous line
                       ;; (match-beginning 0) is end of previous line
                       (if (eq (point-at-bol 0) (match-beginning 0))
                           'empty 'non-empty))))

;;; Producing LaTeX

(defvar esh--latex-props '(display invisible newline))
(defvar esh--latex-face-attrs '(:underline :background :foreground :weight :slant))

(defvar esh--latex-specials '(?\\ ?^ ?$ ?~ ?% ?& ?{ ?} ?_ ?#))
(defvar esh--latex-substitutions '()) ;; None needed thanks to \obeyspaces and \obeylines

(defun esh--latex-substitutions ()
  "Construct a list of (REGEXP . REPLACE) to sanitize Latex code."
  (let ((specials-re (concat "\\(" (regexp-opt-charset esh--latex-specials) "\\)")))
    (cons (cons specials-re "{\\\\char`\\\\\\1}")
          esh--latex-substitutions)))

(defun esh--wrap-symbols (str)
  "Wrap characters of STR that use a fallback font in \\ESHSpecialChar{}."
  (let ((ref-fonts (list nil (esh--font-for-char ?a))))
    (seq-mapcat (lambda (chr)
                  (if (memq (esh--font-for-char chr) ref-fonts)
                      (char-to-string chr)
                    (format "\\ESHSpecialChar{%c}" chr)))
                str 'string)))

(defun esh--escape-for-latex (str)
  "Escape LaTeX special characters in STR."
  (with-temp-buffer
    (insert (substring-no-properties str))
    (pcase-dolist (`(,from . ,to) (esh--latex-substitutions))
      (goto-char (point-min))
      (while (re-search-forward from nil t)
        (replace-match to t)))
    (setq str (buffer-substring-no-properties (point-min) (point-max))))
  (esh--wrap-symbols str))

(defun esh--normalize-underline (underline)
  "Normalize UNDERLINE."
  (pcase underline
    (`t '(nil . line))
    ((pred stringp) `(,underline . line))
    (`(:color ,color) `(,color . line))
    (`(:color ,color :style ,style) `(,color . ,style))))

(defun esh--normalize-weight (weight)
  "Normalize WEIGHT."
  (pcase weight
    ((or `thin `ultralight `ultra-light `extralight
         `extra-light `light `book `demilight
         `semilight `semi-light)
     'light)
    ((or `normal `medium `regular)
     'regular)
    ((or `demi `demibold `semibold `semi-bold
         `bold `extrabold `extra-bold `black
         `ultrabold `ultra-bold)
     'bold)))

(defun esh--latexify-span (span)
  "Render SPAN as a LaTeX string."
  (let ((template "%s")
        (latex-str (esh--escape-for-latex span))
        (props-alist (esh--extract-props esh--latex-props span))
        (attrs-alist (esh--extract-face-attributes esh--latex-face-attrs span)))
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
                 (format (pcase (esh--normalize-weight val)
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
                 (format (pcase (esh--normalize-underline val)
                           ;; FIXME colored waves
                           (`(_ . wave) "\\uwave{%s}")
                           (`(nil . line) "\\uline{%s}")
                           (`(color . line) "\\colorlet{saved}{.}\\uline{\\color{saved}%s}")
                           (_ (error "Unexpected underline %S" val)))
                         template))
                (_ (error "Unexpected attribute %S" attribute))))))
    (pcase-dolist (`(,property . ,val) (esh--filter-cdr nil props-alist))
      (pcase property
        (`display
         (setq template
               (format (pcase val
                         (`(raise ,amount) ;; FIXME raisebox?
                          (if (> amount 0) "\\textsuperscript{%s}"
                            "\\textsubscript{%s}"))
                         (_ (error "Unexpected display property %S" val)))
                       template)))
        (`invisible
         (when val (setq latex-str "")))
        (`newline
         ;; Ensure that no newlines are added inside commands (instead the
         ;; newline is added to the end of the template), and add an mbox to
         ;; prevent TeX from complaining about underfull boxes.
         (setq latex-str "\\hfill")
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

(defun esh--latexify-current-buffer ()
  "Export current buffer to LaTeX."
  (esh--commit-compositions)
  (esh--mark-newlines)
  (esh--latexify-protect-bols (mapconcat #'esh--latexify-span (esh--buffer-spans) "")))

(defun esh--latexify-in-buffer (str buffer mode-fn)
  "Insert STR in BUFFER, fontify it, and latexify it.
With non-fbound MODE-FN, don't run font-lock on STR."
  (save-match-data
    (with-current-buffer buffer
      (insert str)
      (when (fboundp mode-fn) (font-lock-ensure))
      (esh--latexify-current-buffer))))

(defconst esh--missing-mode-template
  (concat ">>> (void-function %S); did you forget to install"
          " or 'require a dependency? <<<%s"))

(defun esh--missing-mode-error-msg (mode inline)
  "Construct an error message about missing MODE.
With non-nil INLINE, suffix message with blanks instead of newlines."
  (propertize (format esh--missing-mode-template mode (if inline "  " "\n\n"))
              'face 'error 'font-lock-face 'error))

(defmacro esh--make-temp-buffer (mode buffers inline)
  "Get temp buffer for MODE from BUFFERS.
If no such buffer exist, create one and add it to BUFFERS.  In
all cases, the buffer is erased, and a message is added to it if
the required mode isn't available.  INLINE is passed to
`esh--missing-mode-error-msg'."
  (let ((buf (make-symbol "buf")))
    `(save-match-data
       (let ((,buf (alist-get ,mode ,buffers)))
         (unless ,buf
           (setq ,buf (generate-new-buffer " *temp*"))
           (with-current-buffer ,buf
             (funcall (if (fboundp ,mode) ,mode #'fundamental-mode)))
           (push (cons ,mode ,buf) ,buffers))
         (with-current-buffer ,buf
           (erase-buffer)
           (unless (fboundp ,mode)
             (insert (esh--missing-mode-error-msg ,mode ,inline))))
         ,buf))))

(defvar esh--latex-preamble
  "% Packages
%%%%%%%%%%

\\RequirePackage{xcolor}
\\RequirePackage[normalem]{ulem}

% Fonts
%%%%%%%

% \\ESHFont is for code blocks
\\providecommand*{\\ESHFont}{\\ttfamily}

% \\ESHInlineFont is for inline code samples
\\providecommand*{\\ESHInlineFont}{\\ESHFont}

% \\ESHSpecialCharFont is applied to characters not included in Emacs' default font
\\providecommand*{\\ESHSpecialCharFont}{\\ttfamily}

% Environments
%%%%%%%%%%%%%%

% \\ESHNoHyphens disables hyphenation
\\providecommand*{\\ESHNoHyphens}{\\righthyphenmin=62\\lefthyphenmin=62}

% \\ESHInlineBasicSetup is used by \\ESHInline
\\providecommand*{\\ESHInlineBasicSetup}{%
  \\obeyspaces\\ESHNoHyphens\\ESHInlineFont}

% \\ESHInline is used for inline code
% Note the extra pair of braces in the definition
\\providecommand*{\\ESHInline}[1]{{\\ESHInlineBasicSetup#1}}

% \\ESHSpecialChar is used for special characters (those not found in the default font)
% Note the extra pair of braces in the definition
\\providecommand*{\\ESHSpecialChar}[1]{{\\ESHSpecialCharFont#1}}

% \\ESHObeySpaces is a variant of \\obeyspaces that forbids line breaks
{\\catcode`\\-=\\active
 \\catcode`\\ =\\active
 % The \hbox prevents line breaks around source hyphens
 \\gdef\\ESHObeySpaces{% Must be a \\gdef to escape the surrounding group
   \\catcode`\\-=\\active\\def-{\\hbox{\\char`\\-}\\nobreak}\\catcode`\\ =\\active\\def {\\nobreakspace}}}

% \\ESHBlockBasicSetup is used by \\ESHBlock
\\providecommand*{\\ESHBlockBasicSetup}{%
  \\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0pt}
  \\obeylines\\ESHObeySpaces\\ESHNoHyphens\\ESHFont
  \\spaceskip=\\fontdimen2\\font\\xspaceskip=0pt}

\\makeatletter
% \\ESHSkip is the amount to skip before and after an ESHBlock
\\@ifundefined{ESHSkip}{\\newlength{\\ESHSkip}\\setlength{\\ESHSkip}{\\baselineskip}}{}

% \\ESHBlock is used for code blocks
\\@ifundefined{ESHBlock}{%
  \\newenvironment{ESHBlock}{%
    \\ESHBlockBasicSetup\\par\\addvspace{\\ESHSkip}%
  }{%
    \\par\\addvspace{\\ESHSkip}
  }
}{}
\\makeatother")

(defvar esh-latexify-block-envs
  `(("^[ \t]*%%[ \t]*ESH: \\([-+a-zA-Z0-9]+\\)[ \t]*\n[ \t]*\\\\begin{\\([^}]+\\)}.*\n" "\\begin{ESHBlock}\n"
     ,(lambda () (concat "\n[ \t]*\\\\end{" (match-string 2) "}")) "\n\\end{ESHBlock}"))
  "List of replaceable environments.")

(defvar esh--latexify-preamble-marker "^%%[ \t]*ESH-preamble-here[ \t]*$")
(defvar esh--latexify-inline-env-declaration-re "^[ \t]*%%[ \t]*ESH-inline:[ \t]+\\([-+a-zA-Z0-9]+\\)[ \t]+\\(.*\\)$")

(defun esh--latexify-add-preamble ()
  "Expand `esh--latexify-preamble-marker'."
  (goto-char (point-min))
  (when (re-search-forward esh--latexify-preamble-marker nil t)
    (replace-match (replace-quote esh--latex-preamble) t)))

(defun esh--latexify-inline-envs ()
  "Construct a list of regexps matching user-defined inline environments.
This looks for `esh--latexify-inline-env-declaration-re'."
  (goto-char (point-min))
  (let ((envs nil))
    (while (re-search-forward esh--latexify-inline-env-declaration-re nil t)
      (when (string-empty-p (match-string 1))
        (error "Invalid ESH-inline declaration: %S" (match-string 0)))
      (let* ((mode (intern (match-string 1)))
             (def-string (regexp-quote (match-string 2)))
             (re (replace-regexp-in-string
                  (regexp-quote (regexp-quote "..."))
                  "\\(?1:.*?\\)" def-string t t)))
        (push (cons re mode) envs)))
    (reverse envs)))

(defun esh--latexify-do-inline-envs (temp-buffers)
  "Latexify sources in esh inline environments.
TEMP-BUFFERS is an alist of (MODE . TEMP-BUFFER)."
  (pcase-dolist (`(,env-re . ,mode-fn) (esh--latexify-inline-envs))
    (goto-char (point-min))
    (while (re-search-forward env-re nil t)
      (let* ((code (match-string-no-properties 1))
             (temp-buffer (esh--make-temp-buffer mode-fn temp-buffers t))
             (code-latex (esh--latexify-in-buffer code temp-buffer mode-fn)))
        (replace-match (concat "\\ESHInline{" code-latex "}") t t))))
  temp-buffers)

(defun esh--latexify-do-block-envs (code-start code-end temp-buffers)
  "Latexify sources in esh block environments.
CODE-START and CODE-END are markers.  TEMP-BUFFERS is an alist
of (MODE . TEMP-BUFFER)."
  (pcase-dolist (`(,old-start ,new-start ,old-end ,new-end)
                 esh-latexify-block-envs)
    (goto-char (point-min))
    (while (re-search-forward old-start nil t)
      (goto-char (match-end 0))
      (set-marker code-start (point))
      (when (string-empty-p (match-string-no-properties 1))
        (error "Invalid ESH header: %S" (match-string-no-properties 0)))
      (let ((mode-fn (intern (match-string-no-properties 1)))
            (old-end (cond ((stringp old-end) old-end)
                           ((functionp old-end) (funcall old-end)))))
        (replace-match new-start t t)
        (re-search-forward old-end)
        (goto-char (match-beginning 0))
        (set-marker code-end (point))
        (replace-match new-end t t)
        (let* ((code (buffer-substring-no-properties code-start code-end))
               (temp-buffer (esh--make-temp-buffer mode-fn temp-buffers nil)))
          (goto-char code-start)
          (delete-region code-start code-end)
          (insert (esh--latexify-in-buffer code temp-buffer mode-fn))))))
  temp-buffers)

(defun esh2tex-current-buffer ()
  "Fontify contents of all esh environments.
Replace `esh--latexify-preamble-marker' by
`esh--latex-preamble', then latexify sources in environments
delimited by `esh-latexify-block-envs' and user-defined inline
groups."
  (interactive)
  (save-excursion
    (esh--latexify-add-preamble)
    (let* ((temp-buffers nil)
           (code-start (make-marker))
           (code-end (make-marker)))
      (unwind-protect
          (progn
            (setq temp-buffers
                  (esh--latexify-do-inline-envs temp-buffers))
            (setq temp-buffers
                  (esh--latexify-do-block-envs code-start code-end temp-buffers)))
        (set-marker code-start nil)
        (set-marker code-end nil)
        (seq-map (lambda (p) (kill-buffer (cdr p))) temp-buffers)))))

(defun esh-latexify-file (path)
  "Fontify contents of all esh environments in PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (esh2tex-current-buffer)
    (buffer-string)))

(provide 'esh)
;;; esh.el ends here

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End:
