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

;;; Misc utils

(defun esh--normalize-color (color)
  "Return COLOR as a hex string."
  (unless (member color '("unspecified-fg" "unspecified-bg" "" nil))
    (let ((color (if (= (aref color 0) ?#) color
                   (apply #'color-rgb-to-hex (color-name-to-rgb color)))))
      (substring (upcase color) 1))))

(defun esh--filter-cdr (val alist)
  "Remove conses in ALIST whose `cdr' is VAL."
  ;; This is essentially seq-filter
  (let ((kept nil))
    (while alist
      (let ((top (pop alist)))
        (when (not (eq (cdr top) val))
          (push top kept))))
    (nreverse kept)))

(defun esh--extract-props (props str)
  "Read PROPS from STR as an ALIST or (PROP . VAL)."
  (mapcar (lambda (prop)
            (cons prop (get-text-property 0 prop str)))
          props))

(defun esh--same-file-p (f1 f2)
  "Check if F1 and F2 are the same files."
  (string= (file-truename f1) (file-truename f2)))

(defvar esh--name-to-mode-alist nil
  "Alist of block name → mode name.

For example, this could include (\"ocaml\" . \"tuareg\") to use
`tuareg-mode' for code blocks tagged “src-ocaml”.")

(defun esh--resolve-mode-fn (fn-name)
  "Translate FN-NAME to a function symbol.
Uses `esh--name-to-mode-alist'."
  (let ((translated (cdr (assoc fn-name esh--name-to-mode-alist))))
    (intern (concat (or translated fn-name) "-mode"))))

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

;;; Segmenting a buffer

;; FIXME this splits by properties including overlays, but then it ignores them

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
  (mapcar (lambda (pair)
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

(defun esh--str-face-attribute (str attribute)
  "Look at STR and find out value of face ATTRIBUTE."
  (let ((face-or-faces (get-text-property 0 'face str)))
    (unless (listp face-or-faces)
      (setq face-or-faces (list face-or-faces)))
    (cons attribute (esh--faces-attribute face-or-faces attribute))))

(defun esh--extract-face-attributes (face-attributes text)
  "Extract FACE-ATTRIBUTES from TEXT."
  (mapcar (apply-partially #'esh--str-face-attribute text)
          face-attributes))

;;; Removing composition

(defun esh--commit-compositions ()
  "Apply compositions in current buffer.
This replaced each composed string by its composition, forgetting
the original string."
  (let ((composed-ranges (esh--buffer-ranges-from 1 'composition)))
    (dolist (pair (reverse composed-ranges))
      (pcase pair
        (`(,from . ,to)
         (let ((composition (get-text-property from 'composition))
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
                       ;; (point-at-bol 0) is beginning of previous line
                       ;; (match-beginning 0) is end of previous line
                       (if (eq (point-at-bol 0) (match-beginning 0))
                           'empty 'non-empty))))

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

(defmacro esh--make-temp-buffer (mode buffers)
  "Get temp buffer for MODE from BUFFERS.
If no such buffer exist, create one and add it to BUFFERS.  In
all cases, the buffer is erased, and a message is added to it if
the required mode isn't available."
  (let ((buf (make-symbol "buf")))
    `(save-match-data
       (let ((,buf (cdr (assq ,mode ,buffers))))
         (unless ,buf
           (setq ,buf (generate-new-buffer " *temp*"))
           (with-current-buffer ,buf
             (funcall (if (fboundp ,mode) ,mode #'fundamental-mode)))
           (push (cons ,mode ,buf) ,buffers))
         (with-current-buffer ,buf
           (erase-buffer)
           (unless (fboundp ,mode)
             (insert (esh--missing-mode-error-msg ,mode))))
         ,buf))))

;;; Producing LaTeX

(defvar esh--latex-props '(display invisible newline))
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

(defun esh--escape-for-latex (str)
  "Escape LaTeX special characters in STR."
  (esh--wrap-non-ascii (esh--latex-substitute-specials str)))

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

(defun esh--latexify-raise (str amount)
  "Issue a LaTeX command to raise STR by AMOUNT."
  (format "\\ESH%sRaise{%gex}{%s}"
          (if esh--inline "Inline" "Block")
          amount str))

(defun esh--latexify-span (span)
  "Render SPAN as a LaTeX string.
Exact behavior is dependent on value of `esh--inline'."
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
                    ;; use it unless a the underline needs to be colored.
                    (let* ((prefix (if color "\\ESHUnder" "\\u"))
                           (command (format "%s%S" prefix type))
                           (color-arg (if color (format "{\\color{%s}}" color) "")))
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
        (`newline
         ;; Ensure that no newlines are added inside commands (instead the
         ;; newline is added to the end of the template), and add an mbox to
         ;; prevent TeX from complaining about underfull boxes.
         (setq latex-str "")
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
  (esh--commit-compositions)
  (esh--mark-newlines)
  (let* ((str (mapconcat #'esh--latexify-span (esh--buffer-spans) "")))
    (if esh--inline str
      (esh--latexify-obeylines (esh--latexify-protect-bols str)))))

(defun esh--highlight-in-buffer (str buffer mode-fn convert-fn)
  "Insert STR in BUFFER, fontify it, and convert it.
With non-fbound MODE-FN, don't run font-lock on STR.  Calls
CONVERT-FN, whose return value nay depend on `esh--inline'."
  (save-match-data
    (with-current-buffer buffer
      (insert str)
      (when (fboundp mode-fn) (esh--font-lock-ensure))
      (funcall convert-fn))))

(defvar esh--latex-preamble
  "% Packages
%%%%%%%%%%

\\RequirePackage{xcolor} % TeXLive-recommended
\\RequirePackage[normalem]{ulem} % TeXLive-recommended

\\RequirePackage{relsize} % TeXLive-latex-extra
\\renewcommand{\\RSsmallest}{1pt}
\\renewcommand{\\RSlargest}{50pt}

% Fonts
%%%%%%%

% \\ESHFont is for code blocks
\\providecommand*{\\ESHFont}{\\ttfamily}

% \\ESHInlineFont is for inline code samples
\\providecommand*{\\ESHInlineFont}{\\ESHFont}

% \\ESHFallbackFont is applied to characters not covered by \\ESHFont
\\providecommand*{\\ESHFallbackFont}{\\ESHFont}

% Utils
%%%%%%%

% \\ESHNoHyphens disables hyphenation
\\providecommand*{\\ESHNoHyphens}{\\hyphenpenalty=10000}

% \\ESHBlockDash is a dash disallowing line breaks
\\providecommand*{\\ESHBlockDash}{\\hbox{-}\\nobreak}

% \\ESHCenterInWidthOf{#A}{#B} prints #B centered in a box as large as #A.
\\newdimen\\ESHtempdim%
\\providecommand*{\\ESHCenterInWidthOf}[2]
  {\\settowidth\\ESHtempdim{#1}%
   \\makebox[\\ESHtempdim][c]{#2}}

\\RequirePackage{iftex}
\\ifXeTeX
  % \\ESHWithFallback{#A} prints #A in the current font if possible, falling back to \\ESHFallbackFont
  % Adapted from https://tug.org/pipermail/xetex/2011-November/022319.html
  \\def\\ESHWithFallback#1{%
    \\begingroup
      \\def\\found{#1}%
      \\def\\notfound{\\ESHFallbackFont#1}%
      \\ifnum\\XeTeXfonttype\\font>0%
        \\ifnum\\XeTeXcharglyph`#1>0\\found\\else\\notfound\\fi
      \\else
        \\setbox0=\\hbox{\\tracinglostchars=0\\kern1sp#1\\expandafter}%
        \\ifnum\\lastkern=1\\notfound\\else\\found\\fi
      \\fi
    \\endgroup}
\\else
  % Fall back to just using \\ESHFallbackFont
  \\def\\ESHWithFallback#1{\\ESHFallbackFont#1}
\\fi

% \\ESHInlineSpecialChar and \\ESHBlockSpecialChar are used by ESH to indicate
% non-ascii characters, which may need a fallback font.
\\DeclareRobustCommand*{\\ESHInlineSpecialChar}[1]
  {{\\ESHFont\\ESHWithFallback{#1}}}
\\DeclareRobustCommand*{\\ESHBlockSpecialChar}[1]
  {{\\ESHCenterInWidthOf{\\ESHFont{a}}{\\ESHInlineSpecialChar{#1}}}}

% \\ESHInlineRaise and \\ESHBlockRaise implement sub/superscripts
\\DeclareRobustCommand*{\\ESHInlineRaise}[2]
  {\\raisebox{#1}{\\relsize{-2}#2}}
\\DeclareRobustCommand*{\\ESHBlockRaise}[2]
  {\\rlap{\\ESHInlineRaise{#1}{#2}}\\hphantom{#2}}

\\makeatletter
% ESHUnderline produces a straight underline
\\DeclareRobustCommand*{\\ESHUnderline}[1]
  {\\bgroup\\markoverwith{#1\\rule[-0.5ex]{2pt}{0.4pt}}\\ULon}
% ESHUnderwave produces a wavy underline
\\DeclareRobustCommand*{\\ESHUnderwave}[1]
  {\\bgroup\\markoverwith{#1\\lower3.5\\p@\\hbox{\\sixly\\char58}}\\ULon}
\\makeatother

% Environments
%%%%%%%%%%%%%%

% \\ESHInlineBasicSetup is used by \\ESHInline
\\providecommand*{\\ESHInlineBasicSetup}{\\ESHNoHyphens\\ESHInlineFont}

% \\ESHInline is used for inline code
% Note the extra pair of braces in the definition
\\providecommand*{\\ESHInline}[1]{{\\ESHInlineBasicSetup#1}}

% \\ESHBlockBasicSetup is used by \\ESHBlock
\\providecommand*{\\ESHBlockBasicSetup}
  {\\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0pt}
   \\ESHNoHyphens\\ESHFont\\spaceskip=\\fontdimen2\\font
   \\xspaceskip=0pt}

\\makeatletter
% \\ESHSkip is the amount to skip before and after an ESHBlock
\\@ifundefined{ESHSkip}{\\newlength{\\ESHSkip}\\setlength{\\ESHSkip}{\\baselineskip}}{}

% \\ESHBlock is used for code blocks
\\@ifundefined{ESHBlock}
  {\\newenvironment{ESHBlock}
     {\\ESHBlockBasicSetup\\par\\addvspace{\\ESHSkip}}
     {\\par\\addvspace{\\ESHSkip}}}{}
\\makeatother")

(defvar esh-latexify-block-envs
  `(("^[ \t]*%%[ \t]*ESH: \\([^ \t\n]+\\)[ \t]*\n[ \t]*\\\\begin{\\([^}]+\\)}.*\n" "\\begin{ESHBlock}\n"
     ,(lambda () (concat "\n[ \t]*\\\\end{" (match-string 2) "}")) "\n\\end{ESHBlock}"))
  "List of replaceable environments.")

(defvar esh--latexify-preamble-marker "^%%[ \t]*ESH-preamble-here[ \t]*$")
(defvar esh--latexify-inline-env-declaration-re "^[ \t]*%%[ \t]*ESH-inline-verb:[ \t]+\\([^ \t\n]+\\)[ \t]+\\(.*\\)$")

(defun esh--latexify-add-preamble ()
  "Expand `esh--latexify-preamble-marker'."
  (goto-char (point-min))
  (if (re-search-forward esh--latexify-preamble-marker nil t)
      (replace-match (replace-quote esh--latex-preamble) t)
    (error "%s" "Source document is missing the `%% ESH-preamble-here' line")))

(defun esh--latexify-inline-verb-matcher (re)
  "Search for a \\verb-like delimiter from point.
That is, a match of the form RE?...? where ? is any
character."
  (when (re-search-forward re nil t)
    (let ((form-beg (match-beginning 0))
          (command (match-string 0))
          (delimiter (char-to-string (char-after)))
          (code-beg (1+ (point))))
      (unless delimiter
        (error "No delimiter found after use of `%s'" command))
      (goto-char code-beg)
      (if (search-forward delimiter (point-at-eol) t)
          (list form-beg (point) code-beg (1- (point)) command)
        (error "No matching delimiter found after use of `%s%s'"
               command delimiter)))))

(defun esh--latexify-beginning-of-document ()
  "Go past \\begin{document}."
  (goto-char (point-min))
  (unless (search-forward "\\begin{document}" nil t)
    (error "No \\begin{document} found")))

(defun esh--latexify-find-inline-envs-decls ()
  "Construct a list of regexps matching user-defined inline environments.
This looks for `esh--latexify-inline-env-declaration-re, and
constructs a cons of a regular expressions matching all inline
envs, and an alist mapping envs to mode symbols."
  (let ((envs nil))
    (goto-char (point-min))
    (while (re-search-forward esh--latexify-inline-env-declaration-re nil t)
      (let* ((mode (esh--resolve-mode-fn (match-string 1)))
             (command (match-string 2)))
        (push (cons command mode) envs)))
    (when envs
      (cons (regexp-opt (mapcar #'car envs)) envs))))

(defun esh--latexify-do-inline-envs (master-buffer temp-buffers)
  "Latexify sources in esh inline environments.
Read definitions from MASTER-BUFFER if non nil, current buffer
otherwise.  TEMP-BUFFERS is be an alist of (MODE . TEMP-BUFFER).
If MASTER-BUFFER is nil and there's no \\begin{document},
complain loudly: otherwise, we'll risk making replacements in the
document's preamble."
  (pcase (with-current-buffer (or master-buffer (current-buffer))
           (esh--latexify-find-inline-envs-decls))
    (`(,envs-re . ,modes-alist)
     (if master-buffer
         (goto-char (point-min))
       (esh--latexify-beginning-of-document))
     (let ((match-info nil))
       (while (setq match-info (esh--latexify-inline-verb-matcher envs-re))
         (pcase match-info
           (`(,beg ,end ,code-beg ,code-end ,cmd)
            (let* ((esh--inline t)
                   (mode-fn (cdr (assoc cmd modes-alist)))
                   (code (buffer-substring-no-properties code-beg code-end))
                   (temp-buffer (esh--make-temp-buffer mode-fn temp-buffers)))
              (goto-char beg)
              (delete-region beg end)
              (insert (concat "\\ESHInline{"
                              (esh--highlight-in-buffer
                               code temp-buffer mode-fn
                               #'esh--latexify-current-buffer)
                              "}")))))))))
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
      (when (string= "" (match-string-no-properties 1))
        (error "Invalid ESH header: %S" (match-string-no-properties 0)))
      (let ((mode-fn (esh--resolve-mode-fn (match-string-no-properties 1)))
            (old-end (cond ((stringp old-end) old-end)
                           ((functionp old-end) (funcall old-end)))))
        (replace-match new-start t t)
        (re-search-forward old-end)
        (goto-char (match-beginning 0))
        (set-marker code-end (point))
        (replace-match new-end t t)
        (let* ((esh--inline nil)
               (code (buffer-substring-no-properties code-start code-end))
               (temp-buffer (esh--make-temp-buffer mode-fn temp-buffers)))
          (goto-char code-start)
          (delete-region code-start code-end)
          (insert (esh--highlight-in-buffer code temp-buffer mode-fn
                                         #'esh--latexify-current-buffer))))))
  temp-buffers)

(defun esh2tex-current-buffer (&optional master)
  "Fontify contents of all ESH environments.
Replace `esh--latexify-preamble-marker' by `esh--latex-preamble',
then latexify sources in environments delimited by
`esh-latexify-block-envs' and user-defined inline groups.  With
non-nil MASTER, read ESH settings from there and don't complain
about missing \\\\begin{document}s."
  (interactive)
  (save-excursion
    (with-current-buffer (or master (current-buffer))
      (esh--latexify-add-preamble))
    (let* ((temp-buffers nil)
           (code-start (make-marker))
           (code-end (make-marker)))
      (unwind-protect
          (progn
            (setq temp-buffers
                  (esh--latexify-do-inline-envs master temp-buffers))
            (setq temp-buffers
                  (esh--latexify-do-block-envs code-start code-end temp-buffers)))
        (set-marker code-start nil)
        (set-marker code-end nil)
        (mapcar (lambda (p) (kill-buffer (cdr p))) temp-buffers)))))

(defun esh-latexify-file (path &optional master)
  "Fontify contents of all ESH environments in PATH.
With non-nil MASTER, read ESH settings from there."
  (let ((master-buffer (when (and master (not (esh--same-file-p path master)))
                         (generate-new-buffer " *temp*"))))
    (unwind-protect
        (with-temp-buffer
          (when master-buffer
            (with-current-buffer master-buffer
              (insert-file-contents master)
              (current-buffer)))
          (insert-file-contents path)
          (esh2tex-current-buffer master-buffer)
          (buffer-string))
      (when (and master (buffer-live-p master-buffer))
        (kill-buffer master-buffer)))))

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

(defvar esh--html-props '(display invisible newline))
(defvar esh--html-face-attrs '(:underline :background :foreground :weight :slant))

(defun esh--htmlify-plug-hole (term filler)
  "Replace instances of `hole' in TERM with FILLER."
  (cond
   ((eq term 'hole) filler)
   ((symbolp term) term)
   ((listp term) (mapcar #'esh--htmlify-plug-hole term))
   (t (error "Invalid SEXP %S" term))))

(defun esh--htmlify-span (span)
  "Render SPAN as an HTML tree."
  (let ((styles nil)
        (text (substring-no-properties span))
        (props-alist (esh--extract-props esh--html-props span))
        (attrs-alist (esh--extract-face-attributes esh--html-face-attrs span)))
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
            ;; FIXME
            )
           (_ (error "Unexpected display property %S" val))))
        (`invisible
         (when val (setq text "")))
        (`newline
         ;; Ensure that no newlines are added inside commands.
         ;; FIXME background color extending past end of line
         (setq styles nil))
        (_ (error "Unexpected property %S" property))))
    (if (or (null text) (equal text "")) ""
      `(span ,(when styles
                `((style . ,(mapconcat #'identity styles ";"))))
             ,text))))

(defun esh--htmlify-current-buffer ()
  "Export current buffer to HTML."
  (esh--commit-compositions)
  (esh--mark-newlines)
  (mapcar #'esh--htmlify-span (esh--buffer-spans)))

(defconst esh--html-class-re
  "\\_<esh-\\(inline\\|block\\)-\\([^ ]+\\)\\_>"
  "Regexp matching classes of tags to be processed by ESH.")

(defun esh--htmlify-do-tree (node temp-buffers)
  "Highlight code in annotated descendants of NODE.
TEMP-BUFFERS is an alist of (MODE . TEMP-BUFFER)."
  (pcase node
    ((pred stringp) node)
    (`(,tag ,attributes . ,children)
     (let ((class (cdr (assq 'class attributes))))
       (if (and class (string-match esh--html-src-class-re class))
           (let* ((mode-fn (esh--resolve-mode-fn (match-string 2)))
                  (temp-buffer (esh--make-temp-buffer mode-fn temp-buffers)))
             (unless (stringp children)
               (error "Code block has children: %S" node))
             `(,tag ,attributes
                    ,(esh--highlight-in-buffer children temp-buffer mode-fn
                                            #'esh--htmlify-current-buffer)))
         `(,tag ,attributes
                ,(mapcar (lambda (c)
                           (esh--htmlify-do-tree c temp-buffers))
                         children)))))))

(defvar esh-html-before-parse-hook nil
  "Hook called before parsing input HTML.
Hook may e.g. make modifications to the buffer.")

(defun esh2html-current-buffer ()
  "Fontify contents of all ESH blocks in current document.
Highlight sources in any environments containing the “esh-block”
or “esh-inline” special classes."
  (interactive)
  (run-hook-with-args esh-html-before-parse-hook)
  (goto-char (point-min))
  (let ((temp-buffers nil))
    (unwind-protect
        (let ((tree (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (esh--htmlify-serialize (esh--htmlify-do-tree tree temp-buffers) t))
      (mapcar (lambda (p) (kill-buffer (cdr p))) temp-buffers))))

(defun esh-htmlify-file (path)
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
