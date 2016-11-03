;;; esh-cli.el --- Workhorse for esh2tex and esh2html -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement@clem-w50-mint>
;; Keywords: faces, tools

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

;; Run `esh2tex --usage' or `esh2html --usage' for help.

;;; Code:

(setq-default load-prefer-newer t)
(setq-default text-quoting-style 'grave)

(require 'esh-client)

(eval-and-compile
  (defconst esh-cli--script-full-path
    (or (and load-in-progress load-file-name)
        (bound-and-true-p byte-compile-current-file)
        (buffer-file-name))
    "Full path of this script.")

  (defconst esh-cli--esh-directory
    (file-name-directory esh-cli--script-full-path)
    "Full path to directory of this script."))

(defvar esh-cli--persist nil
  "See option --persist.")

(defvar esh-cli--stdout-p nil
  "See option --stdout.")

(defvar esh-cli--standalone-p nil
  "See option --standalone.")

(defun esh-cli--help ()
  "Read help from README file."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "README.rst" esh-cli--esh-directory))
    (goto-char (point-min))
    (while (re-search-forward "\\(\\.\\. code\\)?::.*\n" nil t) (replace-match ""))
    (buffer-string)))

(defun esh-cli--init ()
  "See option --init."
  (let ((template-dir (expand-file-name "template" esh-cli--esh-directory))
        (fonts-dir (expand-file-name "example/fonts" esh-cli--esh-directory))
        (esh2tex (expand-file-name "bin/esh2tex" esh-cli--esh-directory))
        (esh2html (expand-file-name "bin/esh2html" esh-cli--esh-directory)))
    (esh-cli--write-preamble)
    (pcase-dolist (`(,src-dir . ,dst-dir) `((,template-dir . "")
                                            (,fonts-dir . "fonts")))
      (make-directory dst-dir t)
      (dolist (fname (directory-files src-dir))
        (unless (file-directory-p fname)
          (copy-file (expand-file-name fname src-dir)
                     (expand-file-name fname dst-dir)))))
    (with-temp-file "Makefile"
      (insert (format "ESH2TEX := %S\n" esh2tex))
      (insert (format "ESH2HTML := %S\n" esh2html))
      (insert-file-contents "Makefile"))))

(defun esh-cli--write-preamble ()
  "Copy esh-preamble.tex to current directory, possibly overwriting it."
  (copy-file (expand-file-name "esh-preamble.tex" esh-cli--esh-directory)
             (expand-file-name "esh-preamble.tex") t t))

(defconst esh-cli--type-ext-alist
  '((html . "html") (latex . "tex") (latex-pv . "tex")))

(defconst esh-cli--output-ext-alist
  '((html . ".esh.%s") (latex . ".esh.%s") (latex-pv . ".esh-pv.%s")))

(defun esh-cli--process-one (in-path out-type)
  "Process IN-PATH in OUT-TYPE.
Output path is computed by appending “.esh.FORMAT” to file name,
unless `esh-cli--stdout-p' is non-nil.  Warns and skips if PATH
doesn't end in .FORMAT, unless `esh-cli--standalone-p' is
non-nil."
  (let* ((ext (cdr (assoc out-type esh-cli--type-ext-alist)))
         (out-ext-format (cdr (assoc out-type esh-cli--output-ext-alist)))
         (ext-re (format "\\.%s\\'" ext))
         (out-ext (format out-ext-format ext))
         (in-type
          (cond (esh-cli--standalone-p 'source)
                (t 'mixed)))
         (out-path
          (cond (esh-cli--stdout-p nil)
                (esh-cli--standalone-p (concat in-path out-ext))
                (t (replace-regexp-in-string ext-re out-ext in-path t t)))))
    (cond
     ((and (not esh-cli--standalone-p) (not (string-match-p ext-re in-path)))
      (esh-client-stderr "ESH Warning: skipping %S (unrecognized extension)
Are you missing --standalone?\n" in-path))
     (t (esh-client-process-one in-path out-path in-type out-type)))))

;; FIXME test this

(defun esh-cli--unexpected-arg-msg (arg)
  "Construct an unexpected ARG error message."
  (concat (format "ESH: Unexpected argument %S." arg)
          (unless (string-match-p "^-" arg)
            "  Are you using --stdout with multiple input files?")))

(defun esh-cli--main (format)
  "Main entry point for esh2 FORMAT."
  (unless argv
    (setq argv '("--usage")))
  (unwind-protect
      (let ((complain-about-missing-input t)
            (write-preamble nil))
        (while argv
          (pcase (pop argv)
            ("--usage"
             (princ (esh-cli--help))
             (setq complain-about-missing-input nil))
            ("--debug-on-error"
             (setq debug-on-error t)
             (setq esh-client-debug-server t))
            ("--kill-server"
             (esh-client-kill-server)
             (setq complain-about-missing-input nil))
            ("--persist"
             (setq esh-cli--persist t))
            ("--no-cask"
             (setq esh-client-use-cask nil))
            ("--no-Q"
             (setq esh-client-pass-Q-to-server nil))
            ("--stdout"
             (setq esh-cli--stdout-p t))
            ("--standalone"
             (setq esh-cli--standalone-p t))
            ("--precompute-verbs-map"
             (unless (eq format 'latex)
               (error "%s" (esh-cli--unexpected-arg-msg "--precompute-verbs-map")))
             (setq format 'latex-pv))
            ("--no-preamble"
             (setq write-preamble 'skip))
            ("--write-preamble"
             (setq write-preamble t)
             (setq complain-about-missing-input nil))
            ("--init"
             (esh-cli--init)
             (setq complain-about-missing-input nil))
            (arg
             (when (or (and argv esh-cli--stdout-p) (string-match-p "\\`--" arg))
               (error "%s" (esh-cli--unexpected-arg-msg arg)))
             (setq complain-about-missing-input nil)
             (setq write-preamble (or write-preamble t))
             (esh-cli--process-one arg format))))
        (when complain-about-missing-input
          (error "No input files given"))
        (when (eq write-preamble t)
          (esh-cli--write-preamble)))
    (unless esh-cli--persist
      (esh-client-kill-server))))

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End:

(provide 'esh-cli)
;;; esh-cli ends here
