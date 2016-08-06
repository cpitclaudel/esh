;;; esh-client.el --- Communicate with a daemon running ESH  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'server) ;; That's Emacs' server

(defconst esh-client--server-name "esh--server")
(defconst esh-client--init-file-name "esh-init.el")

(defconst esh-client--script-full-path
  (or (and load-in-progress load-file-name)
      (bound-and-true-p byte-compile-current-file)
      (buffer-file-name))
  "Full path of this script.")

(defconst esh-client--script-directory
  (file-name-directory esh-client--script-full-path)
  "Full path to directory of this script.")

(defvar esh-client-use-cask t
  "Whether to use `cask exec' to start the highlighting server.")

(defvar esh-client-pass-Q-to-server t
  "Whether to pass -Q to the server.")

(defvar esh-client-debug-server nil
  "Whether to print backtraces produced by the server.")

;;; Utils

(defun esh-client--server-running-p ()
  "Check if the ESH server is running."
  (let* ((server-use-tcp nil)
         (socket-fname (expand-file-name esh-client--server-name server-socket-dir)))
    (when (file-exists-p socket-fname) socket-fname)))

(defun esh-client--busy-wait (proc)
  "Wait for PROC to exit."
  (while (process-live-p proc)
    (accept-process-output proc 0 10)))

(defun esh-client-stderr (&rest args)
  "Like `message' on ARGS, but don't print a final newline.
Also, don't interact in weird ways with `message' (bug #24157)."
  (princ (apply #'format args) #'external-debugging-output))

(defmacro esh-client--with-progress-msg (msg &rest body)
  "Display MSG before running BODY, then display ‘done.’."
  (declare (indent 1) (debug t))
  `(progn
     (esh-client-stderr "%s:\n" ,msg)
     (prog1 ,@body
       (esh-client-stderr "  done.\n"))))

;;; RPC forms

(defun esh-client--rpc-eval-form (form dest)
  "Construct a form to eval FORM on server.
Output of FORM is saved to DEST.  `esh-server' is required here
instead of when starting the server due to the better error
reporting (see docstring of `esh-client--ensure-server-1')."
  `(progn
     (require 'esh-server)
     (esh-server-eval ',form ,dest)))

(defun esh-client--rpc-server-init-form (display &optional prelude)
  "Construct a form to initialize the ESH server.
DISPLAY and PRELUDE are forwarded."
  `(progn
     (esh-server-init ,display ,prelude)))

(defun esh-client--rpc-latexify-form (path)
  "Construct a form to latexify PATH on server."
  `(esh-server-latexify ,path))

;;; Running forms on server

(defun esh-client--run-1 (form)
  "Start client process for FORM.
Errors in client's output are signaled."
  (with-temp-buffer
    (let ((proc (start-process "client" (current-buffer)
                               "emacsclient" "-s" esh-client--server-name
                               "--eval" (prin1-to-string form))))
      (esh-client--busy-wait proc)
      (esh-client--die-if-rpc-failed))))

(defun esh-client--die-if-rpc-failed ()
  "Check if client response is *ERROR*, and throw if so.
This should only happen for very early errors, such as errors
while loading `esh-server'.  Other errors are captured and
properly returned by the server."
  (goto-char (point-min))
  (when (looking-at-p "\\*ERROR\\*:")
    (error "%s" (buffer-string))))

(defconst esh-client--backtrace-msg
  ">> Run esh2tex again with --debug-on-error to see the full stack trace. <<")

(defun esh-client--read-server-response (fpath)
  "Read server response from FPATH."
  (with-temp-buffer
    (insert-file-contents fpath)
    (pcase (read (buffer-string))
      (`(success ,retv) retv)
      (`(error ,err ,backtrace)
       (setq debug-on-error nil)
       (error "ESH error: %S\n%s" err
              (if esh-client-debug-server backtrace esh-client--backtrace-msg))))))

(defun esh-client--run (form)
  "Run FORM on ESH server.
Result of FORM is printed to a temporary file, and read back by
client, to work around a bug in `emacsclient'."
  (esh-client--ensure-server)
  (let* ((fname (make-temp-name "esh-server-output"))
         (fpath (expand-file-name fname temporary-file-directory)))
    (unwind-protect
        (progn
          (esh-client--run-1 (esh-client--rpc-eval-form form fpath))
          (esh-client--read-server-response fpath))
      (ignore-errors (delete-file fpath)))))

;;; Server management

(defun esh-client--init-file-path ()
  "Find init file for current directory."
  (let* ((parent-dir (locate-dominating-file "." esh-client--init-file-name))
         (fpath (expand-file-name esh-client--init-file-name (or parent-dir user-emacs-directory))))
    (if (file-exists-p fpath)
        fpath
      (expand-file-name esh-client--init-file-name esh-client--script-directory))))

(defun esh-client--truncate-right (str threshold)
  "If STR is longer than THRESHOLD, truncate it from the left."
  (let ((len (length str)))
    (if (<= len threshold) str
      (concat "..." (substring str (- len threshold) len)))))

(defun esh-client--init-server ()
  "Initialize ESH server."
  (let* ((init-fpath (esh-client--init-file-path)))
    (esh-client--with-progress-msg (format "Loading %S" (esh-client--truncate-right init-fpath 25))
      (esh-client--run (esh-client--rpc-server-init-form (getenv "DISPLAY") init-fpath)))))

(defun esh-client--use-cask ()
  "Check whether cask should be used."
  (and esh-client-use-cask (file-exists-p "Cask") (executable-find "cask")))

(defun esh-client--find-emacs ()
  "Find Emacs binary."
  (or (getenv "EMACS") "emacs"))

(defun esh-client--ensure-server-1 ()
  "Start server process.
This passes a -L arg to make it possible to load `esh-server',
but it doesn't load it immediately (neither with -l or with
--eval): errors while loading the daemon cause it to never
return.  Instead, the client passes (require 'esh-server) before
each `esh-server-eval' query."
  (let* ((server-name-form `(setq server-name ,esh-client--server-name))
         (emacs-cmdline `(,@(when (esh-client--use-cask) '("cask" "exec"))
                          ,(esh-client--find-emacs) ,@(when esh-client-pass-Q-to-server '("-Q"))
                          "-L" ,esh-client--script-directory ;; no -l esh-server
                          "--eval" ,(prin1-to-string server-name-form)
                          "--daemon")))
    (apply #'start-process "server" nil emacs-cmdline)))

(defun esh-client--ensure-server ()
  "Ensure that an ESH server is running."
  (unless (esh-client--server-running-p)
    (esh-client--with-progress-msg "Starting ESH"
      (esh-client--busy-wait (esh-client--ensure-server-1)))
    (esh-client--init-server)))

(defun esh-client-kill-server ()
  "Kill the ESH server."
  (interactive)
  (let ((socket-fname (esh-client--server-running-p)))
    (when socket-fname
      (signal-process (server-eval-at esh-client--server-name '(emacs-pid)) 'kill)
      (ignore-errors (delete-file socket-fname)))))

;;; Main entry point

(defun esh-client-latexify-one (path)
  "Latexify PATH.
This starts an Emacs daemon, and runs the latexification code on
it.  If a daemon is available, it is reused.  Originally, the
only reason a daemon was needed was that it's hard to make Emacs'
initial frame invisible.  Now, it's also used to make things
faster.  Output goes to `standard-output'."
  (esh-client--ensure-server) ;; To prevent progress messages from interleaving
  (esh-client--with-progress-msg (format "Highlighting %S" path)
    (princ (esh-client--run (esh-client--rpc-latexify-form path)))))

(provide 'esh-client)
;;; esh-client.el ends here

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End:
