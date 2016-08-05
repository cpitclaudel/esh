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

;; Can only use built-in packages in esh-cli and esh-client, because ‘cask exec’
;; is slow.  On server, it's fine to use cask-loaded dependencies.

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

(defvar esh-client-use-cask nil
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

;;; RPC forms

(defun esh-client--rpc-eval-form (form dest)
  "Construct a form to eval FORM on server.
Output of FORM is saved to DEST."
  `(esh-server-eval ',form ,dest))

;; FIXME remove
;; (defun esh-client--rpc-server-pre-init-form ()
;;   "Export current load path to ESH server.
;; No error checking here, but we don't expect this to run through
;; esh-server-eval, since it won't have been loaded yet."
;;   `(;; Load path needed to find `esh-server'
;;     (setq load-path (append load-path ,load-path))
;;     (require 'esh-server)))

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
  "Check if client response is *ERROR*, and throw if so."
  ;; Should not happen: `esh-server-eval' captures all errors
  (goto-char (point-min))
  (when (looking-at-p "\\*ERROR\\*:")
    (error "%s" (buffer-string))))

(defun esh-client--read-server-response (fpath)
  "Read server response from FPATH."
  (with-temp-buffer
    (insert-file-contents fpath)
    (pcase (read (buffer-string))
      (`(success ,retv) retv)
      (`(error ,err ,backtrace)
       (error "ESH error: %S\n%s" err
              (if esh-client-debug-server backtrace
                ">> Run esh2tex with --debug-server to see a stack trace."))))))

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

(defun esh-client--ensure-server-1 ()
  "Start server process."
  (let* ((server-name-form `(setq server-name ,esh-client--server-name))
         (emacs-cmdline `(,@(when esh-client-use-cask '("cask" "exec"))
                          "emacs" ,@(when esh-client-pass-Q-to-server '("-Q"))
                          "-L" ,esh-client--script-directory "-l" "esh-server"
                          "--eval" ,(prin1-to-string server-name-form)
                          "--daemon")))
    (apply #'start-process "server" nil emacs-cmdline)))

(defun esh-client--init-file-path ()
  "Find init file for current directory."
  (let* ((parent-dir (locate-dominating-file "." esh-client--init-file-name))
         (fpath (expand-file-name esh-client--init-file-name (or parent-dir user-emacs-directory))))
    (if (file-exists-p fpath)
        fpath
      (expand-file-name esh-client--init-file-name esh-client--script-directory))))

(defun esh-client--init-server ()
  "Initialize ESH server."
  (esh-client--run (esh-client--rpc-server-init-form (getenv "DISPLAY") (esh-client--init-file-path))))

(defun esh-client--ensure-server ()
  "Ensure that an ESH server is running."
  (unless (esh-client--server-running-p)
    (esh-client--busy-wait (esh-client--ensure-server-1))
    (message "ESH server started.")
    (esh-client--init-server)
    (message "ESH server initialized.")))

(defun esh-client-kill-server ()
  "Kill the ESH server."
  (interactive)
  (let ((socket-fname (esh-client--server-running-p)))
    (when socket-fname
      (signal-process (server-eval-at esh-client--server-name '(emacs-pid)) 'kill)
      (delete-file socket-fname))))

;;; Main entry point

(defun esh-client-latexify-batch (path)
  "Latexify PATH.
This starts an Emacs daemon, and runs the latexification code on
it.  If a daemon is available, it is reused.  Originally, the
only reason a daemon was needed was that it's hard to make Emacs'
initial frame invisible.  Now, it's also used to make things
faster."
  (princ (esh-client--run (esh-client--rpc-latexify-form path))))

(provide 'esh-client)
;;; esh-client.el ends here

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End:
