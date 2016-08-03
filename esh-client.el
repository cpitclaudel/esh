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

(defvar esh-client-use-cask nil
  "Whether to use `cask exec' to start the highlighting server.")

(defun esh-client--server-running-p ()
  "Check if the ESH server is running."
  (let* ((server-use-tcp nil)
         (socket-fname (expand-file-name esh-client--server-name server-socket-dir)))
    (when (file-exists-p socket-fname) socket-fname)))

(defun esh-client--busy-wait (proc)
  "Wait for PROC to exit."
  (while (process-live-p proc)
    (accept-process-output proc 0 10)))

(defun esh-client--wrap-form (form dest)
  "Wrap FORM to save its output to file DEST."
  `(with-temp-buffer
     (insert (prin1-to-string ,form))
     (write-region (point-min) (point-max) ,dest)))

(defun esh-client--run-1 (form)
  "Start client process for FORM."
  (start-process "client" (current-buffer) "emacsclient"
                 "-s" esh-client--server-name "--eval" (prin1-to-string form)))

(defun esh-client--run (form)
  "Run FORM on ESH server.
Result of FORM is printed to a temporary file, and read back by
client, to work around a bug in `emacsclient'."
  (esh-client--ensure-server)
  (let* ((fname (make-temp-name "esh-server-output"))
         (fpath (expand-file-name fname temporary-file-directory)))
    (unwind-protect
        (with-temp-buffer
          (let* ((client-proc (esh-client--run-1 (esh-client--wrap-form form fpath))))
            (esh-client--busy-wait client-proc)
            (goto-char (point-min))
            (if (looking-at-p "\\*ERROR\\*:")
                (error "%s" (buffer-string))
              (with-temp-buffer
                (insert-file-contents fpath)
                (read (buffer-string))))))
      (ignore-errors (delete-file fpath)))))

(defun esh-client--ensure-server-1 ()
  "Start server process."
  (let* ((server-name-form `(setq server-name ,esh-client--server-name))
         (emacs-cmdline `(,@(if esh-client-use-cask '("cask" "exec")) "emacs" "-Q"
                          "--eval" ,(prin1-to-string server-name-form)
                          "--daemon")))
    (apply #'start-process "server" nil emacs-cmdline)))

(defun esh-client--server-init-form (server-load-path display &optional prelude)
  "Initialize the ESH server.
Inherit SERVER-LOAD-PATH, then create an invisible frame on DISPLAY
after loading PRELUDE."
  `(progn
     ;; Load path still needed to find `esh'
     (setq load-path (append load-path ',server-load-path))
     (require 'esh)
     (when ,prelude (load-file ,prelude))
     (setq esh--server-frame (make-frame '((window-system . x)
                                           (display . ,display)
                                           (visibility . nil))))
     t))

(defconst esh-client--init-file-name "esh-init.el")

(defun esh-client--init-file ()
  "Find init file for current directory."
  (let* ((parent-dir (locate-dominating-file "." esh-client--init-file-name))
         (fname (expand-file-name esh-client--init-file-name (or parent-dir user-emacs-directory))))
    (when (and (file-exists-p fname) (file-readable-p fname))
      fname)))

(defun esh-client--init-server ()
  "Initialize ESH server."
  (let ((init-fpath (esh-client--init-file)))
    (esh-client--run (esh-client--server-init-form load-path (getenv "DISPLAY") init-fpath))))

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

(defun esh-client--latexify-batch-form (path)
  "Construct a ELisp form to latexify PATH."
  `(progn
     (with-selected-frame esh--server-frame
       (esh-latexify-file ,path))))

(defun esh-client-latexify-batch (path)
  "Latexify PATH.
This starts an Emacs daemon, and runs the latexification code on
it.  If a daemon is available, it is reused.  Originally, the
only reason a daemon was needed was that it's hard to make Emacs'
initial frame invisible.  Now, it's also used to make things
faster."
  (princ (esh-client--run (esh-client--latexify-batch-form path))))

(provide 'esh-client)
;;; esh-client.el ends here

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End: