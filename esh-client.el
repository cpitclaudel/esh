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

(require 'esh)
(require 'dash)
(require 'server)

(defconst esh-client--server-name "esh--server")

(defun esh-client--server-running-p ()
  "Check if the ESH server is running."
  (let* ((server-use-tcp nil)
         (socket-fname (expand-file-name esh-client--server-name server-socket-dir)))
    (when (file-exists-p socket-fname) socket-fname)))

(defun esh-client--busy-wait (proc)
  "Wait for PROC to exit."
  (while (process-live-p proc)
    (accept-process-output proc 0 10)))

(defun esh-client--demote-errors (form)
  "Wrap FORM to prevent it from crashing the server."
  `(with-demoted-errors "Error in form sent to server: %S"
     ,form))

(defun esh-client--run-1 (form)
  "Start client process for FORM."
  (start-process "client" (current-buffer) "emacsclient"
                 "-s" esh-client--server-name "--eval" (prin1-to-string form)))

(defun esh-client--run (form)
  "Run FORM on ESH server."
  (esh-client--ensure-server)
  (with-temp-buffer
    (let* ((client-proc (esh-client--run-1 form)))
      (esh-client--busy-wait client-proc)
      (goto-char (point-min))
      (if (looking-at-p "\\*ERROR\\*:")
          (error "%s" (buffer-string))
        (read (buffer-string))))))

(defun esh-client--ensure-server-1 ()
  "Start server process."
  (let* ((server-name-form `(setq server-name ,esh-client--server-name)))
    (start-process "server" nil "emacs" "-Q"
                   "--eval" (prin1-to-string server-name-form)
                   "--daemon")))

(defun esh-client--server-init-form (server-load-path display &optional prelude)
  "Initialize the ESH server.
Inherit SERVER-LOAD-PATH, then create an invisible frame on DISPLAY
after loading PRELUDE."
  `(progn
     (setq load-path ',server-load-path)
     (setq gc-cons-threshold 16000000)
     (require 'esh)
     (when ,prelude (load-file ,prelude))
     (setq esh--server-frame (make-frame '((window-system . x)
                                           (display . ,display)
                                           (visibility . nil))))
     t))

(defun esh-client--init-server ()
  "Initialize ESH server."
  (let ((init-fpath (expand-file-name "esh-init-file.el")))
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
  (-when-let* ((socket-fname (esh-client--server-running-p)))
    (signal-process (server-eval-at esh-client--server-name '(emacs-pid)) 'kill)
    (delete-file socket-fname)))

(defun esh-client--latexify-batch-form (path)
  "Construct a ELisp form to latexify PATH."
  `(progn
     ;; (profiler-start 'cpu)
     ;; (profiler-write-profile (profiler-cpu-profile) "batch.profile")
     (with-selected-frame esh--server-frame
       (esh-latexify-file ,path))))

(defun esh-latexify-batch (path &optional restart-server)
  "Latexify PATH.
This starts an Emacs daemon, and runs the latexification code on
it.  If a daemon is available it is reused, except when
RESTART-SERVER is non-nil (in that case, the server is restarted
first).  Originally, the only reason a daemon was needed was that
it's hard to make Emacs' initial frame invisible.  Now, it's also
used to make things faster."
  (when restart-server (esh-client-kill-server))
  (princ (esh-client--run (esh-client--latexify-batch-form path))))

;; (let* (;;(server-id (format "esh--%S-%S" (random) (float-time (current-time))))
;;        (server-pid nil))
;;   (unwind-protect
;;       (progn
;;         ;; (esh-client--busy-wait launch-server-proc) ;; Wait for server to start
;;         ;; (setq server-pid (server-eval-at server-id '(emacs-pid)))
;;         ;; (message "Server %S" server-pid)
;;         ) ;; )))
;;     (when (process-live-p launch-server-proc)
;;       (delete-process launch-server-proc))
;;     (if server-pid
;;         (signal-process server-pid 'kill)
;;       ;; May prompt:
;;       (server-eval-at server-id '(kill-emacs))))))


;;; Code:

(provide 'esh-client)
;;; esh-client.el ends here
