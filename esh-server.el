;;; esh-server.el --- Communicate with a daemon running ESH  -*- lexical-binding: t; -*-

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

;; This package contains functions than run on the server that esh-client
;; spawns.  It's mostly an eval facility plus error handling.

;;; Code:

(require 'esh)

(defvar esh-server--backtrace nil
  "Backtrace of last error during `esh-server-eval'.")

(defvar esh-server-initializing nil
  "Non-nil while the ESH server is initializing.")

(defvar esh-server-init-info 'none
  "Init info (INIT-FPATH . _) that was used to initialize the server.")

(defvar esh-server--capture-backtraces nil
  "Whether to capture backtraces.
Capturing a backtrace can be very costly, because arguments can
be huge — so only do it if requested.")

(defvar esh--server-frame nil
  "Global variable holding the invisible ESH frame.")

(defun esh-server--backtrace ()
  "Retrieve a backtrace and clean it up."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (backtrace)
      (goto-char (point-min))
      (when (re-search-forward "^  esh-server--handle-error" nil t)
        (delete-region (point-min) (point-at-bol)))
      (buffer-string))))

(defun esh-server--handle-error (&rest args)
  "Handle an error in code run on the server.
This function is appropriate as a value for `debugger' (hence
ARGS).  We need this function because the server doesn't print
backtraces; thus we take a more aggressive stand and simply
intercept all errors as they happen.  We just store the backtrace
in esh--server-backtrace and rethrow the error immediately; then
the `condition-case' in `esh-server-eval' will catch the error,
unless it's nicely handled somewhere else."
  ;; Prevent recursive error catching
  (let ((debugger #'debug)
        (debug-on-quit nil)
        (debug-on-error nil)
        (debug-on-signal nil))
    ;; HACK HACK HACK: The first time the debugger is invoked, the value of
    ;; `num-nonmacro-input-events' is recorded (eval.c).  On subsequent
    ;; invocations, there's a check to see if the `num-nonmacro-input-events'
    ;; has increased.  But since all of this is running on the server, there are
    ;; no non-macro input events; and thus the debugger is only broken into
    ;; once.  To work around this, we increase `num-nonmacro-input-events' here.
    (setq num-nonmacro-input-events (1+ num-nonmacro-input-events))
    (pcase args
      (`(exit ,retv) retv)
      (`(error ,error-args)
       (when esh-server--capture-backtraces
         ;; Capturing backtraces can be very costly
         (setq esh-server--backtrace (esh-server--backtrace)))
       (signal (car error-args) (cdr error-args))))))

(defun esh-server--writeout (file form)
  "Write FORM to FILE."
  (with-temp-buffer
    (insert (prin1-to-string form))
    (write-region (point-min) (point-max) file)))

(defun esh-server-eval (form file &optional capture-backtraces)
  "Eval FORM and write result or error to FILE.
Written value can be `read' from FILE; it is either a
list (success RESULT) or a cons (error ERR BACKTRACE).  BACKTRACE
will be non-nil only if CAPTURE-BACKTRACES was non-nil."
  (condition-case err
      (let* (;; Make sure that we'll intercept all errors
             (debug-on-quit t)
             (debug-on-error t)
             (debug-on-signal t)
             (debug-ignored-errors nil)
             ;; Make sure debugger has room to execute
             (max-specpdl-size (+ 50 max-specpdl-size))
             (max-lisp-eval-depth (+ 50 max-lisp-eval-depth))
             ;; Register ourselves as the debugger
             (debugger #'esh-server--handle-error)
             ;; Possibly turn backtraces on
             (esh-server--capture-backtraces capture-backtraces)
             ;; Compute result
             (result (eval form)))
        (esh-server--writeout file `(success ,result)))
    (error (esh-server--writeout file `(error ,err ,esh-server--backtrace)))))

(defun esh-server-init (display &optional init-info)
  "Initialize the ESH server.
Create an invisible frame on DISPLAY after loading INIT-INFO,
which should be a cons of (INIT-FPATH . CLIENT-PROVIDED-DATA).
No error checking here; we expect this to be invoked through
`esh-server-eval'."
  (setq esh-server-init-info init-info)
  (setq-default load-prefer-newer t)
  (setq-default text-quoting-style 'grave)
  (let ((init-fpath (car init-info)))
    (when init-fpath
      (let ((esh-server-initializing t))
        (load-file init-fpath))))
  (ignore (setq esh--server-frame
                (make-frame `((window-system . x)
                              (display . ,display)
                              (visibility . nil))))))

(defun esh-server-process (path format &optional fragment-p)
  "Latexify PATH in FORMAT and return the result as a string.
With non-nil FRAGMENT-P, don't read inline env declarations and
don't complain about a missing \\begin{document}.  No error
checking here; we expect this to be invoked through
`esh-server-eval'."
  ;; (require 'profiler)
  ;; (profiler-start 'cpu)
  ;; (prog1
  ;; (profiler-write-profile (profiler-cpu-profile) "esh.profile"))
  (with-selected-frame esh--server-frame
    (pcase format
      (`tex (esh-latexify-file path fragment-p))
      (`html (esh-htmlify-file path fragment-p))
      (_ (error "Unknown format %S" format)))))

(provide 'esh-server)
;;; esh-server.el ends here

;; Local Variables:
;; checkdoc-arguments-in-order-flag: nil
;; End:
