;;; esh-tree.el --- Transform a highlighted buffer into an annotated tree of text spans  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement.pitclaudel@live.com>
;; Keywords: internal

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

;;

;;; Code:

;;; Linked-list data structure.

(defun esh-tree--ll-val (ll) "Get current element of LL." (nth 0 ll))
(defun esh-tree--ll-prev (ll) "Get PREV pointer of LL." (nth 1 ll))
(defun esh-tree--ll-next (ll) "Get NEXT pointer of LL." (nth 2 ll))

(gv-define-setter esh-tree--ll-prev (prev ll) `(setf (nth 1 ,ll) ,prev))
(gv-define-setter esh-tree--ll-next (next ll) `(setf (nth 2 ,ll) ,next))

(defun esh-tree--ll-new (val prev next)
  "Create new link (VAL PREV NEXT).
Updates PREV and NEXT to point to new link."
  (let ((new (list val prev next)))
    (when prev (setf (esh-tree--ll-next prev) new))
    (when next (setf (esh-tree--ll-prev next) new))
    new))

(defun esh-tree--ll-push-front (e ll)
  "Insert E into linked list LL.
Returns linked list including new link."
  (esh-tree--ll-new e (esh-tree--ll-prev ll) ll))

(defun esh-tree--ll-pop (ll)
  "Remove first element of linked list LL.
Returns updated list."
  (pcase ll
    (`nil (error "Popping from empty list"))
    (`(,_ ,prev ,next)
     (when prev (setf (esh-tree--ll-next prev) next))
     (when next (setf (esh-tree--ll-prev next) prev))
     next)))

(defun esh-tree--ll-first (ll)
  "Rewind to beginning of linked list LL."
  (let ((first nil))
    (while ll
      (setq first ll)
      (setq ll (esh-tree--ll-prev ll)))
    first))

(defun esh-tree--ll-to-list (ll)
  "Make a list from LL."
  (setq ll (esh-tree--ll-first ll))
  (let ((acc nil))
    (while ll
      (push (esh-tree--ll-val ll) acc)
      (setq ll (esh-tree--ll-next ll)))
    (nreverse acc)))

(provide 'esh-tree)
;;; esh-tree.el ends here
