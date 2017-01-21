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

(defun esh-tree--dll-val (dll) "Get current element of DLL." (nth 0 dll))
(defun esh-tree--dll-prev (dll) "Get PREV link of DLL." (nth 1 dll))
(defun esh-tree--dll-next (dll) "Get NEXT link of DLL." (nth 2 dll))

(gv-define-setter esh-tree--dll-prev (prev dll) `(setf (nth 1 ,dll) ,prev))
(gv-define-setter esh-tree--dll-next (next dll) `(setf (nth 2 ,dll) ,next))

(defun esh-tree--dll-new (val prev next)
  "Create new link (VAL PREV NEXT).
Updates PREV and NEXT to point to new link."
  (let ((new (list val prev next)))
    (when prev (setf (esh-tree--dll-next prev) new))
    (when next (setf (esh-tree--dll-prev next) new))
    new))

(defmacro esh-tree--dll-push-front (e dll)
  "Insert E into linked list DLL.
Returns linked list including new link."
  `(setq ,dll (esh-tree--dll-new ,e (and ,dll (esh-tree--dll-prev ,dll)) ,dll)))

(defun esh-tree--dll-pop (dll)
  "Remove first element of linked list DLL.
Returns updated list."
  (pcase dll
    (`nil (error "Popping from empty list"))
    (`(,_ ,prev ,next)
     (when prev (setf (esh-tree--dll-next prev) next))
     (when next (setf (esh-tree--dll-prev next) prev))
     next)))

(defun esh-tree--dll-first (dll)
  "Rewind to beginning of linked list DLL."
  (let ((first nil))
    (while dll
      (setq first dll)
      (setq dll (esh-tree--dll-prev dll)))
    first))

(defun esh-tree--dll-to-list (dll)
  "Make a list from DLL."
  (setq dll (esh-tree--dll-first dll))
  (let ((acc nil))
    (while dll
      (push (esh-tree--dll-val dll) acc)
      (setq dll (esh-tree--dll-next dll)))
    (nreverse acc)))

(defun esh-tree--dll-from-list (ls)
  "Make a doubly-linked list from LS."
  (let ((dll nil))
    (dolist (elem (reverse ls))
      (esh-tree--dll-push-front elem dll))
    dll))

;; (esh-tree--dll-to-list (esh-tree--dll-from-list '((1 2 3) (4 5 6) 7 8 9)))

;;; Collecting properties

(require 'esh)

(defun esh-tree--annotate-ranges (ranges text-props face-attrs)
  "Annotate each range in RANGES with a property alist.
Returns a list of (START END ALIST); the keys of ALIST are
properties in TEXT-PROPS or face attributes in FACE-ATTRS; its
values are the values of these properties.  START is inclusive;
END is exclusive."
  (let ((acc nil))
    (pcase-dolist (`(,start . ,end) ranges)
      (let* ((props-alist (esh--extract-props text-props start))
             (attrs-alist (esh--extract-face-attributes face-attrs start))
             (merged-alist (nconc (esh--filter-cdr 'unspecified attrs-alist)
                                  (esh--filter-cdr nil props-alist))))
        (push (nconc (list start end) (list merged-alist)) acc)))
    (nreverse acc)))

(defun esh-tree--compute-tags-from-ranges (ranges props)
  "Enumerate opening and closing tags from annotated RANGES.
Each returned element has one of the following forms:
  (\\='text START END)
  (\\='open POSITION (PROPERTY . VALUE))
  (\\='close POSITION (PROPERTY . VALUE))
Each PROPERTY is one of PROPS.  There can be multiple consecutive (text …) nodes
if two consecutive ranges agree on all PROPS."
  (let ((tags nil)
        (prev-alist nil)
        (prev-end nil))
    (pcase-dolist (`(,start ,end ,alist) ranges)
      (let ((open-tags nil)
            (close-tags nil))
        (dolist (prop props)
          (let ((old (assq prop prev-alist))
                (new (assq prop alist)))
            (unless (equal (cdr old) (cdr new))
              (when old
                (push `(close ,start ,old) close-tags))
              (when new
                (push `(open ,start ,new) open-tags)))))
        (let ((text-node `((text ,start ,end))))
          (setq tags (nconc text-node (nreverse open-tags) close-tags tags))))
      (setq prev-end end)
      (setq prev-alist alist))
    (dolist (pair prev-alist)
      (push `(close ,prev-end ,pair) tags))
    (nreverse tags)))
(provide 'esh-tree)
;;; esh-tree.el ends here
