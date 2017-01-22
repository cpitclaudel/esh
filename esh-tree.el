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
Returns updated list.  Beware of the fact that if DLL is the
first link of a linked list, the caller must update it using
something like (setq dll (esh-tree--dll-pop dll))."
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

(defun esh-tree--tags-from-annotated-ranges (ranges props)
  "Enumerate opening and closing tags from annotated RANGES.
Each returned element has one of the following forms:
  (\\='text START END BUFFER)
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
        (let ((text-node `((text ,start ,end ,(current-buffer)))))
          ;; `nreverse' ensures that closers are enumerated in reverse order
          ;; from openers
          (setq tags (nconc text-node (nreverse open-tags) close-tags tags))))
      (setq prev-end end)
      (setq prev-alist alist))
    (dolist (pair prev-alist)
      (push `(close ,prev-end ,pair) tags))
    (nreverse tags)))

(defun esh-tree--tag-property (tag)
  "Read property changed by TAG."
  (car (nth 2 tag)))

(defun esh-tree--find-conflicts-1 (tag dll conflicts)
  "Find conflicts for closer TAG in DLL.
Corresponding opener is removed.  Conflicts are added to
CONFLICTS.  Returns (DLL . CONFLICTS)."
  (let ((pointer dll)
        (prop (esh-tree--tag-property tag)))
    (catch 'found
      (while t
        (let* ((prev-tag (esh-tree--dll-val pointer))
               (prev-kind (car prev-tag)))
          (cl-assert (eq prev-kind 'open) t "Unexpected: %S" prev-tag)
          (if (eq (esh-tree--tag-property prev-tag) prop)
              (throw 'found nil)
            (push (cons prev-tag tag) conflicts)))
        (setq pointer (esh-tree--dll-next pointer))))
    (if (eq pointer dll)
        ;; Opener was just before closer
        (setq dll (esh-tree--dll-pop pointer))
      (esh-tree--dll-pop pointer))
    (cons dll conflicts)))

(defun esh-tree--find-conflicts (tags)
  "Find conflicting spans in TAGS.
Two tags conflict if the overlap, but neither is included in the
other.  Returns results as a list of (OPENER-1 . CLOSER-2)."
  (let ((dll nil)
        (conflicts nil))
    (dolist (tag tags)
      (pcase (car tag)
        (`text)
        (`open (esh-tree--dll-push-front tag dll))
        (`close
         (let ((res (esh-tree--find-conflicts-1 tag dll conflicts)))
           (setq dll (car res) conflicts (cdr res))))))
    conflicts))

(defconst esh--latex-priorities
  `(;; Fine to split:
    ,@'(line-height newline :underline :foreground :weight :height :background)
    ;; Should not split
    ,@'(:slant :box display invisible))
  "Priority order for text properties in LaTeX export.
See `esh-tree--resolve-conflicts'.")

(defun esh-tree--build-priorities (priorities)
  "Create a hashmap mapping keys of PRIORITIES to their index in the list."
  (let ((idx 0)
        (pri (make-hash-table :test #'eq)))
    (dolist (symb priorities)
      (puthash symb (setq idx (1+ idx)) pri))
    pri))

(defvar esh-tree--priorities nil
  "Internal variable used to pass priority maps around.")

(defun esh-tree--tag-cmp (t1 t2)
  "Compare two tags T1 and T2."
  (let ((pos1 (nth 1 t1)) (pos2 (nth 1 t2)))
    (cond
     ((< pos1 pos2) -1)
     ((> pos1 pos2) 1)
     (t (let* ((types '(close open text))
               (type1 (car t1)) (type2 (car t2))
               (tp1 (cl-position type1 types))
               (tp2 (cl-position type2 types)))
          (cond
           ((< tp1 tp2) -1)
           ((> tp1 tp2) 1)
           (t (cl-assert (not (eq type1 'text)))
              (let* ((pri1 (gethash (esh-tree--tag-property t1) esh-tree--priorities))
                     (pri2 (gethash (esh-tree--tag-property t2) esh-tree--priorities))
                     (highest-priority-first (eq type1 'open)))
                (cond
                 ((< pri1 pri2) (if highest-priority-first 1 -1))
                 ((> pri1 pri2) (if highest-priority-first -1 1))
                 (t 0))))))))))

(defun esh-tree--tag-less-p (t1 t2)
  "Compare two tags T1 and T2 for sorting."
  (< (esh-tree--tag-cmp t1 t2) 0))

(defun esh-tree--compute-tags-to-resolve-conflicts (conflicts)
  "Compute a list of additional openers and closers to resolve CONFLICTS.
`esh-tree--priorities' is used to resolve conflicts, i.e. to determine which
span to split when two spans intersect without nesting.  Earlier
properties are split first."
  (let ((more-tags nil))
    (pcase-dolist (`((open ,op-pos ,op-pr) . (close ,cl-pos ,cl-pr)) conflicts)
      (let* ((op-pri (gethash (car op-pr) esh-tree--priorities))
             (cl-pri (gethash (car cl-pr) esh-tree--priorities)))
        (unless op-pri (error "Unknown property %S" (car op-pr)))
        (unless cl-pri (error "Unknown property %S" (car cl-pr)))
        (let ((tl (if (<= op-pri cl-pri) `(,cl-pos ,op-pr) `(,op-pos ,cl-pr))))
          (push (cons 'close tl) more-tags)
          (push (cons 'open tl) more-tags))))
    (sort more-tags #'esh-tree--tag-less-p)))

(defun esh-tree--merge-sorted-tags (ts1 ts2)
  "Merge two sorted lists of tags TS1 and TS2."
  (let ((acc nil))
    (while (or ts1 ts2)
      (cond
       ((and ts1 ts2)
        (if (esh-tree--tag-less-p (car ts1) (car ts2))
            (push (pop ts1) acc)
          (push (pop ts2) acc)))
       (ts1 (push (pop ts1) acc))
       (ts2 (push (pop ts2) acc))))
    (nreverse acc)))

(defun esh-tree--resolve-conflicts (tags properties)
  "Enrich and return TAGS to remove overlapping spans.
See `esh-tree--compute-tags-to-resolve-conflicts' for info on
PROPERTIES."
  (let* ((conflicts (esh-tree--find-conflicts tags))
         (esh-tree--priorities (esh-tree--build-priorities properties))
         (extras (esh-tree--compute-tags-to-resolve-conflicts conflicts)))
    (esh-tree--merge-sorted-tags tags extras)))

(defun esh-tree--buffer-to-tags-list (text-props face-attrs priority-ranking)
  "Construct a properly nested list of tags from current buffer.
TEXT-PROPS and FACE-ATTRS specify which properties to keep track
of.  PRIORITY-RANKING ranks all these properties in order of
tolerance to splitting (if a property comes late in this list,
ESH will try to preserve this property's spans when resolving
conflicts."
  (let* ((ranges (esh--buffer-ranges))
         (annotated-ranges (esh-tree--annotate-ranges ranges text-props face-attrs))
         (tags (esh-tree--tags-from-annotated-ranges annotated-ranges priority-ranking))
         (unconflicted-tags (esh-tree--resolve-conflicts tags priority-ranking)))
    unconflicted-tags))

(defun esh-tree--export (exporter tags)
  "Call EXPORTER on `car' of list of TAGS until it returns nil.
On each iteration, the return value of EXPORTER is used as the next STREAM."
  (while tags
    (setq tags (funcall exporter tags))))

(defun esh-tree--drop-until-close (prop stream)
  "Drop elements of STREAM until a closer for PROP is found."
  (let ((depth 1))
    (while (> depth 0)
      (pcase (car (pop stream))
        (`nil (error "No closing tag found for %S" prop))
        (`open (cl-incf depth))
        (`close (cl-decf depth)))))
  stream)

(provide 'esh-tree)
;;; esh-tree.el ends here
