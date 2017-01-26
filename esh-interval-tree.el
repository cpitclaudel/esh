;;; esh-interval-tree.el --- Utilities to manipulate trees of intervals  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Clément Pit-Claudel

;; Author: Clément Pit-Claudel <clement@clem-w50-mint>
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

;; Interval trees are a mix between binary search trees of intervals and a
;; document tree.  They are described by the following inductive type:
;;
;; Inductive Tree ≜
;; | Empty : Tree
;; | Text : ∀ (low high: ℕ) (annotations: list A), Tree
;; | Branch : ∀ (low high: ℕ) (annotations: list A) (l r: Tree), Tree.
;;
;; Branching nodes whose annotation list is empty do not carry semantic
;; information: they are just used to improve the complexity of insertion
;; operations.  Conversely, text nodes and branching nodes with non-empty
;; annotation lists do carry information.
;;
;; Flattening them removes the information-less nodes and unfolds the lists of
;; annotations, yielding the following inductive type:
;;
;; Inductive FlatTree :=
;; | Leaf : forall (from to: nat), FlatTree
;; | Node : forall (annotation: A) (children: list FlatTree), FlatTree.

;;; Code:

(require 'cl-lib) ;; cl-assert

(defconst esh-interval-tree--empty
  'empty)
(cl-defstruct
    (esh-interval-tree--text
     (:constructor nil)
     (:constructor esh-interval-tree--text (low high annots)))
  low high annots)
(cl-defstruct
    (esh-interval-tree--branch
     (:constructor nil)
     (:constructor esh-interval-tree--branch (low high annots left right)))
  low high annots left right)

(defvar esh-interval-tree-nest-annotations nil
  "How annotations are rendered in flattened trees.
When non-nil, each `tag' node contains a single annotation.
Otherwise, tag nodes contain lists of annotations.")

(defsubst esh-interval-tree--empty-p (tree)
  "Check if TREE is the empty tree."
  (eq tree esh-interval-tree--empty))

(defsubst esh-interval-tree--low (tree)
  "Extract lower bound of non-empty TREE."
  (aref tree 1))

(defsubst esh-interval-tree--high (tree)
  "Extract higher bound of non-empty TREE."
  (aref tree 2))

(defsubst esh-interval-tree--annots (tree)
  "Extract annotations of non-empty TREE."
  (aref tree 3))
(gv-define-setter esh-interval-tree--annots (v tree) `(aset ,tree 3 ,v))

(defun esh-interval-tree-new (low high)
  "Make a new text node with bounds LOW and HIGH."
  (esh-interval-tree--text low high nil))

(defun esh-interval-tree--merge (t1 t2)
  "Merge interval trees T1 and T2."
  (cond
   ((esh-interval-tree--empty-p t1) t2)
   ((esh-interval-tree--empty-p t2) t1)
   (t (esh-interval-tree--branch (esh-interval-tree--low t1) (esh-interval-tree--high t2) nil t1 t2))))

(defun esh-interval-tree--annotate (annots tree)
  "Add ANNOTS to TREE (mutably)."
  (unless (or (esh-interval-tree--empty-p tree) (null annots))
    (setf (esh-interval-tree--annots tree) (append annots (esh-interval-tree--annots tree))))
  tree)

(defun esh-interval-tree--annotate-1 (annot tree)
  "Add ANNOT to TREE (mutably)."
  (unless (esh-interval-tree--empty-p tree)
    (push annot (esh-interval-tree--annots tree)))
  tree)

(defun esh-interval-tree--split (tree threshold)
  "Split TREE around THRESHOLD.
Constructs two interval trees T1 and T2 satisfying the equation
T1 < THRESHOLD <= T2.  Returns a list (ANNOTS T1 T2)."
  ;; Returning annotations instead of applying them directly to both subtrees
  ;; allows the caller (`esh-interval-tree-add-split') to apply these annotations to a newly created
  ;; branch node, instead of duplicating them.
  (if (esh-interval-tree--empty-p tree) (list nil esh-interval-tree--empty esh-interval-tree--empty)
    (let ((low (esh-interval-tree--low tree)) (high (esh-interval-tree--high tree)))
      (cond
       ((<= threshold low)
        (list nil esh-interval-tree--empty tree))
       ((<= high threshold)
        (list nil tree esh-interval-tree--empty))
       ((esh-interval-tree--text-p tree)
        ;; Instead of splitting text segments here, one could pre-populate the
        ;; tree with all text ranges; then this branch would never be reached.
        (list (esh-interval-tree--annots tree)
              (esh-interval-tree--text low threshold nil)
              (esh-interval-tree--text threshold high nil)))
       ((esh-interval-tree--branch-p tree)
        (pcase-let* ((`(,lannots ,llow ,lhigh)
                      (esh-interval-tree--split (esh-interval-tree--branch-left tree) threshold))
                     (`(,rannots ,rlow ,rhigh)
                      (esh-interval-tree--split (esh-interval-tree--branch-right tree) threshold)))
          ;; At least one of the four nodes is empty
          (list (esh-interval-tree--annots tree)
                (esh-interval-tree--merge (esh-interval-tree--annotate lannots llow)
                         (esh-interval-tree--annotate rannots rlow))
                (esh-interval-tree--merge (esh-interval-tree--annotate lannots lhigh)
                         (esh-interval-tree--annotate rannots rhigh)))))))))

(defun esh-interval-tree--subset (l1 h1 l2 h2)
  "Check whether [L1, H1[ ⊆ [L2, H2[."
  (and (<= l2 l1) (<= h1 h2)))

(defun esh-interval-tree--add-split (annot low high tree)
  "Insert interval LOW .. HIGH annotated with ANNOT into TREE."
  (cl-assert (not (esh-interval-tree--empty-p tree)) t)
  (pcase-let* ((`(,lannots ,l ,rest) (esh-interval-tree--split tree low))
               (`(,rannots ,mid ,r) (esh-interval-tree--split rest high))
               (annotated (esh-interval-tree--annotate-1 annot mid)))
    (cl-assert (not (esh-interval-tree--empty-p mid)) t)
    (esh-interval-tree--annotate lannots
                (esh-interval-tree--merge l (esh-interval-tree--annotate rannots
                                       (esh-interval-tree--merge annotated r))))))

(defun esh-interval-tree--fits-in (low high tree)
  "Check whether LOW .. HIGH fits into bounds of TREE."
  (and (not (esh-interval-tree--empty-p tree))
       (esh-interval-tree--subset low high (esh-interval-tree--low tree) (esh-interval-tree--high tree))))

(defun esh-interval-tree-add (annot low high tree)
  "Insert interval LOW .. HIGH annotated with ANNOT into TREE.
This function will never split the range LOW .. HIGH; if needed,
it will reorganize and split TREE instead.  Range LOW .. HIGH
must be a subset of the range covered by TREE."
  (if (esh-interval-tree--empty-p tree) esh-interval-tree--empty
    (cl-assert (esh-interval-tree--fits-in low high tree))
    (cond
     ((esh-interval-tree--subset (esh-interval-tree--low tree) (esh-interval-tree--high tree) low high)
      (esh-interval-tree--annotate-1 annot tree))
     ((esh-interval-tree--text-p tree)
      (esh-interval-tree--add-split annot low high tree))
     ;; Otherwise, try to annotate just one of the subtrees
     ((esh-interval-tree--branch-p tree)
      (let ((bl (esh-interval-tree--branch-left tree))
            (br (esh-interval-tree--branch-right tree)))
        (cond
         ((esh-interval-tree--fits-in low high bl)
          (setf (esh-interval-tree--branch-left tree) (esh-interval-tree-add annot low high bl))
          tree)
         ((esh-interval-tree--fits-in low high br)
          (setf (esh-interval-tree--branch-right tree) (esh-interval-tree-add annot low high br))
          tree)
         (t (esh-interval-tree--add-split annot low high tree)))))
     (t (error "Unexpected tree %S" tree)))))

(defun esh-interval-tree--flat-annotate (annotations trees)
  "Annotate TREES with all ANNOTATIONS.
Returns a single node.  ANNOTATIONS must be non-nil.  Result
depends on `esh-interval-tree-nest-annotations': when non-nil,
this function generates one tag node per annotation in
ANNOTATIONS."
  (cl-assert annotations)
  (if (not esh-interval-tree-nest-annotations)
      `(tag ,annotations ,@trees)
    (dolist (ann (reverse annotations))
      (setq trees `((tag ,ann ,@trees))))
    (car trees)))

(defun esh-interval-tree-flatten-acc (tree acc)
  "Flatten TREE, adding nodes to front of ACC."
  (if (esh-interval-tree--empty-p tree) acc
    (let ((annots (esh-interval-tree--annots tree)))
      (cond
       ((esh-interval-tree--text-p tree)
        (let* ((txt `(text ,(esh-interval-tree--low tree) ,(esh-interval-tree--high tree)))
               (annotated (if annots (esh-interval-tree--flat-annotate annots `(,txt)) txt)))
          (cons annotated acc)))
       ((esh-interval-tree--branch-p tree)
        (let ((l (esh-interval-tree--branch-left tree))
              (r (esh-interval-tree--branch-right tree)))
          (if annots
              (let ((flat (esh-interval-tree-flatten-acc l (esh-interval-tree-flatten-acc r nil))))
                (cons (esh-interval-tree--flat-annotate annots flat) acc))
            (esh-interval-tree-flatten-acc l (esh-interval-tree-flatten-acc r acc)))))
       (t (error "Unexpected tree %S" tree))))))

(defun esh-interval-tree--depth (tree)
  "Compute the depth of TREE."
  (cond
   ((esh-interval-tree--empty-p tree) 0)
   ((esh-interval-tree--text-p tree) 1)
   ((esh-interval-tree--branch-p tree)
    (+ 1 (max (esh-interval-tree--depth (esh-interval-tree--branch-left tree)) (esh-interval-tree--depth (esh-interval-tree--branch-right tree)))))
   (t (error "Unexpected tree %S" tree))))

(defun esh-interval-tree--print (tree)
  "Print TREE."
  (cond
   ((esh-interval-tree--empty-p tree) (princ "∅"))
   ((esh-interval-tree--text-p tree) (princ (format "(%d %d)" (esh-interval-tree--low tree) (esh-interval-tree--high tree))))
   ((esh-interval-tree--branch-p tree)
    (princ (format "(b (%d %d) " (esh-interval-tree--low tree) (esh-interval-tree--high tree)))
    (esh-interval-tree--print (esh-interval-tree--branch-left tree))
    (princ " ")
    (esh-interval-tree--print (esh-interval-tree--branch-right tree))
    (princ ")"))
   (t (error "Unexpected tree %S" tree))))

(defun esh-interval-tree--write (tree fname)
  "Write TREE to file FNAME."
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (esh-interval-tree--print tree)
      (write-file fname))))

(defun esh-interval-tree-flatten (tree)
  "Flatten TREE into a list of flat trees (see commentary)."
  (esh-interval-tree-flatten-acc tree nil))

(provide 'esh-interval-tree)
;;; esh-interval-tree.el ends here
