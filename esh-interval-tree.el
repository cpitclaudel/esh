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
T1 < THRESHOLD <= T2.  Returns a cons (T1 . T2)."
  ;; (cl-incf ~/counter)
  ;; (when (> ~/counter 1212)
  ;;   (error "Split %d %S" threshold tree))
  (if (esh-interval-tree--empty-p tree) (cons esh-interval-tree--empty esh-interval-tree--empty)
    (let ((low (esh-interval-tree--low tree)) (high (esh-interval-tree--high tree)))
      (cond
       ((<= threshold low)
        (cons esh-interval-tree--empty tree))
       ((<= high threshold)
        (cons tree esh-interval-tree--empty))
       ((esh-interval-tree--text-p tree)
        ;; FIXME could (cl-assert nil) here if tree was pre-populated with all text segments
        (cons (esh-interval-tree--text low threshold (esh-interval-tree--annots tree))
              (esh-interval-tree--text threshold high (esh-interval-tree--annots tree))))
       ((esh-interval-tree--branch-p tree)
        (pcase-let* ((`(,llow . ,lhigh)
                      (esh-interval-tree--split (esh-interval-tree--branch-left tree) threshold))
                     (`(,rlow . ,rhigh)
                      (esh-interval-tree--split (esh-interval-tree--branch-right tree) threshold)))
          ;; At least one of the four nodes is empty
          (cons (esh-interval-tree--annotate (esh-interval-tree--annots tree) (esh-interval-tree--merge llow rlow))
                (esh-interval-tree--annotate (esh-interval-tree--annots tree) (esh-interval-tree--merge lhigh rhigh)))))))))

(defun esh-interval-tree--subset (l1 h1 l2 h2)
  "Check whether [L1, H1[ ⊆ [L2, H2[."
  (and (<= l2 l1) (<= h1 h2)))

(defun esh-interval-tree--add-no-split (annot low high tree)
  "Insert interval LOW .. HIGH annotated with ANNOT into TREE.
This function doesn't split TREE (but it may split subtrees of
TREE and mutate TREE).  It returns nil if it can't insert without
splitting the top node of TREE."
  (cond
   ;; Nothing interesting to do in these two cases
   ((or (esh-interval-tree--empty-p tree) (esh-interval-tree--text-p tree)) nil)
   ;; If annotation covers entire tree, stop recursing
   ((esh-interval-tree--subset (esh-interval-tree--low tree) (esh-interval-tree--high tree) low high)
    ;; (Invariant below is due to intervals always covering a text range)
    (cl-assert (and (= (esh-interval-tree--low tree) low) (= (esh-interval-tree--high tree) high)))
    (esh-interval-tree--annotate-1 annot tree))
   ;; Otherwise, try to annotate just one of the subtrees
   ((esh-interval-tree--branch-p tree)
    (and (esh-interval-tree--subset low high (esh-interval-tree--low tree) (esh-interval-tree--high tree))
         (or (let ((ll (esh-interval-tree--add-no-split annot low high (esh-interval-tree--branch-left tree))))
               (and ll (setf (esh-interval-tree--branch-left tree) ll))) ;; Insert in left branch
             (let ((rr (esh-interval-tree--add-no-split annot low high (esh-interval-tree--branch-right tree))))
               (and rr (setf (esh-interval-tree--branch-right tree) rr)))) ;; …or in right branch
         tree))
   (t (error "Unexpected tree %S" tree))))

(defun esh-interval-tree-add (annot low high tree)
  "Insert interval LOW .. HIGH annotated with ANNOT into TREE."
  (if (esh-interval-tree--empty-p tree) esh-interval-tree--empty
    (or (esh-interval-tree--add-no-split annot low high tree)
        (pcase-let* ((`(,l   . ,rest) (esh-interval-tree--split tree low))
                     (`(,middle . ,r) (esh-interval-tree--split rest high))
                     (annotated (esh-interval-tree--annotate-1 annot middle)))
          (cl-assert (not (eq middle esh-interval-tree--empty)))
          (esh-interval-tree--merge l (esh-interval-tree--merge annotated r))))))

(defun esh-interval-tree--flat-annotate (annotations trees)
  "Annotate TREES with all ANNOTATIONS.
Returns a single node.  ANNOTATIONS must be non-nil."
  (cl-assert annotations)
  (dolist (ann (reverse annotations))
    (setq trees `((tag ,ann ,@trees))))
  (car trees))

(defun esh-interval-tree--flatten-1 (tree acc)
  "Flatten TREE, adding nodes to ACC."
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
              (let ((flat (esh-interval-tree--flatten-1 l (esh-interval-tree--flatten-1 r nil))))
                (cons (esh-interval-tree--flat-annotate annots flat) acc))
            (esh-interval-tree--flatten-1 l (esh-interval-tree--flatten-1 r acc)))))
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
  (esh-interval-tree--flatten-1 tree nil))

(provide 'esh-interval-tree)
;;; esh-interval-tree.el ends here
