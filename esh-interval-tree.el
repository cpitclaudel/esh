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
;; document tree.  Binary branching nodes do not carry semantic information:
;; they are just used to improve the complexity of insertion operations.
;; Conversely, annotation notes and text node carry information (in the text
;; case a range corresponding to a string of the original buffer, and in the
;; annotation case an annotation — in practice, a (property . value) pair).
;;
;; Interval trees are described by the following inductive type:
;;
;; Inductive Tree :=
;; | Empty : Tree
;; | Text : ∀ (low high: ℕ), Tree
;; | Branch : ∀ (low high: ℕ) (l r: Tree), Tree
;; | Annotation : ∀ (low high: ℕ) (annotation: A) (t: Tree), Tree.
;;
;; Flattening them removes the information-less nodes, yielding the following
;; inductive type:
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
     (:constructor esh-interval-tree--text (low high)))
  low high)
(cl-defstruct
    (esh-interval-tree--branch
     (:constructor nil)
     (:constructor esh-interval-tree--branch (low high left right)))
  low high left right)
(cl-defstruct
    (esh-interval-tree--annotation
     (:constructor nil)
     (:constructor esh-interval-tree--annotation (low high annot subtree)))
  low high annot subtree)

(defsubst esh-interval-tree--empty-p (tree)
  "Check if TREE is the empty tree."
  (eq tree esh-interval-tree--empty))

(defsubst esh-interval-tree--low (tree)
  "Extract LOW bound of non-empty TREE."
  (aref tree 1))

(defsubst esh-interval-tree--high (tree)
  "Extract LOW bound of non-empty TREE."
  (aref tree 2))

(defun esh-interval-tree-new (low high)
  "Make a new text node with bounds LOW and HIGH."
  (esh-interval-tree--text low high))

(defun esh-interval-tree--mk-branch (t1 t2)
  "Merge interval trees T1 and T2."
  (cond
   ((esh-interval-tree--empty-p t1) t2)
   ((esh-interval-tree--empty-p t2) t1)
   (t (esh-interval-tree--branch (esh-interval-tree--low t1) (esh-interval-tree--high t2) t1 t2))))

(defun esh-interval-tree--mk-annotation (annot tree)
  "Construct an annotation node (_ _ ANNOT TREE)."
  (cond
   ((esh-interval-tree--empty-p tree) esh-interval-tree--empty)
   (t (esh-interval-tree--annotation (esh-interval-tree--low tree) (esh-interval-tree--high tree) annot tree))))

(defun esh-interval-tree--split (tree threshold)
  "Split TREE around THRESHOLD.
Constructs two interval trees T1 and T2 satisfying the equation
T1 < THRESHOLD <= T2.  Returns a cons (T1 . T2)."
  (cond
   ((esh-interval-tree--empty-p tree) (cons esh-interval-tree--empty esh-interval-tree--empty))
   ((esh-interval-tree--text-p tree)
    (let ((low (esh-interval-tree--low tree)) (high (esh-interval-tree--high tree)))
      (cond
       ((<= threshold low) (cons esh-interval-tree--empty tree))
       ((<= high threshold) (cons tree esh-interval-tree--empty))
       (t (cons (esh-interval-tree--text low threshold)
                (esh-interval-tree--text threshold high))))))
   ((esh-interval-tree--branch-p tree)
    (let ((low (esh-interval-tree--low tree)) (high (esh-interval-tree--high tree)))
      (cond
       ((<= threshold low) (cons esh-interval-tree--empty tree))
       ((<= high threshold) (cons tree esh-interval-tree--empty))
       (t (pcase-let* ((`(,llow . ,lhigh)
                        (esh-interval-tree--split (esh-interval-tree--branch-left tree) threshold))
                       (`(,rlow . ,rhigh)
                        (esh-interval-tree--split (esh-interval-tree--branch-right tree) threshold)))
            ;; At least one of the four nodes is empty
            (cons (esh-interval-tree--mk-branch llow rlow)
                  (esh-interval-tree--mk-branch lhigh rhigh)))))))
   ((esh-interval-tree--annotation-p tree)
    (let* ((annot (esh-interval-tree--annotation-annot tree))
           (lr (esh-interval-tree--split (esh-interval-tree--annotation-subtree tree) threshold)))
      (cons (esh-interval-tree--mk-annotation annot (car lr))
            (esh-interval-tree--mk-annotation annot (cdr lr)))))
   (t (error "Unexpected tree %S" tree))))

(defun esh-interval-tree--subset (l1 h1 l2 h2)
  "Check whether [L1, H1[ ⊆ [L2, H2[."
  (and (<= l2 l1) (<= h1 h2)))

(defun esh-interval-tree--add-no-split (annot low high tree)
  "Insert interval LOW .. HIGH annotated with ANNOT into TREE.
This function doesn't split TREE (but it may split subtrees of
TREE); instead, it returns nil if it can't insert without
splitting TREE."
  (cond
   ;; Nothing interesting to do in these two cases
   ((or (esh-interval-tree--empty-p tree) (esh-interval-tree--text-p tree)) nil)
   ;; If annotation covers entire tree, stop recursing
   ((esh-interval-tree--subset (esh-interval-tree--low tree) (esh-interval-tree--high tree) low high)
    (esh-interval-tree--annotation low high annot tree))
   ;; Otherwise, try to tuck new annotation under existing one
   ((esh-interval-tree--annotation-p tree)
    (and (esh-interval-tree--subset low high (esh-interval-tree--low tree) (esh-interval-tree--high tree))
         (esh-interval-tree--annotation (esh-interval-tree--low tree) (esh-interval-tree--high tree) (esh-interval-tree--annotation-annot tree)
                       (esh-interval-tree-add annot low high (esh-interval-tree--annotation-subtree tree)))))
   ;; …or inside one of the branches of the existing branch
   ((esh-interval-tree--branch-p tree)
    (and (esh-interval-tree--subset low high (esh-interval-tree--low tree) (esh-interval-tree--high tree))
         (or (let ((ll (esh-interval-tree--add-no-split annot low high (esh-interval-tree--branch-left tree))))
               ;; Insert in left branch…
               (and ll (esh-interval-tree--branch (esh-interval-tree--low tree) (esh-interval-tree--high tree)
                                 ll (esh-interval-tree--branch-right tree))))
             (let ((rr (esh-interval-tree--add-no-split annot low high (esh-interval-tree--branch-right tree))))
               ;; …or in right branch
               (and rr (esh-interval-tree--branch (esh-interval-tree--low tree) (esh-interval-tree--high tree)
                                 (esh-interval-tree--branch-left tree) rr))))))
   (t (error "Unexpected tree %S" tree))))

(defun esh-interval-tree-add (annot low high tree)
  "Insert interval LOW .. HIGH annotated with ANNOT into TREE."
  (if (esh-interval-tree--empty-p tree) esh-interval-tree--empty
    (or (esh-interval-tree--add-no-split annot low high tree)
        (pcase-let* ((`(,l   . ,rest) (esh-interval-tree--split tree low))
                     (`(,middle . ,r) (esh-interval-tree--split rest high))
                     (annotated (esh-interval-tree--mk-annotation annot middle)))
          (cl-assert (not (eq middle esh-interval-tree--empty)))
          (esh-interval-tree--mk-branch l (esh-interval-tree--mk-branch annotated r))))))

(defun esh-interval-tree--flatten-1 (tree acc)
  "Flatten TREE, adding nodes to ACC."
  (cond
   ((esh-interval-tree--empty-p tree) acc)
   ((esh-interval-tree--text-p tree)
    (cons `(text ,(esh-interval-tree--low tree) ,(esh-interval-tree--high tree)) acc))
   ((esh-interval-tree--branch-p tree)
    (esh-interval-tree--flatten-1 (esh-interval-tree--branch-left tree) (esh-interval-tree--flatten-1 (esh-interval-tree--branch-right tree) acc)))
   ((esh-interval-tree--annotation-p tree)
    (cons `(tag ,(esh-interval-tree--annotation-annot tree)
                ,@(esh-interval-tree--flatten-1 (esh-interval-tree--annotation-subtree tree) nil))
          acc))))

(defun esh-interval-tree-flatten (tree)
  "Flatten TREE into a list of flat trees (see commentary)."
  (esh-interval-tree--flatten-1 tree nil))

(provide 'esh-interval-tree)
;;; esh-interval-tree.el ends here
