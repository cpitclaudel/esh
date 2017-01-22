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

(defun esh-interval-tree--branch (t1 t2)
  "Merge interval trees T1 and T2."
  (cond
   ((eq t1 'empty) t2)
   ((eq t2 'empty) t1)
   (t (pcase-let ((`(,_ ,low ,_  . ,_) t1)
                  (`(,_ ,_ ,high . ,_) t2))
        `(branch ,low ,high ,t1 ,t2)))))

(defun esh-interval-tree--annotation (annot tree)
  "Construct an annotation node (_ _ ANNOT TREE)."
  (pcase tree
    (`empty 'empty)
    (`(,_ ,low ,high . ,_) `(annotation ,low ,high ,annot ,tree))))

(defun esh-interval-tree--split (tree threshold)
  "Split TREE around THRESHOLD.
Constructs two interval trees T1 and T2 satisfying the equation
T1 < THRESHOLD <= T2.  Returns a cons (T1 . T2)."
  (pcase tree
    (`empty (cons 'empty 'empty))
    (`(text ,low ,high)
     (cond
      ((<= threshold low) (cons 'empty tree))
      ((<= high threshold) (cons tree 'empty))
      (t (cons `(text ,low ,threshold)
               `(text ,threshold ,high)))))
    (`(branch ,low ,high ,l ,r)
     (cond
      ((<= threshold low) (cons 'empty tree))
      ((<= high threshold) (cons tree 'empty))
      (t (pcase-let* ((`(,llow . ,lhigh) (esh-interval-tree--split l threshold))
                      (`(,rlow . ,rhigh) (esh-interval-tree--split r threshold)))
           ;; At least one of the four nodes is empty
           (cons (esh-interval-tree--branch llow rlow)
                 (esh-interval-tree--branch lhigh rhigh))))))
    (`(annotation ,_ ,_ ,annot ,tree)
     (pcase-let* ((`(,l . ,r) (esh-interval-tree--split tree threshold)))
       (cons (esh-interval-tree--annotation annot l)
             (esh-interval-tree--annotation annot r))))))

(defun esh-interval-tree-add (annot low high tree)
  "Insert interval LOW .. HIGH annotated with ANNOT into TREE."
  (pcase-let* ((`(,l   . ,rest) (esh-interval-tree--split tree low))
               (`(,middle . ,r) (esh-interval-tree--split rest high))
               (annotated (esh-interval-tree--annotation annot middle)))
    (cl-assert (not (eq middle 'empty)))
    (esh-interval-tree--branch l (esh-interval-tree--branch annotated r))))

(defun esh-interval-tree--flatten-1 (tree acc)
  "Flatten TREE, adding nodes to ACC."
  (pcase tree
    (`empty acc)
    (`(text ,_ ,_)
     (cons tree acc))
    (`(branch ,_ ,_ ,l ,r)
     (esh-interval-tree--flatten-1 l (esh-interval-tree--flatten-1 r acc)))
    (`(annotation ,_ ,_ ,annot ,tr)
     (cons `(tag ,annot . ,(esh-interval-tree--flatten-1 tr nil)) acc))))

(defun esh-interval-tree-flatten (tree)
  "Flatten TREE into a list of flat trees (see commentary)."
  (esh-interval-tree--flatten-1 tree nil))

(provide 'esh-interval-tree)
;;; esh-interval-tree.el ends here
