;;; This file provides some tools for building patterns, as well as
;;; some patterns for the default expander.

(defpackage :proto-dot.tools
  (:use :cl)
  (:export #:symbol-pattern
           #:first-argument-pattern
           #:underscore-pattern
           #:diamond-pattern
           #:error-pattern

           #:walk-tree-elements
           #:map-tree
           #:tree-contains-p
           #:symbol-with-name-p
           #:quoted-symbol-p
           #:keyword-or-quoted-symbol-p))

(in-package :proto-dot.tools)

;;; Utils

(defun walk-tree-elements (fn tree)
  (labels ((walk (tr)
             (typecase tr
               (cons
                (walk (car tr))
                (walk (cdr tr)))
               (t (funcall fn tr)))))
    (walk tree)))

(defun map-tree (fn tree)
  (labels ((walk (tr)
             (typecase tr
               (cons
                (cons (walk (car tr))
                      (walk (cdr tr))))
               (t (funcall fn tr)))))
    (walk tree)))

(defun tree-contains-p (predicate tree)
  (walk-tree-elements
   (lambda (e)
     (when (funcall predicate e)
       (return-from tree-contains-p t)))
   tree))

(defun symbol-with-name-p (symbol name)
  (and (symbolp symbol)
       (string= (symbol-name symbol) name)))

(defun underscorep (x)
  (symbol-with-name-p x "_"))

(defun diamondp (x)
  (symbol-with-name-p x "<>"))

(defun quoted-symbol-p (thing)
    (and (consp thing)
         (= 2 (length thing))
         (eq 'quote (first thing))
         (symbolp (second thing))))

(defun keyword-or-quoted-symbol-p (thing)
    (or (keywordp thing)
        (quoted-symbol-p thing)))

;;; Patterns

;; A pattern is a function that tries to match a single step in the
;; "path" part of a dot expression. If successul, it returns the
;; expansion, otherwise NIL.

;; (dot a foo) -> (foo a)
(defun symbol-pattern (object expression)
  (when (symbolp expression)
    (list expression object)))

;; (dot a (foo b)) -> (foo a b)
(defun first-argument-pattern (object expression)
  (when (and (listp expression)
             (not (eq 'quote (first expression))))
    (list* (first expression)
           object
           (rest expression))))

(defun make-placeholder-pattern (placeholder)
  (lambda (object expression)
    (let (actual-symbol)
      (when (and (listp expression)
                 (not (eq 'quote (first expression)))
                 (tree-contains-p (lambda (x)
                                    (if (symbol-with-name-p x placeholder)
                                        (if actual-symbol
                                            t
                                            (prog1 t
                                              (setf actual-symbol x)))
                                        nil))
                                  expression))
        `(let ((,actual-symbol ,object))
           ,expression)))))

;; (dot a (foo b _)) -> (foo b a)
(setf (fdefinition 'underscore-pattern)
      (make-placeholder-pattern "_"))

;; (dot a (foo b <>)) -> (foo b a)
(setf (fdefinition 'diamond-pattern)
      (make-placeholder-pattern "<>"))

(defun error-pattern (thing op)
  (declare (ignore thing))
  (error "Invalid expression: ~S." op))
