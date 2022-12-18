(defpackage :proto-dot
  (:use :cl :proto-dot.tools)
  (:export #:proto-dot
           #:proto-dot?
           #:proto-fdot
           #:proto-fdot?

           #:make-dot-expander
           #:define-dot-expander

           #:expand-form
           #:generic-expand-form

           #:*default-dot-patterns*
           #:default-dot-expander))

(in-package :proto-dot)

;;; An expander is one of two things:

;; 1. A symbol that names a function of two arguments, where the first
;; argument is any object and the second an operation that will be
;; applied to the object. The function will return an expansion
;; corresponding to the operation, or nil if the operation cannot be
;; performed.
;;
;; 2. An object of class C for which a method with specializers (C T T)
;; on the generic function GENERIC-EXPAND-FORM exists.

(defgeneric generic-expand-form (expander object form))

(defun expand-form (expander object operation)
  (typecase expander
    ((or symbol function)
     (funcall expander object operation))
    (t (generic-expand-form expander object operation))))

(defun make-dot-expander (patterns)
  (lambda (object operation)
    (or (loop :for pattern :in patterns
            :thereis (funcall pattern object operation))
        (error "No pattern matched ~S." operation))))

(defmacro define-dot-expander (name patterns)
  `(setf (fdefinition ',name) (make-dot-expander ,patterns)))

;;; Default expander

(defparameter *default-dot-patterns* '(underscore-expression-pattern
                                       symbol-expression-pattern
                                       first-argument-expression-pattern
                                       error-pattern))

(define-dot-expander default-dot-expander *default-dot-patterns*)

;;; Proto dots

(defun generate-proto-dot-form (expander object path)
  (if path
      (destructuring-bind (first &rest rest) path
        (let ((op (expand-form expander object first)))
          (generate-proto-dot-form expander op rest)))
      object))

(defmacro proto-dot (expander object &rest path)
  (generate-proto-dot-form expander object path))

(defmacro proto-fdot (expander &rest path)
  (alexandria:with-gensyms (object)
    `(lambda (,object)
       (proto-dot ,expander ,object ,@path))))

(defmacro proto-dot? (expander object &rest path)
  (let* ((block-name (gensym "PROTO-DOT?"))
         (return-on-nil-expander
          (lambda (object operation)
            (let ((expansion (expand-form expander object operation)))
              `(or ,expansion (return-from ,block-name))))))
    `(block ,block-name
       ,(generate-proto-dot-form return-on-nil-expander object path))))

(defmacro proto-fdot? (expander &rest path)
  (alexandria:with-gensyms (object)
    `(lambda (,object)
       (proto-dot? ,expander ,object ,@path))))
