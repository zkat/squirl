;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defmacro fun (&body body)
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

;; from alexandria:
(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence delete-form &rest keyword-arguments)
  (apply #'delete-if delete-form sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")

(define-modify-macro delete-iff (predicate &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE-IF. Sets place designated by the first argument to
the result of calling DELETE with PREDICATE, place, and the REMOVE-KEYWORDS.")

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect `(,var (gensym ,(symbol-name var))))
     ,@body))

(defmacro push-cons (cons place)
  "Like `cl:push', but reuses CONS"
  (with-gensyms (cons-sym)
    `(let ((,cons-sym ,cons))
       (setf (cdr ,cons-sym) ,place
             ,place ,cons-sym))))

(defun expt-mod (b e m &aux (result 1))
  (do ((expt e (ash expt -1))
       (base b (mod (* base base) m)))
      ((zerop expt) result)
    (when (oddp expt)
      (setf result (mod (* result base) m)))))

(defmacro define-constant (name value &optional doc)
  "ANSI-compliant replacement for `defconstant'. cf SBCL Manual 2.3.4."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun maybe/ (a b)
  (if (zerop b) 0 (/ a b)))
