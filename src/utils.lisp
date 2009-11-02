;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

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

(defmacro push-cons (cons place)
  "Like `cl:push', but reuses CONS"
  (let ((cons-sym (gensym)))
    `(let ((,cons-sym ,cons))
       (setf (cdr ,cons-sym) ,place
             ,place ,cons-sym))))
