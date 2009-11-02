;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

;; from alexandria:
(declaim (inline delete/swapped-arguments))
(defun delete/swapped-arguments (sequence predicate &rest keyword-arguments)
  (apply #'delete-if predicate sequence keyword-arguments))

(define-modify-macro delete-iff (predicate &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")
