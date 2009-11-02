;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct poly-axis
  normal distance)

(defstruct (poly (:constructor %make-poly (body vertices offset))
                       (:include shape))
  vertices axes transformed-vertices transformed-axes)

(defun make-poly (body vertices offset)
  (let ((poly (%make-poly body verticel offset)))
    poly))

(defun validate-vertices (vertices)
  "Check that a set of vertices has a correct winding, and that they form a convex polygon."
  ;; todo
  )