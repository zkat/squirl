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

(defun num-vertices (poly)
  (length (poly-vertices poly)))

(defun nth-vertex (index poly)
  (elt (poly-vertices poly) index))

(defun poly-value-on-axis (poly normal distance)
  "Returns the minimum distance of the polygon to the axis."
  (- (loop for vertex in (poly-vertices poly)
        minimizing (vec. normal vertex))
     distance))
