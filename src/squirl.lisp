;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defun moment-of-inertia-for-circle (mass inner-diameter outer-diameter &optional (offset +zero-vector+))
  "Calculate the moment of inertia for a circle.
A solid circle has an inner diameter of 0."
  (+ (* mass 1/2 (+ (expt inner-diameter 2) (expt outer-diameter 2)))
     (* mass (vec-length-sq offset))))

(defun moment-of-inertia-for-segment (mass point-a point-b)
  "Calculate the moment of inertia for a line segment connecting POINT-A to POINT-B."
  (let ((length (vec-length (vec- point-b point-a))))
    (+ (* mass length (/ length 12))
       (* mass (vec-length-sq (vec* (vec+ point-a point-b) 0.5d0))))))

(defun moment-of-inertia-for-poly (m verts &optional (offset +zero-vector+))
  "Calculate the moment of inertia for a solid convex polygon."
  (flet ((transform-vertex (vertex) (vec+ vertex offset)))
    (loop
       for vertex in verts
       for v1 = (transform-vertex vertex)
       and v2 = (transform-vertex (car (last verts))) then v1
       for a = (vec-cross v2 v1) sum a into sum2
       sum (* a (+ (vec. v1 v1) (vec. v1 v2) (vec. v2 v2))) into sum1
       finally (return (/ (* m sum1) (* 6 sum2))))))

