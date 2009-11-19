;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defun moment-for-circle (mass inner-diameter outer-diameter &optional (offset +zero-vector+))
  "Calculate the moment of inertia for a circle.
A solid circle has an inner diameter of 0."
  (+ (* mass 1/2 (+ (expt inner-diameter 2) (expt outer-diameter 2)))
     (* mass (vec-length-sq offset))))

(defun moment-for-segment (mass point-a point-b)
  "Calculate the moment of inertia for a line segment connecting POINT-A to POINT-B."
  (let ((length (vec-length (vec- point-b point-a))))
    (+ (* mass length (/ length 12))
       (* mass (vec-length-sq (vec* (vec+ point-a point-b) 0.5d0))))))

(defun moment-for-poly (m verts &optional (offset +zero-vector+)
                        &aux (num-verts (length verts))
                             (t-verts (make-array num-verts)))
  "Calculate the moment of inertia for a solid convex polygon."
  (dotimes (i num-verts)
    (setf (svref t-verts i) (vec+ (elt verts i) offset)))
  (loop
     for i below num-verts
     for v1 across t-verts
     for v2 = (svref t-verts (mod (1+ i) num-verts))
     for a = (vec-cross v2 v1)
     for b = (+ (vec. v1 v1) (vec. v1 v2) (vec. v2 v2))
     sum (* a b) into sum1
     sum a into sum2
     finally (return (/ (* m sum1)
                        (* 6 sum2)))))
