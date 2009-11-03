;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (pin-joint (:include constraint)
                      (:constructor
                       make-pin-joint
                       (body-a body-b anchor1 anchor2
                               &aux (distance
                                     (let ((p1 (vec+ (body-position body-a)
                                                     (vec-rotate anchor1 (body-rotation body1))))
                                           (p2 (vec+ (body-position body-b)
                                                     (vec-rotate anchor2 (body-rotation body2)))))
                                       (vec-length (vec- p1 p2)))))))
  anchor1 anchor2
  distance
  r1 r2
  normal
  n-mass
  (jn-acc 0)
  jn-max
  bias)

