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

(defmethod pre-step ((joint pin-joint) dt dt-inverse)
  (let ((body-a (pin-joint-body-a joint))
        (body-b (pin-joint-body-b joint)))
    (setf (pin-joint-r1 joint) (vec-rotate (pin-joint-anchor1 joint)
                                           (body-rotation body-a))
          (pin-joint-r2 joint) (vec-rotate (pin-joint-anchor2 joint)
                                           (body-rotation body-b)))
    (let* ((delta (vec- (vec+ (body-position body-b)
                              (pin-joint-r2 joint))
                        (vec+ (body-position body-a)
                              (pin-joint-r2 joint))))
           (distance (vec-length delta))
           (max-bias (pin-joint-max-bias joint)))
      (setf (pin-joint-normal joint) (vec* delta (/ (if (zerop distance)
                                                        most-positive-double-float
                                                        distance)))
            (pin-joint-n-mass joint) (/ (k-scalar body-a body-b
                                                  (pin-joint-r1 joint)
                                                  (pin-joint-r2 joint)
                                                  (pin-joint-normal joint)))
            (pin-joint-bias joint) (clamp (- (* (pin-joint-bias-coefficient joint)
                                                dt-inverse
                                                (- distance (pin-joint-distance joint))))
                                          (- max-bias) max-bias)
            (pin-joint-jn-max joint) (impulse-max joint dt))
      (apply-impulses body-a body-b (pin-joint-r1 joint) (pin-joint-r2 joint)
                      (vec* (pin-joint-normal joint) (pin-joint-jn-acc joint)))))
  (values))

