;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (pin-joint (:include constraint)
                      (:constructor
                       make-pin-joint
                       (body-a body-b anchor1 anchor2 &aux
                               (point-1 (vec+ (body-position body-a)
                                              (vec-rotate anchor1 (body-rotation body-a))))
                               (point-2 (vec+ (body-position body-b)
                                              (vec-rotate anchor2 (body-rotation body-b))))
                               (distance (vec-length (vec- point-1 point-2))))))
  anchor1 anchor2
  distance
  r1 r2
  normal
  n-mass
  (jn-acc 0d0)
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

(defmethod apply-impulse ((joint pin-joint))
  (let* ((body-a (pin-joint-body-a joint))
         (body-b (pin-joint-body-b joint))
         (normal (pin-joint-normal joint))
         ;; compute relative velocity
         (relative-velocity (normal-relative-velocity body-a body-b
                                                      (pin-joint-r1 joint)
                                                      (pin-joint-r2 joint)
                                                      normal))
         ;; copmute normal impulse
         (jn (* (- (pin-joint-bias joint) relative-velocity)
                (pin-joint-n-mass joint)))
         (jn-old (pin-joint-jn-acc joint)))
    (setf (pin-joint-jn-acc joint) (clamp (+ jn jn-old)
                                          (- (pin-joint-jn-max joint))
                                          (pin-joint-jn-max joint))
          jn (- (pin-joint-jn-acc joint) jn-old))
    (apply-impulses body-a body-b (pin-joint-r1 joint) (pin-joint-r2 joint) (vec* normal jn)))
  (values))

(defmethod get-impulse ((joint pin-joint))
  (abs (pin-joint-jn-acc joint)))

