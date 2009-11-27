;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (inline make-contact))
(defstruct (contact (:constructor make-contact (point normal distance &optional hash)))
  ;; Contact point and normal
  (point +zero-vector+ :type vec)
  (normal +zero-vector+ :type vec)
  ;; Penetration distance
  (distance 0d0 :type double-float)
  ;; Calculated by arbiter-prestep
  (r1 +zero-vector+ :type vec)
  (r2 +zero-vector+ :type vec)
  (normal-mass 0d0 :type double-float)
  (tangent-mass 0d0 :type double-float)
  (bounce 0d0 :type double-float)
  ;; Persistant contact information
  (accumulated-normal-impulse 0d0 :type double-float)
  (accumulated-frictional-impulse 0d0 :type double-float)
  (impulse-bias 0d0 :type double-float)
  (bias 0d0 :type double-float)
  ;; Hash value used as a (mostly) unique ID
  (hash 0 :type fixnum))

(defun contacts-sum-impulses (&rest contacts)
  (reduce #'vec+ contacts :initial-value +zero-vector+
          :key (fun (vec* (contact-normal _)
                          (contact-accumulated-normal-impulse _)))))

(defun contact-impulse-with-friction (contact)
  (vec-rotate (contact-normal contact)
              (vec (contact-accumulated-normal-impulse contact)
                   (contact-accumulated-frictional-impulse contact))))

(defun contacts-sum-impulses-with-friction (&rest contacts)
  (reduce #'vec+ contacts :initial-value +zero-vector+
          :key #'contact-impulse-with-friction))

(defun contacts-estimate-crushing-impulse (&rest contacts)
  (loop for contact in contacts
     for impulse = (contact-impulse-with-friction contact)
     for vec-sum = +zero-vector+ then (vec+ vec-sum impulse)
     sum (vec-length impulse) into scalar-sum
     finally (return (- 1 (/ (vec-length vec-sum) scalar-sum)))))

