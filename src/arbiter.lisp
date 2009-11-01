;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defconstant +bias-coefficient+ 0.1
  "Determines how fast penetrations resolve themselves.")
(defconstant +collision-slop+ 0.1
  "Amount of allowed penetration.  Used to reduce vibrating contacts.")

(defstruct (contact (:constructor make-contact (point normal distance hash)))
  ;; Contact point and normal
  point normal
  ;; Penetration distance
  distance
  ;; Calculated by arbiter-prestep
  r1 r2
  n-mass t-mass bounce
  ;; Persistant contact information
  (jn-acc 0) (jt-acc 0) (j-bias 0)
  bias
  ;; Hash value used as a (mostly) unique ID
  hash)

(defun contacts-sum-impulses (&rest contacts)
  (reduce (lambda (sum contact)
            (vec+ sum (vec* (contact-normal contact)
                            (contact-jn-acc contact))))
          contacts :initial-value +zero-vector+))

(defun contacts-sum-impulses-with-friction (&rest contacts)
  (reduce (lambda (sum contact)
            (vec+ sum
                  (vec* (vec-perp (contact-normal contact))
                        (contact-jt-acc contact))
                  (vec* (contact-normal contact)
                        (contact-jn-acc contact))))
          contacts :initial-value +zero-vector+))

(defstruct (arbiter (:constructor make-arbiter (contacts shape-a shape-b stamp)))
  ;; Information on the contact points between the objects
  contacts
  ;; The two shapes involved in the collision
  shape-a shape-b
  ;; Calculated by arbiter-prestep
  u target-v
  ;; Timestamp of the arbiter (from space)
  stamp)

;;; TODO: Verify correctness of docstring
(defun arbiter-inject (arbiter &rest contacts)
  "Replaces ARBITER's contacts with the supplied set, saving state for persistent contacts."
  (mapc (lambda (old-contact)
          (mapc (lambda (new-contact)
                  (when (= (contact-hash new-contact)
                           (contact-hash old-contact))
                    (setf (contact-jn-acc new-contact)
                          (contact-jn-acc old-contact)
                          (contact-jt-acc new-contact)
                          (contact-jt-acc new-contact))))
                contacts))
        (arbiter-contacts arbiter))
  (values))
