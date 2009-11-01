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