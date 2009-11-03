;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defvar *constraint-bias-coefficient* 0.1)

(defgeneric pre-step (constraint dt dt-inverse))
(defgeneric apply-impulse (constraint))
(defgeneric get-impulse (constraint))

(defstruct (constraint (:constructor (make-constraint (body-a body-b))))
  body-a body-b
  (max-force most-positive-double-float)
  (bias-coefficient *constraint-bias-coefficient*)
  (max-bias most-positive-double-float)
  data)

