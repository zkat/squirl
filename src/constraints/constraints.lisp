;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defvar *constraint-bias-coefficient* 0.1)

(defgeneric pre-step (constraint dt dt-inverse))
(defgeneric apply-impulse (constraint))
(defgeneric get-impulse (constraint))
