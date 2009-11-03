;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (damped-rotary-spring (:include constraint))
  rest-angle stiffness damping dt target-wrn i-sum)

