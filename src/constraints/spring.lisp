;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (spring (:include constraint))
  stiffness dt)

(defmethod get-impulse ((spring damped-rotary-spring))
  0.0)

(defgeneric spring-torque (spring relative-angle))
(defgeneric spring-force (spring distance))

