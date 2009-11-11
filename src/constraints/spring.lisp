;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (spring (:include constraint))
  stiffness dt)

(defmethod get-impulse ((spring spring))
  0d0)

(defgeneric spring-torque (spring relative-angle))
(defgeneric spring-force (spring distance))
