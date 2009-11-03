;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (damped-spring (:include spring))
  anchor1 anchor2 rest-length damping target-vrn r1 r2 n-mass n)

(defmethod spring-force ((spring damped-spring) distance)
  (* (- (damped-spring-rest-length spring) distance)
     (spring-stiffness spring)))

