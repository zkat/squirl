;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

(defpackage squirl
  (:use :cl)
  (:export

   ;; Vector math
   :vec
   :vec-x
   :vec-y
   :vec+
   :vec-
   :vec*
   :+zero-vector+
   :vec-length

   ;; Bodies
   :body
   :make-body
   :body-position
   :body-velocity
   :body-shapes

   ;; Shapes
   :reset-shape-id-counter
   :shape-elasticity
   :shape-friction
   :shape-layers
   :circle
   :make-circle
   :circle-radius
   :circle-transformed-center
   :segment
   :make-segment
   :segment-trans-a
   :segment-trans-b
   :poly
   :make-poly
   :poly-transformed-vertices
   :attach-shape
   :detach-shape

   ;; moments
   :moment-for-circle
   :moment-for-segment
   :moment-for-poly

   ;; world
   :world
   :make-world
   :map-world
   :map-world-hash
   :world-bodies
   :world-constraints
   :world-add-body
   :world-add-shape
   :world-add-static-shape
   :world-add-constraint
   :world-remove-shape
   :world-remove-static-shape
   :world-remove-body
   :world-remove-constraint
   :world-active-shapes
   :world-static-shapes
   :resize-world-static-hash
   :resize-world-active-hash
   :world-step

   ;; constraints
   :breakable-joint
   :make-breakable-joint
   :pivot-joint
   :make-pivot-joint
   :damped-rotary-spring
   :make-damped-rotary-spring
   :damped-spring
   :make-damped-spring
   :gear-joint
   :make-gear-joint
   :groove-joint
   :make-groove-joint
   :pin-joint
   :make-pin-join
   :pivot-joint
   :make-pivot-joint
   :ratchet-joint
   :make-ratchet-joint
   :rotary-limit-joint
   :make-rotary-limit-joint
   :simple-motor
   :make-simple-motor
   :slide-joint
   :make-slide-joint
   :spring
   :make-spring

   ;; callbacks
   :collide
   ))

