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
   :vec-lerp
   :vec-zerop
   :angle->vec
   :vec->angle
   :vec.
   :vec-cross
   :vec-perp
   :vec-rperp
   :vec-project
   :vec-rotate
   :vec-unrotate
   :vec-length-sq
   :vec-normalize
   :vec-clamp
   :vec-dist
   :vec-dist-sq
   :vec-near

   ;; Bodies
   :body
   :make-body
   :body-world
   :body-actor
   :body-rotation
   :body-angle
   :body-position
   :body-velocity
   :body-shapes
   :staticp
   :body-update-velocity
   :body-update-position
   :body-slew
   :body-local->world
   :world->body-local
   :body-reset-forces
   :body-apply-force
   :apply-damped-spring

   ;; Shapes
   :reset-shape-id-counter
   :shape-body
   :shape-restitution
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
   :rehash-world-static-data
   :world-point-query-first
   :world-step

   ;; constraints
   :constraint-body-a
   :constraint-body-b
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
   :make-pin-joint
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
   :defcollision
   ))
