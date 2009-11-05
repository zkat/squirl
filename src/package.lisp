;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :cl-user)

(defpackage squirl
  (:use :cl)
  (:export

   ;; Vector math
   :vec
   :vec-x
   :vec-y
   :+zero-vector+

   ;; Bodies
   :body
   :make-body
   :body-position

   ;; Shapes
   :circle
   :make-circle
   :segment
   :make-segment
   :poly
   :make-poly

   ;; moments
   :moment-for-circle
   :moment-for-segment
   :moment-for-poly

   ;; world
   :world
   :make-world
   :map-world
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
   :resize-world-static-hash
   :resize-world-active-hash
   :world-step

   ;; callbacks
   :collide
   ))

