;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defvar *body-update-velocity-default*)
(defvar *body-update-position-default*)

(defstruct (body (:constructor make-body (mass inertia)))
  ;; Function that is called to integrate the body's velocity.
  (velocity-fun *body-update-velocity-default* :type function)

  ;; Function that is called to integrate the body's position.
  (position-fun *body-update-position-default* :type function)

  ;; Mass Properties
  mass inertia

  ;; Linear components of motion
  (position +zero-vector+ :type vec)
  (velocity +zero-vector+ :type vec)
  (force    +zero-vector+ :type vec)

  ;; Angular components of motion
  angle (angular-velocity 0) (torque 0)

  ;; User Definable Slots
  data

  ;; Velocity bias values used when solving penetrations and correcting constraints.
  (velocity-bias +zero-vector+) (angular-velocity-bias 0))

(defun body-update-velocity (body gravity damping dt)
  (with-accessors ((angular-velocity body-angular-velocity)
                   (inv-inertia body-inverse-inertia)
                   (velocity body-velocity)
                   (torque body-torque)
                   (force body-force)) body
    (setf velocity
          (vec+ (vec* velocity damping)
                (vec* (vec+ gravity (vec* force inv-inertia)) dt)))
    (setf angular-velocity
          (+ (* angular-velocity damping)
             (* torque inv-inertia dt)))))

(defun body-update-position (body dt)
  (with-accessors ((angular-velocity-bias body-angular-velocity-bias)
                   (angular-velocity body-angular-velocity)
                   (velocity-bias body-velocity-bias)
                   (position body-position)
                   (velocity body-velocity)
                   (angle body-angle)) body
    (setf position (vec+ position (vec* (vec+ velocity velocity-bias) dt)))
    (setf angle (+ angle (* angular-velocity angular-velocity-bias dt)))
    (setf velocity-bias +zero-vector+)
    (setf angular-velocity-bias 0)))

(defparameter *body-update-velocity-default* #'body-update-velocity)
(defparameter *body-update-position-default* #'body-update-position)

;;; These should eventually be cached, like in the C version  - Adlai
(defun body-inverse-inertia (body)
  (/ (body-inertia body)))
(defun body-inverse-mass (body)
  (/ (body-mass body)))
(defun body-rotation (body)
  (angle->vec (body-angle body)))

(defun body-slew (body pos dt)
  "Modify the velocity of the body so that it will move to the specified absolute coordinates in
the next timestep.
Intended for objects that are moved manually with a custom velocity integration function."
  (setf (body-velocity body)
        (vec* (vec- pos (body-position body))
              (/ dt))))

(defun body-local->world (body vec)
  "Convert body local to world coordinates."
  (vec+ (body-position body)
        (vec-rotate vec (body-rotation body))))

(defun world->body-local (body vec)
  "Convert world to body local coordinates"
  (vec-unrotate (vec- vec (body-position body))
                (body-rotation body)))

(defun body-apply-impulse (body impulse relative)
  "Apply an impulse (in world coordinates) to the body at a point relative to the center of
gravity (also in world coordinates)."
  (with-accessors ((angular-velocity body-angular-velocity)
                   (inverse-inertia body-inverse-inertia)
                   (inverse-mass body-inverse-mass)
                   (velocity body-velocity)) body
    (setf velocity (vec+ velocity (vec* impulse inverse-mass)))
    (incf angular-velocity (* inverse-inertia (vecx relative impulse)))))

(defun body-apply-bias-impulse (body impulse relative)
  ;; From C: "Not intended for external use. Used by cpArbiter.c and cpConstraint.c."
  (with-accessors ((angular-velocity-bias body-angular-velocity-bias)
                   (inverse-inertia body-inverse-inertia)
                   (inverse-mass body-inverse-mass)
                   (velocity-bias body-velocity-bias)) body
    (setf velocity-bias (vec+ velocity-bias (vec* impulse inverse-mass)))
    (incf angular-velocity-bias (* inverse-inertia (vecx relative impulse)))))

(defun body-reset-forces (body)
  "Zero the forces on a body."
  (setf (body-force body) +zero-vector+
        (body-torque body) 0))

(defun body-apply-force (body force r)
  "Apply a force (in world coordinates) to a body at a point relative to the center
of gravity (also in world coordinates)."
  (setf (body-force body) (vec+ (body-force body) force))
  (incf (body-torque body) (vecx r force)))

(defun apply-damped-spring (body1 body2 anchor1 anchor2 rlen k dmp dt)
  "Apply a damped spring force between two bodies.
Warning: Large damping values can be unstable. Use a DAMPED-SPRING constraint for this instead."
  (let* (;; Calculate the world space anchor coordinates.
         (r1 (vec-rotate anchor1 (body-rotation body1)))
         (r2 (vec-rotate anchor2 (body-rotation body2)))

         (delta (vec- (vec+ (body-position body2) r2)
                      (vec+ (body-position body1) r1)))
         (distance (vec-length delta))
         (n (if (zerop distance) +zero-vector+ (vec* delta (/ 1 distance))))

         (f-spring (* k (- distance rlen)))

         ;; Calculate the world relative velocities of the anchor points.
         (v1 (vec+ (body-velocity body1)
                   (vec* (vec-perp r1) (body-angular-velocity body1))))
         (v2 (vec+ (body-velocity body2)
                   (vec* (vec-perp r2) (body-angular-velocity body2))))

         ;; Calculate the damping force.
         ;; This really sholud be in the impulse solvel and can produce problems when
         ;; using large damping values.
         (vrn (vec. (vec- v2 v1) n))
         (f-damp (* vrn (min dmp (/ 1 (* dt (+ (body-inverse-mass body1)
                                               (body-inverse-mass body2)))))))
         (f (vec* n (+ f-spring f-damp))))
    ;; Apply!
    (body-apply-force body1 f r1)
    (body-apply-force body2 (vec-neg f) r2)))
