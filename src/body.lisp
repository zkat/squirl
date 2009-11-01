;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defvar *body-update-velocity-default*)
(defvar *body-update-position-default*)

(defstruct (body (:constructor make-body (mass inertia)))
  ;; Function that is called to integrate the body's velocity. (Defaults to cpBodyUpdateVelocity)
  (velocity-fun *body-update-velocity-default*)

  ;; Function that is called to integrate the body's position. (Defaults to cpBodyUpdatePosition)
  (position-fun *body-update-position-default*)

  ;; Mass Properties
  mass inertia

  ;; Positional Properties

  ;; Linear components of motion
  (position +zero-vector+)
  (velocity +zero-vector+)
  (force +zero-vector+)

  ;; Angular components of motion
  angle (angular-velocity 0) (torque 0)

  ;; User Definable Slots
  data

  ;; Internal slots
  ;; Velocity bias values used when solving penetrations and correcting constraints.
  (velocity-bias +zero-vector+) (angular-velocity-bias 0))

(defun body-update-velocity (body gravity damping dt)
  (setf (body-velocity body)
        (vec+ (vec* (body-velocity body) damping)
              (vec* (vec+ gravity
                          (vec* (body-force body)
                                (body-inverse-inertia body)))
                    dt)))
  (setf (body-angular-velocity body)
        (+
         (* (body-angular-velocity body)
            damping)
         (* (body-torque body)
            (body-inverse-inertia body)
            dt))))

(defun body-update-position (body dt)
  (setf (body-position body)
        (vec+ (body-position body)
              (vec* (vec+ (body-velocity body)
                          (body-velocity-bias body))
                    dt)))
  (setf (body-angle body)
        (+ (body-angle body)
           (* (body-angular-velocity body)
              (body-angular-velocity-bias body)
              dt)))
  (setf (body-velocity-bias body) +zero-vector+)
  (setf (body-angular-velocity-bias body) 0))

(defparameter *body-update-velocity-default* #'body-update-velocity)
(defparameter *body-update-position-default* #'body-update-position)

(defun body-inverse-inertia (body)
  (/ 1 (body-inertia body)))
(defun body-inverse-mass (body)
  (/ 1 (body-mass body)))
(defun body-rotation (body)
  (angle->vec (body-angle body)))

(defun body-slew (body pos dt)
  "Modify the velocity of the body so that it will move to the specified absolute coordinates in
the next timestep.
Intended for objects that are moved manually with a custom velocity integration function."
  (let ((delta (vec- pos (body-position body))))
    (setf (body-velocity body)
          (vec* delta (/ 1 dt)))))

(defun body-local->world (body vec)
  "Convert body local to world coordinates."
  ;; C version:
  ;; return cpvadd(body->p, cpvrotate(v, body->rot));
  (vec+ (body-position body)
        (vec-rotate vec (body-rotation body))))

(defun world->body-local (body vec)
  "Convert world to body local coordinates"
  ;; return cpvunrotate(cpvsub(v, body->p), body->rot);
  (vec-unrotate (vec- vec (body-position body))
                   (body-rotation body)))

(defun body-apply-impulse (body j r)
  "Apply an impulse (in world coordinates) to the body at a point relative to the center of
gravity (also in world coordinates)."
  (setf (body-velocity body)
        (vec+ (body-velocity body)
              (vec* j (body-inverse-mass body))))
  (incf (body-angular-velocity body)
        (* (body-inverse-inertia body)
           (vecx r j))))

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
