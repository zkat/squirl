(in-package :squirl)

(defun body-update-velocity (body gravity damping dt)
  (setf (body-velocity body)
        (vector-add (vector-multiply (body-velocity body) damping)
                    (vector-multiply (vector-add gravity
                                                 (vector-multiply (body-force body)
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
        (vector-add (body-position body)
                    (vector-multiply (vector-add (body-velocity body)
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

(defstruct (body (:constructor (mass inertia)))
   ;; Function that is called to integrate the body's velocity. (Defaults to cpBodyUpdateVelocity)
   (velocity-fun *body-update-velocity-default*)

   ;; Function that is called to integrate the body's position. (Defaults to cpBodyUpdatePosition)
   (position-fun *body-update-position-default*)

   ;;; Mass Properties
   mass inertia

   ;;; Positional Properties

   ;; Linear components of motion
   (position +zero-vector+)
   (velocity +zero-vector+)
   (force +zero-vector+)

   ;; Angular components of motion
   angle (angular-velocity 0) (torque 0)

   ;;; User Definable Slots
   data

   ;;; Internal slots
   ;; Velocity bias values used when solving penetrations and correcting constraints.
   (velocity-bias +zero-vector+) (angular-velocity-bias 0))

(defun body-inverse-inertia (body)
  (/ 1 (body-inertia body)))
(defun body-inverse-mass (body)
  (/ 1 (body-mass body)))
(defun body-rotation (body)
  (angle->vector (body-angle body)))

(defun body-slew (body pos dt)
  "Modify the velocity of the body so that it will move to the specified absolute coordinates in
the next timestep.
Intended for objects that are moved manually with a custom velocity integration function."
  (let ((delta (vector-subtract pos (body-position body))))
    (setf (body-velocity body)
          (vector-multiply delta (/ 1 dt)))))


(defun body-local->world (body vector)
  "Convert body local to world coordinates."
  ;; C version:
  ;; return cpvadd(body->p, cpvrotate(v, body->rot));
  (vector-add (body-position body)
              (vector-rotate vector (body-rotation body))))

(defun world->body-local (body vector)
  "Convert world to body local coordinates"
  ;; return cpvunrotate(cpvsub(v, body->p), body->rot);
  (vector-unrotate (vector-subtract vector (body-position body))
                   (body-rotation body)))

(defun body-apply-impulse (body j r)
  "Apply an impulse (in world coordinates) to the body at a point relative to the center of
gravity (also in world coordinates)."
  (setf (body-velocity body)
        (vector-add (body-velocity)
                    (vector-multiply j (body-inverse-mass body))))
  (incf (body-angular-velocity body)
        (* (body-inverse-inertia body)
           (vector-cross r j))))

(defun body-reset-forces (body)
  "Zero the forces on a body."
  (setf (body-force body) +zero-vector+
        (body-torque body) 0))

(defun body-apply-force (body force r)
  "Apply a force (in world coordinates) to a body at a point relative to the center
of gravity (also in world coordinates)."
  (setf (body-force body) (vector-add (body-force body) force))
  (incf (body-torque body) (vector-cross r force)))

(defun apply-damped-spring (body1 body2 anchor1 anchor2 rlen k dmp dt)
  "Apply a damped spring force between two bodies.
Warning: Large damping values can be unstable. Use a DAMPED-SPRING constraint for this instead."
  (let* (;; Calculate the world space anchor coordinates.
         (r1 (vector-rotate anchor1 (body-rotation body1)))
         (r2 (vector-rotate anchor2 (body-rotation body2)))

         (delta (vector-subtract (vector-add (body-position body2) r2)
                                 (vector-add (body-position body1) r1)))
         (distance (vector-length delta))
         (n (if (zerop distance) +zero-vector+ (vector-multiply delta (/ 1 distance))))

         (f-spring (* k (- distance rlen)))

         ;; Calculate the world relative velocities of the anchor points.
         (v1 (vector-add (body-velocity body1)
                         (vector-multiply (vector-perp r1) (body-angular-velocity body1))))
         (v2 (vector-add (body-velocity body2)
                         (vector-multiply (vector-perp r2) (body-angular-velocity body2))))

         ;; Calculate the damping force.
         ;; This really sholud be in the impulse solvel and can produce problems when
         ;; using large damping values.
         (vrn (vector-dot (vector-subtract v2 v1) n))
         (f-damp (* vrn (min dmp (/ 1 (* dt (+ (body-inverse-mass body1)
                                               (body-inverse-mass body2)))))))
         (f (vector-multiply n (+ f-spring f-damp))))
    ;; Apply!
    (body-apply-force body1 f r1)
    (body-apply-force body2 (vector-neg f) r2)))

