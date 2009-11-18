;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defmacro defbody (name)
  `(progn
     (defstruct (,name
                  (:include body)
                  (:constructor ,(intern (concatenate 'string "%MAKE-" (symbol-name name)))
                                (%mass %inertia position
                                       velocity force actor
                                       %angle angular-velocity
                                       &aux
                                       (inverse-mass
                                        (#-clisp progn
                                                 #+clisp ext:without-floating-point-underflow
                                                 (if (zerop %mass) 0d0 (/ %mass))))
                                       (inverse-inertia
                                        (#-clisp progn
                                                 #+clisp ext:without-floating-point-underflow
                                                   (/ %inertia)))
                                       (rotation (angle->vec %angle))))))
     (defun ,(intern (concatenate 'string "MAKE-" (symbol-name name)))
         (&key (mass 0d0) (inertia most-positive-double-float)
          (position +zero-vector+) (velocity +zero-vector+) (force +zero-vector+)
          actor shapes (angle 0d0) (angular-velocity 0d0))
       (let ((body (,(intern (concatenate 'string "%MAKE-" (symbol-name name)))
                     (float mass 0d0) (float inertia 1d0) position velocity
                     force actor (float angle 0d0) (float angular-velocity 0d0))))
         (map nil (lambda (_) (attach-shape _ body)) shapes)
         body))))

(defstruct (body
             (:constructor
              %make-body (%mass %inertia position velocity force actor %angle angular-velocity
                                &aux
                                (inverse-mass
                                 (#-clisp progn
                                          #+clisp ext:without-floating-point-underflow
                                          (if (zerop %mass) 0d0 (/ %mass))))
                                (inverse-inertia
                                 (#-clisp progn
                                          #+clisp ext:without-floating-point-underflow
                                          (/ %inertia)))
                                (rotation (angle->vec %angle)))))
  world                 ; world that this body is attached to, if any.
  actor                 ; Actor used for the COLLIDE "callback"
  %shapes               ; shapes associated with this body.
  ;; Mass properties, and cached inverses
  (%mass (assert nil) :type double-float)
  (inverse-mass (assert nil) :type double-float)
  (%inertia (assert nil) :type double-float)
  (inverse-inertia (assert nil) :type double-float)
  ;; Linear components of motion
  (position +zero-vector+ :type vec)
  (velocity +zero-vector+ :type vec)
  (force    +zero-vector+ :type vec)
  ;; Angular components of motion, and cached rotation vector
  (%angle 0d0 :type double-float)
  (rotation +initial-rotation+ :type vec)
  (angular-velocity 0d0 :type double-float)
  (torque 0d0 :type double-float)
  ;; Velocity bias values used when solving penetrations and correcting constraints.
  (velocity-bias +zero-vector+ :type vec)
  (angular-velocity-bias 0d0 :type double-float))

(defun make-body (&key (mass 0d0) (inertia most-positive-double-float)
                  (position +zero-vector+) (velocity +zero-vector+) (force +zero-vector+) actor
                  shapes (angle 0d0) (angular-velocity 0d0))
  (let ((body (%make-body (float mass 0d0) (float inertia 1d0) position velocity
                          force actor (float angle 0d0) (float angular-velocity 0d0))))
    (map nil (fun (attach-shape _ body)) shapes)
    body))

(defun staticp (body)
  (when (= 0 (body-mass body)) t))

(defun body-attached-p (body world)
  (eq world (body-world body)))

(defun body-shapes (body)
  (body-%shapes body))

(define-print-object (body)
  (format t "~@[Actor: ~a, ~]Mass: ~a, Inertia: ~a"
          (body-actor body) (body-mass body) (body-inertia body)))

;;; Wraps the mass, inertia, and angle slots so that setting them updates
;;; the inverse-mass, inverse-inertia, and rotation slots.
(macrolet ((wrap (external internal cached wrapper)
             `(progn (defun ,external (body) (,internal body))
                     (defun (setf ,external) (new-value body)
                       (setf (,internal body) new-value
                             (,cached body) (,wrapper new-value))))))
  (wrap body-mass body-%mass body-inverse-mass /)
  (wrap body-inertia body-%inertia body-inverse-inertia /)
  (wrap body-angle body-%angle body-rotation angle->vec))

(defgeneric body-update-velocity (body gravity damping dt)
  (:method ((body body) gravity damping dt)
    (declare (optimize speed) (double-float dt) (vec gravity))
    (with-accessors ((angular-velocity body-angular-velocity)
                     (inv-inertia body-inverse-inertia)
                     (inv-mass body-inverse-mass)
                     (velocity body-velocity)
                     (torque body-torque)
                     (force body-force)) body
      (setf velocity
            (vec+ (vec* velocity damping)
                  (vec* (vec+ gravity (vec* force inv-mass)) dt)))
      (setf angular-velocity
            (+ (* angular-velocity damping)
               (* torque inv-inertia dt)))
      (values))))

(defgeneric body-update-position (body dt)
  (:method ((body body) dt)
    (with-accessors ((angular-velocity-bias body-angular-velocity-bias)
                     (angular-velocity body-angular-velocity)
                     (velocity-bias body-velocity-bias)
                     (position body-position)
                     (velocity body-velocity)
                     (angle body-angle)) body
      (setf position (vec+ position (vec* (vec+ velocity velocity-bias) dt)))
      (incf angle (* (+ angular-velocity angular-velocity-bias) dt))
      (setf velocity-bias +zero-vector+)
      (setf angular-velocity-bias 0d0))))

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
  (declare (optimize speed))
  (with-accessors ((angular-velocity body-angular-velocity)
                   (inverse-inertia body-inverse-inertia)
                   (inverse-mass body-inverse-mass)
                   (velocity body-velocity)) body
    (setf velocity (vec+ velocity (vec* impulse inverse-mass)))
    (incf angular-velocity (* inverse-inertia (vec-cross relative impulse)))
    (values)))

(defun body-apply-bias-impulse (body impulse relative)
  ;; From C: "Not intended for external use. Used by cpArbiter.c and cpConstraint.c."
  (declare (optimize speed))
  (with-accessors ((angular-velocity-bias body-angular-velocity-bias)
                   (inverse-inertia body-inverse-inertia)
                   (inverse-mass body-inverse-mass)
                   (velocity-bias body-velocity-bias)) body
    (setf velocity-bias (vec+ velocity-bias (vec* impulse inverse-mass)))
    (incf angular-velocity-bias (* inverse-inertia (vec-cross relative impulse))))
  (values))

(defun body-reset-forces (body)
  "Zero the forces on a body."
  (setf (body-force body) +zero-vector+
        (body-torque body) 0d0))

(defun body-apply-force (body force r)
  "Apply a force (in world coordinates) to a body at a point relative to the center
of gravity (also in world coordinates)."
  (setf (body-force body) (vec+ (body-force body) force))
  (incf (body-torque body) (vec-cross r force)))

(defun apply-damped-spring (body1 body2 anchor1 anchor2 rlen k dmp dt)
  "Apply a damped spring force between two bodies.
Warning: Large damping values can be unstable. Use a DAMPED-SPRING constraint for this instead."
  (setf anchor1 (vec-rotate anchor1 (body-rotation body1))
        anchor2 (vec-rotate anchor2 (body-rotation body2)))
  (let* ((delta (vec- (vec+ (body-position body2) anchor2)
                      (vec+ (body-position body1) anchor1)))
         (normal (vec-normalize-safe delta))
         (f-spring (* k (- (vec-length delta) rlen)))
         ;; Calculate the world relative velocities of the anchor points.
         (v1 (vec+ (body-velocity body1)
                   (vec* (vec-perp anchor1) (body-angular-velocity body1))))
         (v2 (vec+ (body-velocity body2)
                   (vec* (vec-perp anchor2) (body-angular-velocity body2))))
         ;; Calculate the damping force.
         ;; This really should be in the impulse solve and can produce problems when
         ;; using large damping values.
         (f-damp (* (vec. (vec- v2 v1) normal)
                    (min dmp (/ (* dt (+ (body-inverse-mass body1)
                                         (body-inverse-mass body2)))))))
         (f (vec* normal (+ f-spring f-damp))))
    ;; Apply!
    (body-apply-force body1 f anchor1)
    (body-apply-force body2 (vec- f) anchor2)))
