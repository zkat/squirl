;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defclass body ()
  ((world :initform nil :accessor body-world)
   (actor :initarg :actor :initform nil :accessor body-actor)
   (shapes :initarg :shapes :initform nil :accessor body-shapes)
   (mass :accessor body-mass :type double-float)
   (inverse-mass :accessor body-inverse-mass :type double-float)
   (inertia :accessor body-inertia :type double-float)
   (inverse-inertia :accessor body-inverse-inertia :type double-float)
   (position :initform +zero-vector+ :initarg :position :accessor body-position :type vec)
   (velocity :initform +zero-vector+ :initarg :velocity :accessor body-velocity :type vec)
   (force :initform +zero-vector+ :initarg :force :accessor body-force :type vec)
   (angle :accessor body-angle :type double-float)
   (rotation :accessor body-rotation :type vec)
   (angular-velocity :accessor body-angular-velocity :type double-float)
   (torque :initarg :torque :initform 0d0 :accessor body-torque :type double-float)
   (velocity-bias :initform +zero-vector+ :accessor body-velocity-bias :type vec)
   (angular-velocity-bias :initform 0d0 :accessor body-angular-velocity-bias :type double-float)))

(defmethod initialize-instance :after ((body body) 
                                       &key (mass most-positive-double-float)
                                       (inertia most-positive-double-float)
                                       (angle 0d0) (angular-velocity 0d0))
  (setf (body-mass body) (float mass 0d0)
        (body-inertia body) (float inertia 1d0)
        (body-angle body) (float angle 0d0)
        (body-angular-velocity body) (float angular-velocity 0d0)
        (body-inverse-mass body)
        #-clisp(/ (body-mass body))
        #+clisp(ext:without-floating-point-underflow
                   (/ (body-mass body)))
        (body-inverse-inertia body)
        #-clisp(/ (body-inertia body))
        #+clisp(ext:without-floating-point-underflow
                   (/ (body-inertia body)))
        (body-rotation body) (angle->vec (body-angle body)))
  (map nil (fun (attach-shape _ body)) (body-shapes body)))

(defun make-body (&rest all-keys)
  (apply #'make-instance 'body all-keys))

(defun staticp (body)
  (when (= most-positive-double-float (body-mass body) (body-inertia body))
    t))

(defun body-attached-p (body world)
  (eq world (body-world body)))

(define-print-object (body)
  (format t "~@[Actor: ~a, ~]Mass: ~a, Inertia: ~a"
          (body-actor body) (body-mass body) (body-inertia body)))

(defmethod (setf body-mass) :after (new-value (body body))
  (setf (body-inverse-mass body) (/ new-value)))
(defmethod (setf body-inertia) :after (new-value (body body))
  (setf (body-inverse-inertia body) (/ new-value)))
(defmethod (setf body-angle) :after (new-value (body body))
  (setf (body-rotation body) (angle->vec new-value)))

(defgeneric body-update-velocity (body gravity damping dt)
  (:method ((body body) gravity damping dt)
    (declare (optimize speed) (double-float dt) (vec gravity))
    (with-accessors ((angular-velocity body-angular-velocity)
                     (inv-inertia body-inverse-inertia)
                     (inv-mass body-inverse-mass)
                     (velocity body-velocity)
                     (torque body-torque)
                     (force body-force)) body
      (declare (double-float angular-velocity inv-mass inv-inertia torque))
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
    (declare (double-float angular-velocity inverse-inertia inverse-mass)
             (vec velocity))
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
    (declare (double-float angular-velocity-bias inverse-mass inverse-inertia)
             (vec velocity-bias))
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
