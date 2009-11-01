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
            (body-inverse-inertia)
            dt))))

(defun body-update-position (body dt)
  ;; todo
  )

(defparameter *body-update-velocity-default* #'body-update-velocity)
(defparameter *body-update-position-default* #'body-update-position)

(defclass body ()
  ( ;; Function that is called to integrate the body's velocity. (Defaults to cpBodyUpdateVelocity)
   (velocity-fun :initform *body-update-velocity-default*)

   ;; Function that is called to integrate the body's position. (Defaults to cpBodyUpdatePosition)
   (position-fun :initform *body-update-position-default*)

   ;;; Mass Properties
  
   ;; Mass and its inverse
   ;; Always use BODY-SET-MASS when changing the mass as these values must agree.
   mass inverse-mass

   ;; Moment of inertia and its inverse
   ;; Always use BODY-SET-MASS when changing the mass as these values must agree.
   inertia inverse-inertia

   ;;; Positional Properties
  
   ;; Linear components of motion
   (position :initform +zero-vector+)
   (velocity :initform +zero-vector+)
   (force :initform +zero-vector+)

   ;; Angular components of motion
   ;; Always use BODY-SET-ANGLE to set the angle of the body, as angle and rotation must agree.
   angle (angular-velocity :initform 0) (torque :initform 0)

   ;; Cached unit length vector representing the angle of the body.
   ;; Used for fast vector rotation using VECTOR-ROTATE
   rotation

   ;;; User Definable Slots
  
   ;; User defined data.
   data

   ;;; Internal slots
  
   ;; Velocity bias values used when solving penetrations and correcting constraints.
   (velocity-bias :initform +zero-vector+) (angular-velocity-bias :initform 0)))

(defun make-body (m i)
  (let ((body (make-instance 'body)))
    (setf (body-mass body) m
          (body-moment body) i
          (body-angle body) 0)
    body))

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
  ;; todo
  )

(defun body-apply-force (body f r)
  "Apply a force (in world coordinates) to a body at a point relative to the center
of gravity (also in world coordinates)."
  ;; todo
  )

(defun apply-damped-spring (body1 body2 anchor1 anchor2 rlen k dmp dt)
  "Apply a damped spring force between two bodies.
Warning: Large damping values can be unstable. Use a DAMPED-SPRING constraint for this instead."
  ;; todo
  )
