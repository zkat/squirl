(in-package :squirl-demo)

(defvar *planet*)

(defclass planet-demo (demo)
  ()
  (:default-initargs :name "Planetary Gravity OMFG."))

(defmethod update-demo ((demo planet-demo) ticks)
  (declare (ignore ticks))
  (let ((dt  (float (/ 1 60 3) 1d0)))
    (world-step *world* dt)
    (body-update-position *planet* dt)))

;; Oh my fucking god this sucks
(defstruct (planetary-body (:include body)
                           (:constructor %make-planetary-body
                                         (squirl::%mass squirl::%inertia squirl::position 
                                                        squirl::velocity squirl::force squirl::actor
                                                        squirl::%angle squirl::angular-velocity
                                                        &aux (squirl::inverse-mass
                                                              #+clisp(ext:without-floating-point-underflow
                                                                         (/ squirl::%mass))
                                                              #-clisp(/ squirl::%mass))
                                                        (squirl::inverse-inertia
                                                         #+clisp(ext:without-floating-point-underflow
                                                                    (/ squirl::%inertia))
                                                         #-clisp(/ squirl::%inertia))
                                                        (squirl::rotation (angle->vec squirl::%angle))))))

;; GAAAAHHH
(defun make-planetary-body (&key (mass most-positive-double-float) (inertia most-positive-double-float)
                            (position +zero-vector+) (velocity +zero-vector+) (force +zero-vector+)
                            actor shapes (angle 0d0) (angular-velocity 0d0))
  (let ((body (%make-planetary-body (float mass 0d0) (float inertia 1d0) position velocity
                                    force actor (float angle 0d0) (float angular-velocity 0d0))))
    (map nil (lambda (_) (attach-shape _ body)) shapes)
    body))

(defmethod body-update-velocity ((body planetary-body) gravity damping dt)
  (let* ((position (body-position body))
         (gravity (vec* position (/ -50000 (vec. position position)))))
    (call-next-method body gravity damping dt)))

(defun random-position (radius)
  (loop for vec = (vec (- (random (- 640 (* 2 radius)))
                          (- 320 radius))
                       (- (random (- 480 (* 2 radius)))
                          (- 240 radius)))
     when (< (vec-length vec) 100)
     return vec))

(defun add-box ()
  (let* ((size 10) (mass 1)
         (verts (list (vec (- size) (- size))
                      (vec (- size) size)
                      (vec size size)
                      (vec size (- size))))
         (radius (vec-length (vec size size)))
         (body (make-planetary-body :mass mass :inertia (moment-for-poly mass verts)
                                    :position (random-position radius)
                                    :velocity (vec* (vec (- (random 2) 1) (- (random 2) 1)) 200d0)))
         (shape (make-poly verts :friction 0.7)))
    (attach-shape shape body)
    (world-add-body *world* body)))

(defmethod init-demo ((demo planet-demo))
  (let ((static-body (make-body :angular-velocity 0.2))
        (shape (make-circle 70 :restitution 1 :friction 1)))
    (attach-shape shape static-body)
    (setf *planet* static-body)
    (reset-shape-id-counter)
    (setf *world* (make-world :iterations 20))
    (loop repeat 30 do (add-box))
    (world-add-body *world* static-body)
    *world*))

(pushnew 'planet-demo *demos*)