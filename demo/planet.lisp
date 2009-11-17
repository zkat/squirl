(in-package :squirl-demo)

(defclass planet-demo (demo)
  ((planet :initarg :planet :accessor planet))
  (:default-initargs :name "Planetary Gravity OMFG."))

(defmethod update-demo ((demo planet-demo) dt)
  (incf (accumulator demo) (if (> dt *dt-threshold*) *dt-threshold* dt))
  (loop while (>= (accumulator demo) (physics-timestep demo))
     do (world-step (world demo) (physics-timestep demo))
       (body-update-position (planet demo) (physics-timestep demo))
     (decf (accumulator demo) (physics-timestep demo))))

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
  (declare (ignore gravity))
  (let* ((position (body-position body))
         (gravity (vec* position (/ -50000 (vec. position position)))))
    (call-next-method body gravity damping dt)))

(defun random-position (radius)
  (loop for vec = (vec (- (random (- 640 (* 2 radius)))
                          (- 320 radius))
                       (- (random (- 480 (* 2 radius)))
                          (- 240 radius)))
     when (< 88 (vec-length vec) 200)
     return vec))

(let ((size 10) (mass 1))
  (defun add-box ()
    (let* ((verts (list (vec (- size) (- size))
                        (vec (- size) size)
                        (vec size size)
                        (vec size (- size))))
           (body (make-planetary-body :mass mass :inertia (moment-for-poly mass verts)
                                      :position (random-position (vec-length (vec size size)))
                                      :velocity (vec* (angle->vec (* pi (random 2d0)))
                                                      (random 200d0)))))
      (attach-shape (make-poly verts :friction 0.7 :restitution 1) body)
      (world-add-body (world *current-demo*) body))))

(defmethod grabbablep ((actor (eql :planet-body)))
  nil)

(defmethod init-demo ((demo planet-demo))
  (let ((planet-body (make-body :angular-velocity 0.3 :actor :planet-body))
        (shape (make-circle 70 :restitution 1 :friction 0.8)))
    (attach-shape shape planet-body)
    (setf (planet demo) planet-body)
    (reset-shape-id-counter)
    (setf (world demo) (make-world :iterations 20))
    (loop repeat 22 do (add-box))
    (world-add-body (world demo) planet-body)
    (world demo)))

(pushnew 'planet-demo *demos*)
