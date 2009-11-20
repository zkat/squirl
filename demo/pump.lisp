(in-package :squirl-demo)

(defclass pump-demo (demo)
  ((static-body :accessor demo-static-body)
   (balls :initform nil :accessor demo-balls))
  (:default-initargs :name "Pump it up!"))

(defvar *static-body*)
(defvar *motor*)
(defparameter *num-balls* 4)

(defmethod update-demo ((demo pump-demo) dt)
  (declare (ignore dt))
  (let* ((coef (/ (+ 2 (vec-y *arrow-direction*)) 3))
         (rate (* (vec-x *arrow-direction*) 30 coef)))
    (setf (squirl::simple-motor-rate *motor*) rate
          (squirl::simple-motor-max-force *motor*) (if (zerop rate) 0 1000000))
    (sleep 0.016)
    (world-step (world demo) (physics-timestep demo))
    (loop for ball in (demo-balls demo)
       do (when (> (vec-x (body-position ball)) 320)
            (setf (body-velocity ball) +zero-vector+)
            (setf (body-position ball) (vec -224 200))))))

(defclass ball () ())
(defclass feeder () ())
(defclass plunger () ())
(defclass gear () ())

;; balls collide with everything
(defcollision ((ball ball) anything contacts) t)
;; the plunger collides with the hopper, but not with the gears
(defcollision ((a plunger) (hopper (eql :not-grabbable)) contacts) t)
(defcollision ((a plunger) (b gear) contacts) nil)
;; the feeder collides with the balls, but not with anything else
(defcollision ((a feeder) anything contacts) nil)
(defcollision ((feeder feeder) (ball ball) contacts) t)
;; gears collide with each other, though
(defcollision ((a gear) (another gear) contacts) t)

(defun add-ball (world pos)
  (world-add-body world (make-body :mass 1 :position pos :actor (make-instance 'ball)
                                   :shapes (list (make-circle 30 :friction 0.5)))))

(defun setup-static-body (world)
  (world-add-body world
                  (make-body :actor :not-grabbable
                             :shapes (list (make-segment (vec -256 16) (vec -256 240) :radius 2
                                                         :restitution 1 :friction 0.5)
                                           (make-segment (vec -256 16) (vec -192 0) :radius 2
                                                         :restitution 1 :friction 0.5)
                                           (make-segment (vec -192 0) (vec -192 -64) :radius 2
                                                         :restitution 1 :friction 0.5)
                                           (make-segment (vec -128 -64) (vec -128 144) :radius 2
                                                         :restitution 1 :friction 0.5)
                                           (make-segment (vec -192 80) (vec -192 176) :radius 2
                                                         :restitution 1 :friction 0.5)
                                           (make-segment (vec -192 176) (vec -128 240) :radius 2
                                                         :restitution 1 :friction 0.5)
                                           (make-segment (vec -128 144) (vec 192 64) :radius 2
                                                         :restitution 1 :friction 0.5)))))
(defun add-plunger (world)
  (let ((verts (list (vec -30 -80)
                     (vec -30 80)
                     (vec 30 64)
                     (vec 30 -80))))
    (world-add-body
     world (make-body :mass 1 :position (vec -160 -80) :actor (make-instance 'plunger)
                      :shapes (list (make-poly verts :restitution 1 :friction 0.5))))))

(defun add-small-gear (world static-body)
  (let ((gear (world-add-body
               world (make-body :mass 10 :angle (/ pi -2) :actor (make-instance 'gear)
                                :position (vec -160 -160) :shapes (list (make-circle 80))))))
    (world-add-constraint world (make-pivot-joint static-body gear (vec -160 -160) +zero-vector+))
    gear))

(defun add-big-gear (world static-body)
  (let ((gear (world-add-body
               world (make-body :mass 40 :actor (make-instance 'gear)
                                :position (vec 80 -160) :angle (/ pi 2)
                                :shapes (list (make-circle 160))))))
    (world-add-constraint world (make-pivot-joint static-body gear (vec 80 -160) +zero-vector+))
    gear))

(defun add-constraints (world small-gear plunger big-gear)
  (world-add-constraint world (make-pin-joint small-gear plunger (vec 80 0) +zero-vector+))
  (world-add-constraint world (make-gear-joint small-gear big-gear (/ pi 2) -2)))

(defun add-feeder (world static-body small-gear)
  (let* ((bottom -300) (top 32) (length (- top bottom))
         (feeder (make-body :mass 1 :actor (make-instance 'feeder)
                            :position (vec -224 (/ (+ bottom top) 2))
                            :shapes (list (make-segment (vec 0 (/ length 2)) (vec 0 (- (/ length 2)))
                                                        :radius 20)))))
    (world-add-body world feeder)
    (world-add-constraint world (make-pivot-joint static-body feeder
                                                  (vec -224 bottom) (vec 0 (- (/ length 2)))))
    (world-add-constraint world (make-pin-joint feeder small-gear
                                                (world->body-local feeder (vec -224 -160))
                                                (vec 0 80)))))

(defun motorize-gear (world static-body big-gear)
  (setf *motor* (world-add-constraint world (make-simple-motor static-body big-gear 3))))

(defmethod init-demo ((demo pump-demo))
  (setf (world demo) (make-world :gravity (vec 0 -600)))
  (setf (demo-static-body demo) (setup-static-body (world demo)))
  (dotimes (i *num-balls*)
    (pushnew (add-ball (world demo) (vec -224 (+ 80 (* i 64))))
             (demo-balls demo)))
  (let* ((plunger (add-plunger (world demo)))
         (small-gear (add-small-gear (world demo) (demo-static-body demo)))
         (big-gear (add-big-gear (world demo) (demo-static-body demo))))
    (add-constraints (world demo) small-gear plunger big-gear)
    (add-feeder (world demo) (demo-static-body demo) small-gear)
    (motorize-gear (world demo) (demo-static-body demo) big-gear))
  (world demo))

(pushnew 'pump-demo *demos*)
