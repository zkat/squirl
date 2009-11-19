(in-package :squirl-demo)

(defclass theo-jansen (demo)
  ((static-body :accessor static-body)
   (walker :accessor demo-walker)
   (motor :accessor demo-motor))
  (:default-initargs :name "Theo Jansen Walker"))

(defparameter *seg-radius* 3)

(defmethod update-demo :before ((demo theo-jansen) dt)
  (declare (ignore dt))
  (let* ((coef (/ (+ 2 (vec-y *arrow-direction*))))
         (rate (* (vec-x *arrow-direction*) 10 coef)))
    (setf (squirl::simple-motor-rate (demo-motor demo)) rate
          (squirl::simple-motor-max-force (demo-motor demo)) (if (zerop rate) 0d0 100000))))

(defclass walker ()
  ((chassis :initarg :chassis :accessor walker-chassis)
   (crank :initarg :crank :accessor walker-crank)
   (upper-legs :initform nil :initarg :upper-legs :accessor walker-upper-legs)
   (lower-legs :initform nil :initarg :lower-legs :accessor walker-lower-legs)))

(defcollision ((a walker) (b walker) contacts) (not (eq a b)))
(defcollision ((a walker) (b (eql :not-grabbable)) contacts) (declare (ignore a b contacts)) t)

(defun make-leg (world side offset walker anchor &aux (leg-mass 1))
  (let* ((upper-leg (world-add-body 
                     world
                     (make-body :mass leg-mass :position (vec offset 0) :actor walker
                                :shapes (list
                                         (make-segment +zero-vector+ (vec 0 side)
                                                       :radius *seg-radius*)))))
         (lower-leg (world-add-body
                     world
                     (make-body :mass leg-mass :position (vec offset 0) :actor walker
                                :shapes (list
                                         (make-segment +zero-vector+ (vec 0 (- side))
                                                       :radius *seg-radius*)
                                         (make-circle (* 2 *seg-radius*)
                                                      :center (vec 0 (- side))
                                                      :friction 1)))))
         (diag (sqrt (+ (* side side) (* offset offset))))
         (chassis (walker-chassis walker))
         (crank (walker-crank walker)))
    (pushnew upper-leg (walker-upper-legs walker))
    (pushnew lower-leg (walker-lower-legs walker))
    ;; add constraints
    (world-add-constraint world (make-pivot-joint chassis upper-leg (vec offset 0) +zero-vector+))
    (world-add-constraint world (make-pin-joint chassis lower-leg (vec offset 0) +zero-vector+))
    (world-add-constraint world (make-gear-joint upper-leg lower-leg 0 1))
    (let ((joint (make-pin-joint crank upper-leg anchor (vec 0 side))))
      (setf (squirl::pin-joint-distance joint) diag)
      (world-add-constraint world joint))
    (let ((joint (make-pin-joint crank lower-leg anchor +zero-vector+)))
      (setf (squirl::pin-joint-distance joint) diag)
      (world-add-constraint world joint))))

(defmethod init-demo ((demo theo-jansen))
  (setf (demo-walker demo) (make-instance 'walker)
        (world demo) (make-world :iterations 20 :gravity (vec 0 -500))
        (static-body demo) (world-add-body
                            (world demo)
                            (make-body :actor :not-grabbable
                                       :shapes
                                       (list (make-segment (vec -320 -240) (vec -320 240)
                                                           :restitution 1 :friction 1)
                                             (make-segment (vec 320 -240) (vec 320 240)
                                                           :restitution 1 :friction 1)
                                             (make-segment (vec -320 -240) (vec 320 -240)
                                                           :restitution 1 :friction 1)))))
  (let ((offset 30) (chassis-mass 2) (crank-mass 1) (crank-radius 13d0) (side 30) (num-legs 2))
    (setf (walker-chassis (demo-walker demo))
          (world-add-body (world demo) 
                          (make-body :mass chassis-mass :actor (demo-walker demo)
                                     :shapes (list (make-segment (vec (- offset) 0)
                                                                 (vec offset 0)
                                                                 :radius *seg-radius*))))
          (walker-crank (demo-walker demo))
          (world-add-body (world demo) 
                          (make-body :mass crank-mass :actor (demo-walker demo)
                                     :shapes (list (make-circle crank-radius)))))
    (world-add-constraint (world demo)
                          (make-pivot-joint (walker-chassis (demo-walker demo))
                                            (walker-crank (demo-walker demo))
                                            +zero-vector+ +zero-vector+))
    ;; add the legs...
    (loop for i upto num-legs do
         (make-leg (world demo) side offset (demo-walker demo)
                   (vec* (angle->vec (/ (* 2 i) (* num-legs pi))) crank-radius))
         (make-leg (world demo) side offset (demo-walker demo)
                   (vec* (angle->vec (/ (* 2 (1+ i)) (* num-legs pi))) crank-radius)))
    (setf (demo-motor demo)
          (world-add-constraint (world demo)
                                (make-simple-motor (walker-chassis (demo-walker demo))
                                                   (walker-crank (demo-walker demo)) 6)))
    (world demo)))

(pushnew 'theo-jansen *demos*)