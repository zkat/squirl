(in-package :squirl-demo)

(defclass springies-demo (demo)
  ((static-body :initarg :box :accessor static-body))
  (:default-initargs :name "Sproing twang!"))

(defstruct (springy-spring (:include damped-spring)
                           (:constructor
                            (make-springy-spring
                             (body-a body-b anchor1 anchor2 rest-length stiffness damping)))))

(defmethod squirl::spring-force (spring distance &aux (clamp 20d0))
  (* (squirl::clamp (- (squirl::damped-spring-rest-length spring) distance (- clamp) clamp))
     (squirl::damped-spring-stiffness spring)))

(defclass springy ()
  ((bodies :initarg :bodies :initform nil :accessor springy-bodies)))

(defmethod initialize-instance :after ((springy springy) &key)
  (dolist (body (springy-bodies springy))
    (setf (body-actor body) springy)))

(defcollision ((a springy) (b springy) contacts) (unless (eq a b) t))

(defun build-springies (world)
  (list (make-instance 'springy :bodies (list (add-bar world (vec -240 160) (vec -160 80))
                                              (add-bar world (vec -160 80) (vec -80 160))))
        (make-instance 'springy :bodies (list (add-bar world (vec 0 160) (vec 80 0))))
        (make-instance 'springy :bodies (list (add-bar world (vec 160 160) (vec 240 160))))
        (make-instance 'springy :bodies (list (add-bar world (vec -240 0) (vec -160 -80))
                                              (add-bar world (vec -160 -80) (vec -80 0))
                                              (add-bar world (vec -80 0) (vec 0 0))))
        (make-instance 'springy :bodies (list (add-bar world (vec 0 -80) (vec 80 -80))))
        (make-instance 'springy :bodies (list (add-bar world (vec 240 80) (vec 160 0))
                                              (add-bar world (vec 160 0) (vec 240 -80))))
        (make-instance 'springy :bodies (list (add-bar world (vec -240 -80) (vec -160 -160))
                                              (add-bar world (vec -160 -160) (vec -80 -160))))
        (make-instance 'springy :bodies (list (add-bar world (vec 0 -160) (vec 80 -160))))
        (make-instance 'springy :bodies (list (add-bar world (vec 160 -160) (vec 240 -160))))))

(defun add-bar (world point-a point-b)
  (let* ((center (vec* (vec+ point-a point-b) 1/2))
         (length (vec-length (vec- point-a point-b)))
         (mass (/ length 160)))
    (world-add-body world
                    (make-body :mass mass :inertia (* mass length (/ length 12)) :position center
                               :shapes (list (make-segment (vec- point-a center) (vec- point-b center)
                                                           :radius 10))))))

(defun constrain-springies (world springies)
  ;; todo
  )

(defun add-springs (world springies)
  ;; todo
  )

(defmethod init-demo ((demo springies-demo))
  (setf (static-body demo) (make-body)
        (world demo (make-world)))
  (resize-world-active-hash (world demo) 30 999)
  (resize-world-static-hash (world demo) 200 99)
  (let ((springies (build-springies (world demo))))
    (constrain-springies (world demo) springies)
    (add-springs (world demo) springies))
  (world demo))

(pushnew 'springies-demo *demos*)

