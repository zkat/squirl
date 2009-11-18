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

(defun add-bar (point-a point-b)
  ;; todo
  )

(defun gen-bars (world)
  ;; todo
  )

(defun constrain-bars (world bars)
  ;; todo
  )

(defun add-springs (world bars)
  ;; todo
  )

(defmethod init-demo ((demo springies-demo))
  (setf (static-body demo) (make-body)
        (world demo (make-world)))
  (resize-world-active-hash (world demo) 30 999)
  (resize-world-static-hash (world demo) 200 99)
  (let ((bars (gen-bars (world demo))))
    (constrain-bars (world demo) bars)
    (add-springs (world demo) bars))
  (world demo))

(pushnew 'springies-demo *demos*)

