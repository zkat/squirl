;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (damped-rotary-spring (:include constraint))
  rest-angle stiffness damping dt target-wrn i-sum)

(defmethod pre-step ((spring damped-rotary-spring) dt dt-inverse)
  (let ((body-a (constraint-body-a spring))
        (body-b (constraint-body-b spring)))
    (setf (damped-rotary-spring-i-sum spring) (/ (+ (body-inverse-inertia body-a)
                                                    (body-inverse-inertia body-b)))
          (damped-rotary-spring-dt spring) dt
          (damped-rotary-spring-target-wrn spring) 0.0)

    ;; Applying spring torque
    (let ((j-spring (* dt (spring-torque spring (- (body-angle body-a) (body-angle body-b))))))
      (decf (body-angular-velocity body-a) (* j-spring (body-inverse-inertia body-a)))
      (incf (body-angular-velocity body-b) (* j-spring (body-inverse-inertia body-b)))))
  (values))
