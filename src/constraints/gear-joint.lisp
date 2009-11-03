;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (gear-joint (:include constraint)
             (:constructor
              make-gear-joint
              (body-a body-b anchor1 anchor2
                      &aux (distance
                            (let ((p1 (vec+ (body-position body-a)
                                            (vec-rotate anchor1 (body-rotation body-b))))
                                  (p2 (vec+ (body-position body-b)
                                            (vec-rotate anchor2 (body-rotation body-a)))))
                                       (vec-length (vec- p1 p2)))))))
  (phase 0.0)
  (ratio 0.0)
  (ratio-inverse 0.0)
  (i-sum 0.0)
  (bias 0.0)
  (j-acc 0.0)
  (j-max 0.0))

(defmethod pre-step ((gear gear-joint) dt dt-inv)
  (with-accessors (
		   (body-a gear-joint-body-a)
		   (body-b gear-joint-body-b)
		   (i-sum gear-joint-i-sum)
		   (j-max gear-joint-j-max)
		   (bias gear-joint-bias)
		   (ratio gear-joint-ratio)
		   (max-bias gear-joint-max-bias)
		   (bias-coef gear-joint-bias-coefficient)
		   (ratio-inverse gear-joint-ratio-inverse)
		   (j-acc gear-joint-j-acc)
		   (phase gear-joint-phase)) gear
    ;; calculate moment of inertia coefficient
    (setf i-sum (/ 1.0 (+ (* (body-inverse-inertia body-a) ratio-inverse) (* ratio (body-inverse-inertia body-b)))))
    ;; calculate bias velocity
    (setf bias (clamp (- (* bias-coef dt-inv (- (* (body-angle body-b) ratio) (body-angle body-a) phase))) (- max-bias) max-bias))
    ;; compute max impulse
    (setf j-max (impulse-max gear dt))
    ;; apply joint torque
    (decf (body-angular-velocity body-a) (* j-acc (body-inverse-inertia body-a) ratio-inverse))
    (incf (body-angular-velocity body-b) (* j-acc (body-inverse-inertia body-b)))))

(defmethod apply-impulse ((gear gear-joint))
    (with-accessors (
		   (body-a gear-joint-body-a)
		   (body-b gear-joint-body-b)
		   (ratio-inverse gear-joint-ratio-inverse)
		   (ratio gear-joint-ratio)
		   (j-max gear-joint-j-max)
		   (i-sum gear-joint-i-sum)
		   (j-acc gear-joint-j-acc)
		   (bias gear-joint-bias)) gear
      ;; compute relative rotational velocity
      (let* ((wr (- (* (body-angular-velocity body-b) ratio) (body-angular-velocity body-a)))
	     ;; compute normal impulse
	     (j (* (- bias wr) i-sum))
	     (j-old j-acc))
	(setf j-acc (clamp (+ j-old j) (- j-max) j-max))
	(setf j (- j-acc j-old))
	;;  apply impulse
	(decf (body-angular-velocity body-a) (* j (body-inverse-inertia body-a) ratio-inverse))
	(incf (body-angular-velocity body-b) (* j (body-inverse-inertia body-b))))))
