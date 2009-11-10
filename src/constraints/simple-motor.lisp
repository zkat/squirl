;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (simple-motor (:include constraint)
             (:constructor
              make-simple-motor
              (body-a body-b rate)))
  (rate 0d0)
  (i-sum 0d0)
  (j-acc 0d0)
  (j-max 0d0))

(defmethod pre-step ((motor simple-motor) dt dt-inv)
  (declare (ignore dt-inv))
  (with-accessors ((body-a simple-motor-body-a)
		   (body-b simple-motor-body-b)
		   (i-sum simple-motor-i-sum)
		   (j-max simple-motor-j-max)
		   (j-acc simple-motor-j-acc)) motor
    ;; calculate moment of inertia coefficient.
    (setf i-sum (/ 1d0 (+ (body-inverse-inertia body-b) (body-inverse-inertia body-b))))
    ;;  compute max impulse
    (setf j-max (impulse-max motor dt))
    ;; apply joint torque
    (decf (body-angular-velocity body-a) (* j-acc (body-inverse-inertia body-a)))
    (incf (body-angular-velocity body-b) (* j-acc (body-inverse-inertia body-b)))))

(defmethod apply-impulse ((motor simple-motor))
    (with-accessors (
		   (body-a simple-motor-body-a)
		   (body-b simple-motor-body-b)
		   (i-sum simple-motor-i-sum)
		   (j-max simple-motor-j-max)
		   (j-acc simple-motor-j-acc)
		   (rate simple-motor-rate)) motor
      ;; compute relative rotational velocity
      (let* ((wr (+ (- (body-angular-velocity body-b) (body-angular-velocity body-a)) rate))
	     ;; compute normal impulse
	     (j (* (- wr) i-sum))
	     (j-old j-acc))
	(setf j-acc (clamp (+ j-old j) (- j-max) j-max))
	(setf j (- j-acc j-old))
	;;  apply impulse
	(decf (body-angular-velocity body-a) (* j (body-inverse-inertia body-a)))
	(incf (body-angular-velocity body-b) (* j (body-inverse-inertia body-b))))))

(defmethod get-impulse ((motor simple-motor))
  (abs (simple-motor-j-acc motor)))