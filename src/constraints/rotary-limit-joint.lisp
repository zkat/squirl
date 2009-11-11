;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (rotary-limit-joint (:include constraint)
             (:constructor
              make-rotary-limit-joint
              (body-a body-b min max)))
  min
  max
  (i-sum 0d0)
  (bias 0d0)
  (j-acc 0d0)
  (j-max 0d0))

(defmethod pre-step ((rotary-limit rotary-limit-joint) dt dt-inv)
  (with-accessors (
                   (body-a rotary-limit-joint-body-a)
                   (body-b rotary-limit-joint-body-b)
                   (i-sum rotary-limit-joint-i-sum)
                   (j-max rotary-limit-joint-j-max)
                   (bias rotary-limit-joint-bias)
                   (bias-coef rotary-limit-joint-bias-coefficient)
                   (min rotary-limit-joint-min)
                   (j-acc rotary-limit-joint-j-acc)
                   (max rotary-limit-joint-max)
                   (max-bias rotary-limit-joint-max-bias)) rotary-limit
    (let* ((dist (- (body-angle body-b) (body-angle body-a)))
           (pdist (if (> dist max) (- max dist) (- min dist))))
      ;; calculate moment of inertia coefficient
      (setf i-sum (/ 1d0 (+ (body-inverse-inertia body-a) (body-inverse-inertia body-b))))
      ;; calculate bias velocity
      (setf bias (clamp (- (* bias-coef dt-inv pdist)) (- max-bias) max-bias))
      ;; compute max impulse
      (setf j-max (impulse-max rotary-limit dt))
      ;; if the bias is zero, the joint is not at a limit, reset impulse
      (when (zerop bias)
        (setf j-acc 0d0))
      ;; apply joint torque
      (decf (body-angular-velocity body-a) (* j-acc (body-inverse-inertia body-a)))
      (incf (body-angular-velocity body-b) (* j-acc (body-inverse-inertia body-b))))))

(defmethod apply-impulse ((rotary-limit rotary-limit-joint))
    (with-accessors (
                   (body-a rotary-limit-joint-body-a)
                   (body-b rotary-limit-joint-body-b)
                   (direction rotary-limit-joint-direction)
                   (j-max rotary-limit-joint-j-max)
                   (i-sum rotary-limit-joint-i-sum)
                   (j-acc rotary-limit-joint-j-acc)
                   (bias rotary-limit-joint-bias)) rotary-limit
      (when (zerop bias) (return-from apply-impulse))
      ;; compute relative rotational velocity
      (let* ((wr (- (body-angular-velocity body-b) (body-angular-velocity body-a)))
             ;; compute normal impulse
             (j (* (- (+ bias wr)) i-sum))
             (j-old j-acc))
        (if (< bias 0d0)
            (setf j-acc (clamp (+ j-old j) 0d0 j-max))
            (setf j-acc (clamp (+ j-old j) (- j-max) 0d0)))
        (setf j (- j-acc j-old))
        ;;  apply impulse
        (decf (body-angular-velocity body-a) (* j (body-inverse-inertia body-a)))
        (incf (body-angular-velocity body-b) (* j (body-inverse-inertia body-b))))))

(defmethod get-impulse ((rotary rotary-limit-joint))
  (abs (rotary-limit-joint-j-acc rotary)))
