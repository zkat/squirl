;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (ratchet-joint (:include constraint)
             (:constructor
              make-ratchet-joint
              (body-a body-b direction)))
  direction
  angle
  (i-sum 0d0)
  (bias 0d0)
  (j-acc 0d0)
  (j-max 0d0))

(defmethod pre-step ((ratchet ratchet-joint) dt dt-inv)
  (with-accessors (
                   (body-a ratchet-joint-body-a)
                   (body-b ratchet-joint-body-b)
                   (i-sum ratchet-joint-i-sum)
                   (j-max ratchet-joint-j-max)
                   (bias ratchet-joint-bias)
                   (bias-coef ratchet-joint-bias-coefficient)
                   (direction ratchet-joint-direction)
                   (j-acc ratchet-joint-j-acc)
                   (angle ratchet-joint-angle)
                   (max-bias ratchet-joint-max-bias)) ratchet
    (let* ((delta (- (body-angle body-b) (body-angle body-a)))
           (diff (- angle delta))
           (pdist (if (> diff 0d0) diff 0d0)))
      (setf angle (* direction (max (* delta direction) (* angle direction))))
      ;; calculate moment of inertia coefficient
      (setf i-sum (/ 1d0 (+ (body-inverse-inertia body-a) (body-inverse-inertia body-b))))
      ;; calculate bias velocity
      (setf bias (clamp (- (* bias-coef dt-inv pdist)) (- max-bias) max-bias))
      ;; compute max impulse
      (setf j-max (impulse-max ratchet dt))
      ;; if the bias is zero, the joint is not at a limit, reset impulse
      (when (zerop bias)
        (setf j-acc 0d0))
      ;; apply joint torque
      (decf (body-angular-velocity body-a) (* j-acc (body-inverse-inertia body-a)))
      (incf (body-angular-velocity body-b) (* j-acc (body-inverse-inertia body-b))))))

(defmethod apply-impulse ((ratchet ratchet-joint))
    (with-accessors (
                   (body-a ratchet-joint-body-a)
                   (body-b ratchet-joint-body-b)
                   (direction ratchet-joint-direction)
                   (j-max ratchet-joint-j-max)
                   (i-sum ratchet-joint-i-sum)
                   (j-acc ratchet-joint-j-acc)
                   (bias ratchet-joint-bias)) ratchet
      (when (zerop bias) (return-from apply-impulse))
      ;; compute relative rotational velocity
      (let* ((wr (- (body-angular-velocity body-b) (body-angular-velocity body-a)))
             ;; compute normal impulse
             (j (* (- (+ bias wr)) i-sum))
             (j-old j-acc))
        (setf j-acc (* (clamp (* (+ j-old j) direction) 0d0 j-max) direction))
        (setf j (- j-acc j-old))
        ;;  apply impulse
        (decf (body-angular-velocity body-a) (* j (body-inverse-inertia body-a)))
        (incf (body-angular-velocity body-b) (* j (body-inverse-inertia body-b))))))

(defmethod get-impulse ((joint ratchet-joint))
  (abs (ratchet-joint-j-acc joint)))
