;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (pivot-joint (:include constraint)
             (:constructor
              make-pivot-joint
              (body-a body-b anchor1 anchor2)))
  (anchor1 +zero-vector+ :type vec)
  (anchor2 +zero-vector+ :type vec)

  (r1 +zero-vector+ :type vec)
  (r2 +zero-vector+ :type vec)
  (k1 +zero-vector+ :type vec)
  (k2 +zero-vector+ :type vec)

  (j-max-length 0d0)
  (bias +zero-vector+ :type vec)
  (j-acc +zero-vector+ :type vec))

(defmethod pre-step ((pivot pivot-joint) dt dt-inv)
  (declare (ignore dt))
  (with-accessors ((body-a pivot-joint-body-a)
                   (body-b pivot-joint-body-b)
                   (j-max-length pivot-joint-j-max-length)
                   (bias pivot-joint-bias)
                   (bias-coef pivot-joint-bias-coefficient)
                   (j-acc pivot-joint-j-acc)
                   (r1 pivot-joint-r1)
                   (r2 pivot-joint-r2)
                   (max-bias pivot-joint-max-bias)
                   (anchor1 pivot-joint-anchor1)
                   (anchor2 pivot-joint-anchor2))
      pivot
    (setf r1 (vec-rotate anchor1 (body-rotation body-a)))
    (setf r2 (vec-rotate anchor2 (body-rotation body-b)))
    ;; calculate mass tensor
    (k-tensor body-a body-b r1 r2)
    ;; calculate bias velocity
    (setf bias (vec-clamp (vec* (vec- (vec+ (body-position body-b) r2) (vec+ (body-position body-b) r1)) (- (* bias-coef dt-inv))) max-bias))
    ;; apply joint torque
    (apply-impulses body-a body-b r1 r2 j-acc)))

(defmethod apply-impulse ((pivot pivot-joint))
    (with-accessors (
                   (body-a pivot-joint-body-a)
                   (body-b pivot-joint-body-b)
                   (j-max-length pivot-joint-j-max-length)
                   (bias pivot-joint-bias)
                   (bias-coef pivot-joint-bias-coefficient)
                   (j-acc pivot-joint-j-acc)
                   (r1 pivot-joint-r1)
                   (r2 pivot-joint-r2)
                   (k1 pivot-joint-k1)
                   (k2 pivot-joint-k2)) pivot
      ;; compute relative velocity
      (let* ((vr (relative-velocity body-a body-b r1 r2))
             ;; compute normal impulse
             (j (mult-k (vec- bias vr) k1 k2))
             (j-old j-acc))
        (setf j-acc (vec-clamp (vec+ j-acc j) j-max-length))
        (setf j (vec- j-acc j-old))
        ;;  apply impulse
        (apply-impulses body-a body-b r1 r2 j))))

(defmethod get-impulse ((pivot pivot-joint))
  (vec-length (pivot-joint-j-acc pivot)))
