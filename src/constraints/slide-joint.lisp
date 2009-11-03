;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (slide-joint (:include constraint)
             (:constructor
              make-slide-joint
              (body-a body-b anchor1 anchor2
                      &aux (distance
                            (let ((p1 (vec+ (body-position body-a)
                                            (vec-rotate anchor1 (body-rotation body-b))))
                                  (p2 (vec+ (body-position body-b)
                                            (vec-rotate anchor2 (body-rotation body-a)))))
                                       (vec-length (vec- p1 p2)))))))

  (anchor1 +zero-vector+ :type vec)
  (anchor2 +zero-vector+ :type vec)
  (min-slide 0)
  (max-slide 0)
  (r1 +zero-vector+ :type vec)
  (r2 +zero-vector+ :type vec)
  (n +zero-vector+ :type vec)
  (n-mass 0)
  (jn-acc 0)
  (jn-max 0)
  (bias 0))

(defmethod pre-step ((joint slide-joint) dt dt-inv)
  (with-accessors (
                   (bias-coef slide-joint-bias-coefficient)
                   (max-bias slide-joint-max-bias)
                   (body-a slide-joint-body-a)
                   (body-b slide-joint-body-b)
                   (anchor1 slide-joint-anchor1)
                   (anchor2 slide-joint-anchor2)
                   (min-slide slide-joint-min-slide)
                   (max-slide slide-joint-max-slide)
                   (r1 slide-joint-r1)
                   (r2 slide-joint-r2)
                   (n slide-joint-n)
                   (n-mass slide-joint-n-mass)
                   (bias slide-joint-bias)
                   (jn-max slide-joint-jn-max)
                   (jn-acc slide-joint-jn-acc)) joint

    (let* ((delta (vec- (vec+ (body-position body-b) r2) (vec+ (body-position body-a) r1)))
           (dist (vec-length delta))
           (pdist 0.0))
      (setf r1 (vec-rotate anchor1 body-a))
      (setf r2 (vec-rotate anchor2 body-b))
      (if (>  dist max-slide)
          (setf pdist (- dist max-slide))
          (progn
            (setf pdist (- min-slide dist))
            (setf dist (- dist))))
      (setf n (vec* delta (maybe/ 1.0 dist)))
      ;; calculate mass normal
      (setf n-mass (/ 1.0 (k-scalar body-a body-b r1 r2  n )))
      ;; calculate bias velocity
      (setf bias (clamp (- (* bias-coef dt-inv pdist)) (- max-bias) max-bias)) ;;;check up bracket thing from C code
      ;; compute max impulse
      (setf jn-max (impulse-max joint dt))
      ;;apply accumulated impulse
      (when (zerop bias)
        ;; if bias is 0, then the joint is not at a limit
        (setf jn-acc 0.0))
      (apply-impulses body-a body-b r1 r2 (vec* n jn-acc)))))

(defmethod apply-impulse (joint)
  (with-accessors (
                   (body-a slide-joint-body-a)
                   (body-b slide-joint-body-b)
                   (r1 slide-joint-r1)
                   (r2 slide-joint-r2)
                   (n slide-joint-n)
                   (n-mass slide-joint-n-mass)
                   (bias slide-joint-bias)
                   (jn-max slide-joint-jn-max)
                   (jn-acc slide-joint-jn-acc)) joint
    (when (zerop bias);; this could be moved up for a minor performance boost
      (return-from apply-impulse))
    ;; compute relative velocity
    (let* ((vr (relative-velocity body-a body-b r1 r2))
           (vrn (vec. vr n))
           ;; compute normal impulse
           (jn (* (- bias vrn) n-mass))
           (jn-old jn-acc))
      (setf jn-acc (clamp (+ jn-old jn) (- jn-max) 0.0))
      (setf jn (- jn-acc jn-old))
      ;; apply impulse
      (apply-impulses body-a body-b  r1 r2 (vec* n jn)))))

(defmethod get-impulse (joint)
   (if (< (slide-joint-jn-acc joint) 0)
       (- (slide-joint-jn-acc joint))
       (slide-joint-jn-acc joint)))