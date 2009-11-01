;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize speed safety))

(defun vector-x (vector)
  (svref vector 0))
(defun vector-y (vector)
  (svref vector 1))

(defconstant +zero-vector+ (vector 0 0))

(defun vector-length (vector)
  (sqrt (vector-dot vector vector)))

;; TODO - Am I sure C uses radians, like lisp?
(defun angle->vector (angle)
  "Convert radians to a normalized vector"
  (vector (cos angle)
          (sin angle)))

(defun vector->angle (vector)
  "Convert a vector to radians."
  (atan (vector-y vector)
        (vector-x vector)))

(defun vector-add (v1 v2)
  (vector (+ (vector-x v1)
             (vector-x v2))
          (+ (vector-y v1)
             (vector-y v2))))

(defun vector-neg (vector)
  (vector (- (vector-x vector))
          (- (vector-y vector))))

(defun vector-subtract (v1 v2)
  (vector (- (vector-x v1)
             (vector-x v2))
          (- (vector-y v1)
             (vector-y v2))))

(defun vector-multiply (vector s)
  (vector (* (vector-x vector) s)
          (* (vector-y vector) s)))

(defun vector-dot (v1 v2)
  (+
   (* (vector-x v1)
      (vector-x v2))
   (* (vector-y v1)
      (vector-y v2))))

(defun vector-cross (v1 v2)
  (-
   (* (vector-x v1)
      (vector-y v2))
   (* (vector-y v1)
      (vector-x v2))))

(defun vector-perp (vector)
  (vector (- (vector-y vector))
          (vector-x vector)))

(defun vector-rperp (vector)
  (vector (vector-y vector)
          (- (vector-x vector))))

(defun vector-project (v1 v2)
  (vector-multiply v2 (/ (vector-dot v1 v2)
                         (vector-dot v2 v2))))

(defun vector-rotate (v1 v2)
  (vector (- (* (vector-x v1)
                (vector-x v2))
             (* (vector-y v1)
                (vector-y v2)))
          (+ (* (vector-x v1)
                (vector-y v2))
             (* (vector-y v1)
                (vector-x v2)))))

(defun vector-unrotate (v1 v2)
  (vector (+ (* (vector-x v1)
                (vector-x v2))
             (* (vector-y v1)
                (vector-y v2)))
          (- (* (vector-y v1)
                (vector-x v2))
             (* (vector-x v1)
                (vector-y v2)))))

(defun vector-length-sq (vector)
  (vector-dot vector vector))

(defun vector-lerp (v1 v2 t)
  (vector-add
   (vector-multiply v1 (- 1 t))
   (vector-multiply v2 t)))

(defun vector-normalize (vector)
  (vector-multiply vector (/ 1 (vector-length vector))))

(defun vector-normalize-safe (vector)
  (if (and (= 0 (vector-x vector))
           (= 0 (vector-y vector)))
      +zero-vector+
      (vector-nermalize vector)))

(defun vector-clamp (vector len)
  (if (> (vector-dot vector vector)
         (* len len))
      (vector-multiply (vector-normalize vector) len)
      vector))

(defun vector-dist (v1 v2)
  (vector-length (vector-subtract v1 v2)))

(defun vector-dist-sq (v1 v2)
  (vector-length-sq (vector-subtract v1 v2)))

(defun vector-near (v1 v2 dist)
  (< (vector-dist-sq v1 v2)
     (* dist dist)))
