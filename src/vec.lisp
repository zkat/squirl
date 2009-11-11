;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize speed))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype vec ()
    '(simple-array double-float (2)))

  (declaim (ftype (function (real real) vec) vec)
           (inline vec))
  (defun vec (x y)
    (let ((v (make-array 2 :element-type 'double-float)))
      ;; Ignore these warnings; we'll be using complexes soon anyways
      (setf (aref v 0) (float x 1d0)
            (aref v 1) (float y 1d0))
      v))

  (declaim (ftype (function (vec) double-float) vec-x vec-y)
           (inline vec-x vec-y) )
  (locally (declare (optimize (speed 1))) ; For SBCL float returns
    (defun vec-x (vec)
      (aref vec 0))
    (defun vec-y (vec)
      (aref vec 1))))

;; this doesn't work very well, since with-place interns things into
;; wrong package...
;; probably worth adding at some point though (at least the first few, need
;; to determine final list once we have more real usage to profile, and
;; calling code is better optimized)
;;(declaim (inline vec* vec-perp vec-cross vec. vec-rotate vec-neg))

(define-constant +zero-vector+ (vec 0 0))

(defmacro with-vec (form &body body)
  "FORM is either a symbol bound to a `vec', or a list of the form:
  (name form)
where NAME is a symbol, and FORM evaluates to a `vec'.
WITH-VEC binds NAME.x and NAME.y in the same manner as `with-accessors'."
  (let ((name (if (listp form) (first form) form))
        (place (if (listp form) (second form) form)))
    `(with-place (,(format nil "~A." name) vec-)
         (x y) ,place
       ,@body)))

(defmacro with-vecs ((form &rest forms) &body body)
  "Convenience macro for nesting WITH-VEC forms"
  `(with-vec ,form ,@(if forms `((with-vecs ,forms ,@body)) body)))

(defun vec-zerop (vec)
  "Checks whether VEC is a zero vector"
  (declare (vec vec))
  (with-vec vec
    (or (eq vec +zero-vector+)          ; Optimization!
        (and (zerop vec.x) (zerop vec.y)))))

(defun angle->vec (angle)
  "Convert radians to a normalized vector"
  (let ((angle (float angle 1d0)))
    (vec (cos angle) (sin angle))))


(defun vec->angle (vec)
  "Convert a vector to radians."
  (declare (vec vec))
  (with-vec vec
    (atan vec.y vec.x)))

(define-compiler-macro vec+ (&whole whole &rest rest)
  (declare (list rest))
  (cond
    ((null rest)
     +zero-vector+)
    ((= 1 (length rest))
     (car rest))
    ((= 2 (length rest))
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,(first rest))
              (,b ,(second rest)))
          (declare (vec ,a ,b))
          (vec (+ (vec-x ,a) (vec-x ,b))
               (+ (vec-y ,a) (vec-y ,b))))))
    (t whole)))

(defun vec+ (&rest vectors)
  (vec (reduce #'+ vectors :key #'vec-x)
       (reduce #'+ vectors :key #'vec-y)))

(defun vec-neg (vec)
  (declare (vec vec))
  (with-vec vec
    (vec (- vec.x) (- vec.y))))


(define-compiler-macro vec- (&whole whole &rest rest)
  (declare (list rest))
  (cond
    ((= 1 (length rest))
     `(vec-neg ,(car rest)))
    ((= 2 (length rest))
     (let ((a (gensym))
           (b (gensym)))
       `(let ((,a ,(first rest))
              (,b ,(second rest)))
          (declare (vec ,a ,b))
          (vec (- (vec-x ,a) (vec-x ,b))
               (- (vec-y ,a) (vec-y ,b))))))
    (t whole)))

(defun vec- (minuend &rest subtrahends)
  (declare (vec minuend))
  (with-vec minuend
    (if (null subtrahends) (vec-neg minuend)
        (loop
           with x double-float = minuend.x
           and  y double-float = minuend.y
           for vec in subtrahends do
             (decf x (vec-x vec))
             (decf y (vec-y vec))
           finally (return (vec x y))))))

(declaim (ftype (function (vec double-float) vec) vec*)
         (inline vec*))
(defun vec* (vec scalar)
  "Multiplies VEC by SCALAR"
  (declare (vec vec))
  (let ((scalar (float scalar 1d0)))
    (with-accessors ((vec.x vec-x) (vec.y vec-y)) vec
      (vec (* vec.x scalar) (* vec.y scalar)))))

(declaim (ftype (function (vec vec) double-float) vec. vec-cross)
         (inline vec. vec-cross))
(defun vec. (v1 v2)
  "Dot product of two vectors"
  (declare (vec v1 v2))
  (with-vecs (v1 v2)
    (+ (* v1.x v2.x)
       (* v1.y v2.y))))

(defun vec-cross (v1 v2)
  "Cross product of two vectors"
  (declare (vec v1 v2))
  (with-vecs (v1 v2)
    (- (* v1.x v2.y)
       (* v1.y v2.x))))

(declaim (ftype (function (vec) vec) vec-perp vec-rperp)
         (inline vec-perp vec-rperp))
(defun vec-perp (vec)
  "Returns a new vector rotated PI/2 counterclockwise from VEC"
  (declare (vec vec))
  (with-vec vec
    (vec (- vec.y) vec.x)))

(defun vec-rperp (vec)
  "Returns a new vector rotated PI/2 clockwise from VEC"
  (declare (vec vec))
  (with-vec vec
    (vec vec.y (- vec.x))))

(defun vec-project (v1 v2)
  "Returns the projection of V1 onto V2"
  (declare (vec v1 v2))
  (vec* v2 (/ (vec. v1 v2) (vec. v2 v2))))

(defun vec-rotate (vec rot)
  "Rotates VEC by (vec->angle ROT) radians. ROT should be a unit vec.
This function is symmetric between VEC and ROT."
  (declare (vec vec rot))
  (with-vecs (vec rot)
    (vec (- (* vec.x rot.x)
            (* vec.y rot.y))
         (+ (* vec.x rot.y)
            (* vec.y rot.x)))))

(defun vec-unrotate (vec rot)
  "Rotates VEC by (- (vec->angle ROT)) radians. ROT should be a unit vec.
This function is symmetric between VEC and ROT."
  (declare (vec vec rot))
  (with-vecs (vec rot)
    (vec (+ (* vec.x rot.x)
            (* vec.y rot.y))
         (- (* vec.y rot.x)
            (* vec.x rot.y)))))

(declaim (ftype (function (vec) (double-float 0d0)) vec-length vec-length-sq)
         (inline vec-length vec-length-sq))
(defun vec-length-sq (vec)
  "Returns the square of a vector's length"
  (vec. vec vec))

(defun vec-length (vec)
  "Returns the vector's length"
  (sqrt (vec-length-sq vec)))

(defun vec-lerp (v1 v2 ratio)
  "Linear interpolation of the vectors and ratio"
  (declare (vec v1 v2))
  (let ((ratio (float ratio 1d0)))
    (vec+ (vec* v1 (- 1d0 ratio))
          (vec* v2 ratio))))

(defun vec-normalize (vec)
  "Normalizes a nonzero vector"
  (declare (vec vec))
  (vec* vec (/ (vec-length vec))))

(defun vec-normalize-safe (vec)
  "Normalizes a vector"
  (declare (vec vec))
  (if (vec-zerop vec) +zero-vector+
      (vec-normalize vec)))

(defun vec-clamp (vec len)
  (declare (vec vec))
  (let ((len (float len 1d0)))
    (if (and (< len (sqrt most-positive-double-float))
             (> (vec-length-sq vec) (* len len)))
        (vec* (vec-normalize vec) len)
        vec)))

(defun vec-dist-sq (v1 v2)
  (declare (vec v1 v2))
  (vec-length-sq (vec- v1 v2)))

(defun vec-dist (v1 v2)
  (declare (vec v1 v2))
  (vec-length (vec- v1 v2)))

(defun vec-near (v1 v2 dist)
  (declare (vec v1 v2))
  (let ((dist (float dist 1d0)))
    (< (vec-dist-sq v1 v2)
       (* dist dist))))
