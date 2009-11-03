;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize speed safety))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype vec ()
    '(cons real real))

  (declaim (ftype (function (real real) vec) vec))
  (defun vec (x y)
    (cons x y))

  (declaim (ftype (function (vec) real) vec-x vec-y))
  (defun vec-x (vec)
    (car vec))
  (defun vec-y (vec)
    (cdr vec)))

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
  (with-vec vec
    (or (eq vec +zero-vector+)          ; Optimization!
        (and (zerop vec.x) (zerop vec.y)))))

;; TODO - Am I sure C uses radians, like lisp?
(defun angle->vec (angle)
  "Convert radians to a normalized vector"
  (vec (cos angle) (sin angle)))

(defun vec->angle (vec)
  "Convert a vector to radians."
  (with-vec vec
    (atan vec.y vec.x)))

(defun vec+ (&rest vectors)
  (vec (reduce #'+ vectors :key #'vec-x)
       (reduce #'+ vectors :key #'vec-y)))

(defun vec-neg (vec)
  (with-vec vec
    (vec (- vec.x) (- vec.y))))

(defun vec- (minuend &rest subtrahends)
  (with-vec minuend
    (if (null subtrahends) (vec-neg minuend)
        (vec (reduce #'- subtrahends :key #'vec-x
                     :initial-value minuend.x)
             (reduce #'- subtrahends :key #'vec-y
                     :initial-value minuend.y)))))

(defun vec* (vec scalar)
  "Multiplies VEC by SCALAR"
  (with-vec vec
    (vec (* vec.x scalar)
         (* vec.y scalar))))

(defun vec. (v1 v2)
  "Dot product of two vectors"
  (with-vecs (v1 v2)
    (+ (* v1.x v2.x)
       (* v1.y v2.y))))

(defun vecx (v1 v2)
  "Cross product of two vectors"
  (with-vecs (v1 v2)
    (- (* v1.x v2.y)
       (* v1.y v2.x))))

(defun vec-perp (vec)
  "Returns a new vector rotated PI/2 counterclockwise from VEC"
  (with-vec vec
    (vec (- vec.y) vec.x)))

(defun vec-rperp (vec)
  "Returns a new vector rotated PI/2 clockwise from VEC"
  (with-vec vec
    (vec vec.y (- vec.x))))

(defun vec-project (v1 v2)
  "Returns the projection of V1 onto V2"
  (vec* v2 (/ (vec. v1 v2) (vec. v2 v2))))

(defun vec-rotate (vec rot)
  "Rotates VEC by (vec->angle ROT) radians. ROT should be a unit vec.
This function is symmetric between VEC and ROT."
  (with-vecs (vec rot)
    (vec (- (* vec.x rot.x)
            (* vec.y rot.y))
         (+ (* vec.x rot.y)
            (* vec.y rot.x)))))

(defun vec-unrotate (vec rot)
  "Rotates VEC by (- (vec->angle ROT)) radians. ROT should be a unit vec.
This function is symmetric between VEC and ROT."
  (with-vecs (vec rot)
    (vec (+ (* vec.x rot.x)
            (* vec.y rot.y))
         (- (* vec.y rot.x)
            (* vec.x rot.y)))))

(defun vec-length-sq (vec)
  "Returns the square of a vector's length"
  (vec. vec vec))

(defun vec-length (vec)
  "Returns the vector's length"
  (sqrt (vec-length-sq vec)))

(defun vec-lerp (v1 v2 ratio)
  "Linear interpolation of the vectors and ratio"
  (vec+ (vec* v1 (- 1 ratio))
        (vec* v2 ratio)))

(defun vec-normalize (vec)
  "Normalizes a nonzero vector"
  (vec* vec (/ (vec-length vec))))

(defun vec-normalize-safe (vec)
  "Normalizes a vector"
  (if (vec-zerop vec) +zero-vector+
      (vec-normalize vec)))

(defun vec-clamp (vec len)
  (if (> (vec-length-sq vec) (* len len))
      (vec* (vec-normalize vec) len)
      vec))

(defun vec-dist-sq (v1 v2)
  (vec-length-sq (vec- v1 v2)))

(defun vec-dist (v1 v2)
  (vec-length (vec- v1 v2)))

(defun vec-near (v1 v2 dist)
  (< (vec-dist-sq v1 v2)
     (* dist dist)))
