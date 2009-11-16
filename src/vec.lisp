;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize speed))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype vec ()
    '(complex double-float))

  (declaim (ftype (function (real real) vec) vec)
           (inline vec))
  (defun vec (x y)
    (complex (float x 1d0) (float y 1d0)))

  (declaim (ftype (function (vec) double-float) vec-x vec-y)
           (inline vec-x vec-y) )
  (locally (declare (optimize (speed 1))) ; For SBCL float returns
    (defun vec-x (vec)
      (realpart vec))
    (defun vec-y (vec)
      (imagpart vec))))

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

(declaim (ftype (function (vec) boolean) vec-zerop)
         (inline vec-zerop))
(defun vec-zerop (vec)
  "Checks whether VEC is a zero vector"
  (= vec +zero-vector+))

(declaim (ftype (function (real) vec) angle->vec)
         (inline angle->vec))
(defun angle->vec (angle)
  "Convert radians to a normalized vector"
  (cis angle))

(declaim (ftype (function (vec) real) vec->angle)
         (inline vec->angle))
(defun vec->angle (vec)
  "Convert a vector to radians."
  (declare (vec vec))
  (with-vec vec
    (atan vec.y vec.x)))

(define-compiler-macro vec+ (&rest rest)
  (declare (list rest))
  (cond
    ((null rest)
     +zero-vector+)
    ((= 1 (length rest))
     (car rest))
    (t `(+ ,@rest))))

(defun vec+ (&rest vectors)
  (apply #'+ vectors))

(defun vec-neg (vec)
  (declare (vec vec))
  (- vec))

(define-compiler-macro vec- (&rest rest)
  (declare (list rest))
  `(- ,@rest))

(defun vec- (minuend &rest subtrahends)
  (declare (vec minuend))
  (apply #'- minuend subtrahends))

(declaim (ftype (function (vec double-float) vec) vec*)
         (inline vec*))
(defun vec* (vec scalar)
  "Multiplies VEC by SCALAR"
  (declare (vec vec))
  (* vec scalar))

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

(define-compiler-macro vec-rotate (&whole whole vec-form rot-form)
  (if (and (listp rot-form) (eq (car rot-form) 'vec))
      (with-gensyms (vec vec.x vec.y rot.x rot.y)
        `(let* ((,vec ,vec-form)
                (,vec.x (vec-x ,vec))
                (,vec.y (vec-y ,vec))
                (,rot.x ,(second rot-form))
                (,rot.y ,(third rot-form)))
           (vec (- (* ,vec.x ,rot.x)
                   (* ,vec.y ,rot.y))
                (+ (* ,vec.y ,rot.x)
                   (* ,vec.x ,rot.y)))))
      whole))

(declaim (ftype (function (vec vec) vec) vec-rotate vec-unrotate)
         (inline vec-rotate vec-unrotate))
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
