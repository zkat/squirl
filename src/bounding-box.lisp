;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize speed debug))

(defstruct (bbox (:constructor make-bbox (left bottom right top)))
  left bottom right top)

(defun bbox-intersects-p (a b)
  (and (<= (bbox-left a)   (bbox-right b))
       (<= (bbox-left b)   (bbox-right a))
       (<= (bbox-bottom a) (bbox-top b))
       (<= (bbox-bottom b) (bbox-top a))))

(defun contains-bbox-p (bb other)
  (and (< (bbox-left bb)   (bbox-left other))
       (> (bbox-right bb)  (bbox-right other))
       (< (bbox-bottom bb) (bbox-bottom other))
       (> (bbox-top bb)    (bbox-top other))))

(defun bbox-containts-vec-p (bb vec)
  (with-vec vec
    (and (< (bbox-left bb)   vec.x)
         (> (bbox-right bb)  vec.x)
         (< (bbox-bottom bb) vec.y)
         (> (bbox-top bb)    vec.y))))

(defun bbox-clamp-vec (bb vec)
  "Clamps the vector to lie within the bbox"
  (vec (min (max (bbox-left bb)   (vec-x vec)) (bbox-right bb))
       (min (max (bbox-bottom bb) (vec-y vec)) (bbox-top bb))))

(defun bbox-wrap-vec (bb vec)
  "Wrap a vector to a bbox."
  (vec (+ (bbox-left bb)
          (mod (- (vec-x vec) (bbox-left bb))
               (abs (- (bbox-right bb) (bbox-left bb)))))
       (+ (bbox-bottom bb)
          (mod (- (vec-y vec) (bbox-bottom bb))
               (abs (- (bbox-top bb) (bbox-bottom bb)))))))
