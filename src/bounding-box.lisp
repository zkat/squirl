;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize speed debug))

(defstruct (bbox (:constructor make-bbox (left bottom right top)))
  left bottom right top)

(defun bbox-intersects-p (a b)
  ;; C version:
  ;; return (a.l<=b.r && b.l<=a.r && a.b<=b.t && b.b<=a.t);
  (and (<= (bbox-left a) (bbox-right b))
       (<= (bbox-left b) (bbox-right a))
       (<= (bbox-bottom a) (bbox-top b))
       (<= (bbox-bottom b) (bbox-top a))))

(defun contains-bbox-p (bb other)
  ;; C version:
  ;; return (bb.l < other.l && bb.r > other.r && bb.b < other.b && bb.t > other.t);
  (and (< (bbox-left bb)
          (bbox-left other))
       (> (bbox-right bb)
          (bbox-right other))
       (< (bbox-bottom bb)
          (bbox-bottom other))
       (> (bbox-top bb)
          (bbox-top other))))

(defun bbox-containts-vec-p (bb vec)
  ;; C version:
  ;; return (bb.l < v.x && bb.r > v.x && bb.b < v.y && bb.t > v.y);
  (with-vec vec
    (and (< (bbox-left bb)   vec.x)
         (> (bbox-right bb)  vec.x)
         (< (bbox-bottom bb) vec.y)
         (> (bbox-top bb)    vec.y))))

(defun bbox-clamp-vec (bb vec)
  "Clamps the vector to lie within the bbox"
  ;; C version:
  ;; cpFloat x = cpfmin(cpfmax(bb.l, v.x), bb.r);
  ;; cpFloat y = cpfmin(cpfmax(bb.b, v.y), bb.t);
  ;; return cpv(x, y);
  (vec (min (max (bbox-left bb)   (vec-x vec)) (bbox-right bb))
       (min (max (bbox-bottom bb) (vec-y vec)) (bbox-top bb))))

(defun bbox-wrap-vec (bb vec)
  "Wrap a vector to a bbox."
  ;; C version:
  ;; cpFloat ix = fabsf(bb.r - bb.l);
  ;; cpFloat modx = cpfmod(v.x - bb.l, ix);
  ;; cpFloat x = (modx > 0.0f) ? modx : modx + ix;

  ;; cpFloat iy = fabsf(bb.t - bb.b);
  ;; cpFloat mody = cpfmod(v.y - bb.b, iy);
  ;; cpFloat y = (mody > 0.0f) ? mody : mody + iy;

  ;; return cpv(x + bb.l, y + bb.b);
  (vec (+ (bbox-left bb)
          (mod (- (vec-x vec) (bbox-left bb))
               (abs (- (bbox-right bb) (bbox-left bb)))))
       (+ (bbox-bottom bb)
          (mod (- (vec-y vec) (bbox-bottom bb))
               (abs (- (bbox-top bb) (bbox-bottom bb)))))))
