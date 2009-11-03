;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

;;;
;;; Collision resolution functions
;;;
(defun circle-to-circle-query (p1 p2 r1 r2)
  (let* ((delta (vec- p2 p1))
         (mindist (+ r1 r2))
         (distsq (vec-length-sq delta)))
   (when (< distsq
            (expt mindist 2))
     (let* ((dist (sqrt distsq)))
       (make-contact (vec+ p1 (vec* delta
                                    (+ 0.5 (maybe/ (- r1 (/ mindist 2))
                                                   dist))))
                     (vec* delta (maybe/ 1 dist))
                     (- dist mindist)
                     0)))))

(defun circle-to-segment (circle segment)
  (let* ((radius-sum (+ (circle-radius circle) (segment-radius segment)))
         (normal-distance (- (vec. (segment-trans-normal segment)
                                   (circle-transformed-center circle))
                             (vec. (segment-trans-a segment)
                                   (segment-trans-normal segment))))
         (distance (- (abs normal-distance) radius-sum)))
    (unless (plusp distance)
      (let ((tangent-distance (- (vec. (segment-trans-normal segment)
                                       (circle-transformed-center circle))))
            (tangent-distance-min (- (vec. (segment-trans-normal segment)
                                           (segment-trans-a segment))))
            (tangent-distance-max (- (vec. (segment-trans-normal segment)
                                           (segment-trans-b segment)))))
        (cond
          ((< tangent-distance tangent-distance-min)
           (when (>= tangent-distance (- tangent-distance-min
                                         radius-sum))
             (circle-to-circle-query (circle-transformed-center circle)
                                     (segment-trans-a segment)
                                     (circle-radius circle)
                                     (segment-radius segment))))
          ((< tangent-distance tangent-distance-max)
           (let ((normal (if (minusp normal-distance)
                             (segment-trans-normal segment)
                             (vec- (segment-trans-normal segment)))))
             (make-contact (vec+ (circle-transformed-center circle)
                                 (vec* normal (+ (circle-radius circle)
                                                 (/ distance 2))))
                           normal distance 0)))
          ((< tangent-distance (+ tangent-distance-max radius-sum))
           (circle-to-circle-query (circle-transformed-center circle)
                                   (segment-trans-b segment)
                                   (circle-radius circle)
                                   (segment-radius segment)))
          (t nil))))))

(defun find-min-separating-axis (poly axes)
  (let ((min-index 0)
        (min (poly-value-on-axis poly
                                 (poly-axis-normal (first axes))
                                 (poly-axis-distance (first axes)))))
    (when (<= min 0)
      (loop
         for i from 0
         for axis in (rest axes)
         do (let ((dist (poly-value-on-axis poly
                                            (poly-axis-normal axis)
                                            (poly-axis-distance axis))))
              (if (> dist 0)
                  (return)
                  (setf min dist
                        min-index i))))
      (values min-index min))))

(defun find-vertices (poly2 poly2 normal distance)
  "Add contacts for penetrating vertices"
  ;; todo
  )

(defun segment-value-on-axis (segment normal distance)
  (- (min (- (vec. normal (segment-trans-a segment)) (segment-radius segment))
          (- (vec. normal (segment-trans-b segment)) (segment-radius segment)))
     distance))

(defun find-points-behind-segment (segment poly p-dist coefficient)
  "Identify vertices that have penetrated the segment."
  ;; todo
  )

(defun segment-to-poly (segment poly)
  ;; todo
  )
(defun circle-to-poly (circle poly)
  ;; todo
  )

;;;
;;; Generic function
;;;
(defgeneric collide-shapes (a b)
  (:documentation "Collide shapes A and B together!")
  ;; Note that we don't handle segment-to-segment (yet?)
  (:method ((shape-1 circle) (shape-2 circle))
    (circle-to-circle-query (circle-transformed-center shape-1)
                            (circle-transformed-center shape-2)
                            (circle-radius shape-1)
                            (circle-radius shape-2)))
  (:method ((segment segment) (circle circle))
    (circle-to-segment circle segment))
  (:method ((circle circle) (segment segment))
    (circle-to-segment circle segment))
  (:method ((segment segment) (poly poly))
    (segment-to-poly segment poly))
  (:method ((poly poly) (segment segment))
    (segment-to-poly segment poly))
  (:method ((circle circle) (poly poly))
    (circle-to-poly circle poly))
  (:method ((poly poly) (circle circle))
    (circle-to-poly circle poly))
  (:method ((poly poly) (poly poly))
    (poly-to-poly poly poly)))

