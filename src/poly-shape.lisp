;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype poly-axis ()
    '(cons vec real))

  (declaim (ftype (function (vec real) poly-axis) make-poly-axis))
  (defun make-poly-axis (normal distance)
    (cons normal distance))

  ;; Note that axes are read-only data structures.
  (declaim (ftype (function (poly-axis) vec) poly-axis-normal)
           (ftype (function (poly-axis) real) poly-axis-distance))
  (defun poly-axis-normal (axis)
    (car axis))
  (defun poly-axis-distance (axis)
    (cdr axis)))

(defstruct (poly (:include shape)
                 (:constructor
                  %make-poly (body length &aux
                                   (vertices (make-array length))
                                   (axes (make-array length))
                                   (transformed-vertices (make-array length))
                                   (transformed-axes (make-array length)))))
  vertices axes transformed-vertices transformed-axes)

(defun set-up-vertices (poly vertices offset)
  (loop for vert in vertices for i from 0
     for axis in (poly-axes poly)
     for a = (vec+ offset vert)
     for b = (vec+ offset (elt vertices (mod (1+ i) (length vertices))))
     for normal = (vec-normalize (vec-perp (vec- b a)))
     do (setf (elt (poly-vertices poly) i) a
              (poly-axis-normal axis) normal
              (poly-axis-distance axis) (vec. normal a))))

(defun make-poly (body vertices offset)
  (let ((poly (%make-poly body (length vertices))))
    (set-up-vertices poly vertices offset)
    (shared-shape-init poly)
    poly))

(defun validate-vertices (vertices)
  "Check that a set of vertices has a correct winding, and that they form a convex polygon."
  (loop with length = (length vertices)
     for i from 0
     for vert-a in vertices
     for vert-b = (elt vertices (mod (1+ i) length))
     for vert-c = (elt vertices (mod (+ i 2) length))
     unless (minusp (vecx (vec- vert-b vert-a) (vec- vert-c vert-b)))
     return nil
     finally (return t)))

(defun num-vertices (poly)
  (length (poly-vertices poly)))

(defun nth-vertex (index poly)
  (elt (poly-vertices poly) index))

(defun poly-value-on-axis (poly normal distance)
  "Returns the minimum distance of the polygon to the axis."
  (- (loop for vertex in (poly-vertices poly)
        minimizing (vec. normal vertex))
     distance))

(defun poly-contains-vertex-p (poly vertex)
  "Returns true if the polygon contains the vertex."
  (loop for axis across (poly-transformed-axes poly)
     never (> (vec. (poly-axis-normal axis) vertex)
              (poly-axis-distance axis))))

(defun partial-poly-contains-vertex-p (poly vertex normal)
  "Same as POLY-CONTAINS-VERTEX-P, but ignores faces pointing away from NORMAL."
  ;; More hilarity. I'm honestly not sure that this translation is correct.
  (notany (fun (unless (vec. (poly-axis-normal _) normal)
                 (plusp (- (vec. (poly-axis-normal _) vertex)
                           (poly-axis-distance _)))))
          (poly-transformed-axes poly)))

(defun poly-transform-vertices (poly position rotation)
  (setf (poly-transformed-vertices poly)
        ;; this'll have to become a MAP-INTO, I think.
        (map 'list (fun (vec+ position (vec-rotate _ rotation)))
             (poly-vertices poly))))

(defun poly-transform-axes (poly position rotation)
  (flet ((transformed-axis (axis)
           (let ((normal (vec-rotate (poly-axis-normal axis) rotation)))
             (make-poly-axis
              :normal normal
              :distance (+ (vec. position normal) (poly-axis-distance axis))))))
    (setf (poly-transformed-axes poly)
          (map 'list #'transformed-axis (poly-axes poly)))))

(defmethod shape-cache-data ((poly poly) position rotation)
  (poly-transform-vertices poly position rotation)
  (poly-transform-axes poly position rotation)
  (let* ((verts (poly-transformed-vertices poly))
         (left (vec-x (elt verts 0)))
         (right (vec-x (elt verts 0)))
         (top (vec-y (elt verts 0)))
         (bottom (vec-y (elt verts 0))))
    (loop for vert in verts
       do (setf left (min left (vec-x vert))
                right (max right (vec-x vert))
                top (max top (vec-y vert))
                bottom (min bottom (vec-y vert))))
    (make-bbox left bottom right top)))

(defmethod shape-point-query ((poly poly) point layers group)
  (declare (ignore layers group))
  (and (bbox-containts-vec-p (poly-bbox poly) point)
       (poly-contains-vertex-p poly point)))

(defmethod shape-segment-query ((poly poly) a b layers group)
  (declare (ignore layers group))
  (let ((axes (poly-axes poly))
        (vertices (poly-vertices poly)))
    (loop for vert in vertices
         for axis in axes
         for i from 0
         do
         (let* ((normal (poly-axis-normal axis))
                (a-normal (vec. a normal)))
           (unless (> (poly-axis-distance axis) a-normal)
             (let* ((b-normal (vec. b normal))
                    (ratio (/ (- (poly-axis-distance axis) a-normal)
                              (- b-normal a-normal))))
               (unless (or (< ratio 0) (< 1 ratio))
                 (let* ((point (vec-lerp a b ratio))
                        (dt (- (vecx normal point)))
                        (dt-min (- (vecx normal vert)))
                        (dt-max (- (vecx normal (elt vertices (mod (1+ i) (length vertices)))))))
                   (when (<= dt-min dt dt-max)
                     (values poly ratio normal))))))))))

