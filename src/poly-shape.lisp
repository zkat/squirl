;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (inline make-poly-axis))
(defstruct (poly-axis
             (:constructor make-poly-axis (normal distance)))
  (normal +zero-vector+ :type vec)
  (distance 0d0 :type double-float))

(defstruct (poly (:include shape)
                 (:constructor
                  %make-poly (length restitution friction &aux
                                     (transformed-vertices (make-array length :element-type 'vec))
                                     (transformed-axes (make-array length :element-type 'poly-axis)))))
  (vertices (make-array 0 :element-type 'vec) :type (simple-array vec (*)))
  (axes (make-array 0 :element-type 'poly-axis) :type (simple-array poly-axis (*)))
  (transformed-vertices (assert nil) :type (simple-array vec (*)))
  (transformed-axes (assert nil) :type (simple-array poly-axis (*))))

(defmethod print-shape progn ((poly poly))
  (format t "Vertex count: ~a" (length (poly-vertices poly))))

(defun compute-new-vertices (vertices offset &aux (limit (1- (length vertices))))
  ;; The proper approach here would be to maintain a separate "fill pointer" aka index
  ;; Three advantages -- we need not return new arrays, the arrays will be simple-arrays
  ;; and thus smaller, and we can share one index for both arrays
  (loop
     with new-vertices = (make-array (1+ limit) :fill-pointer 0 :element-type 'vec)
     and  new-axes     = (make-array (1+ limit) :fill-pointer 0 :element-type 'poly-axis)
     for v in vertices for b = (vec+ offset v)
     and a = (vec+ offset (car (last vertices))) then b
     for normal = (vec-normalize (vec-perp (vec- b a)))
     do (vector-push a new-vertices)
        (vector-push (make-poly-axis normal (vec. normal a)) new-axes)
     finally (return (values (make-array (1+ limit) :element-type 'vec
                                         :initial-contents new-vertices)
                             (make-array (1+ limit) :element-type 'poly-axis
                                         :initial-contents new-axes)))))

(defun validate-vertices (vertices)
  "Check that a set of vertices has a correct winding, and that they form a convex polygon."
  (loop with tail = (last vertices 2)
     for c in vertices
     and a = (pop tail) then b
     and b = (pop tail) then c
     always (minusp (vec-cross (vec- b a) (vec- c b)))))

(defun make-poly (vertices &key (restitution 0d0) (friction 0d0) (offset +zero-vector+))
  (assert (validate-vertices vertices))
  (aprog1 (%make-poly (length vertices) (float restitution 1d0) (float friction 1d0))
    (setf (values (poly-vertices it) (poly-axes it))
          (compute-new-vertices vertices offset))))

(defun num-vertices (poly)
  (length (poly-vertices poly)))

(defun nth-vertex (index poly)
  (aref (poly-vertices poly) index))

(defun poly-value-on-axis (poly normal distance)
  "Returns the minimum distance of the polygon to the axis."
  (- (loop for vertex across (poly-transformed-vertices poly)
        minimizing (vec. normal vertex))
     distance))

(defun poly-contains-vertex-p (poly vertex)
  "Returns true if the polygon contains the vertex."
  (loop for axis across (poly-transformed-axes poly)
     never (> (vec. (poly-axis-normal axis) vertex)
              (poly-axis-distance axis))))

(defun partial-poly-contains-vertex-p (poly vertex normal)
  "Same as POLY-CONTAINS-VERTEX-P, but ignores faces pointing away from NORMAL."
  (loop for axis across (poly-transformed-axes poly)
     never (unless (> 0d0 (vec. (poly-axis-normal axis) normal))
             (> (vec. (poly-axis-normal axis) vertex)
                (poly-axis-distance axis)))))

(defmethod compute-shape-bbox ((poly poly))
  (loop for vert across (poly-transformed-vertices poly)
     minimize (vec-x vert) into minx
     maximize (vec-x vert) into maxx
     minimize (vec-y vert) into miny
     maximize (vec-y vert) into maxy
     finally (return (make-bbox minx miny maxx maxy))))

(defun poly-transform-vertices (poly position rotation)
  (declare (vec position rotation) (optimize speed))
  (do-vector ((i vertex) (poly-vertices poly))
    (setf (aref (poly-transformed-vertices poly) i)
          (vec+ position (vec-rotate vertex rotation)))))

(defun poly-transform-axes (poly position rotation)
  (declare (vec position rotation) (optimize speed))
  (do-vector ((i axis) (poly-axes poly))
    (let ((normal (vec-rotate (poly-axis-normal axis) rotation)))
      (setf (aref (poly-transformed-axes poly) i)
            (make-poly-axis normal (+ (vec. position normal) (poly-axis-distance axis)))))))

(defmethod shape-cache-data ((poly poly))
  (with-place (body. body-) (position rotation) (poly-body poly)
    (poly-transform-vertices poly body.position body.rotation)
    (poly-transform-axes poly body.position body.rotation)))

(defmethod shape-point-query ((poly poly) point)
  (and (bbox-containts-vec-p (poly-bbox poly) point)
       (poly-contains-vertex-p poly point)))

(defmethod shape-segment-query ((poly poly) a b &aux (vertices (poly-transformed-vertices poly)))
  (loop for vert across vertices
     for axis across (poly-transformed-axes poly)
     for i from 0
     do (let* ((normal (poly-axis-normal axis))
               (a-normal (vec. a normal)))
          (unless (> (poly-axis-distance axis) a-normal)
            (let* ((b-normal (vec. b normal))
                   (ratio (/ (- (poly-axis-distance axis) a-normal)
                             (- b-normal a-normal))))
              (when (<= 0 ratio 1)
                (let* ((point (vec-lerp a b ratio))
                       (dt (- (vec-cross normal point)))
                       (dt-min (- (vec-cross normal vert)))
                       (dt-max (- (vec-cross normal (aref vertices (rem (1+ i) (length vertices)))))))
                  (when (<= dt-min dt dt-max)
                    (values poly ratio normal)))))))))
