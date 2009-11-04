;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defparameter *shape-id-counter* 0)

(defun reset-shape-id-counter ()
  (setf *shape-id-counter* 0))

;;;
;;; Shape
;;;
(defstruct shape
  body ; body that the shape is attached to.
  bbox ; Cached Bounding Box for the shape.
  ;; Surface Properties
  ;; ------------------
  (elasticity 0) ; Coefficient of restitution.
  (friction 0) ; Coefficient of friction.
  (surface-velocity +zero-vector+) ; Surface velocity used when solving for friction
  ;; User-definable slots
  ;; --------------------
  (collision-type 0) ; User defined collision type for the shape.
  (group 0); User defined collision group for the shape.
  (layers -1); User defined layer bitmask for the shape.
  ;; Internally used slots
  ;; ---------------------
  (id (prog1 *shape-id-counter* (incf *shape-id-counter*)))) ; Unique id used as the hash value.

(defun shared-shape-init (shape)
  (shape-cache-bbox shape)
  shape)

(defun shape-cache-bbox (shape)
  (let* ((body (shape-body shape))
         (position (body-position body))
         (rotation (body-rotation body)))
    (setf (shape-bbox shape)
          (shape-cache-data shape position rotation))))

(defgeneric shape-cache-data (shape position rotation)
  (:documentation "Cache the BBox of the shape."))

(defun point-inside-shape-p (shape point layers group)
  (when (and (not (and (not (zerop group)) (shape-group shape) (= group (shape-group shape))))
             (logand layers (shape-layers shape)))
    (shape-point-query shape point)))

(defgeneric shape-point-query (shape point)
  (:documentation "Test if a point lies within a shape."))

(defun segment-intersects-shape-p (shape segment-point-a segment-point-b layers group)
  "Tests if the line segment that runs between point A and point B intersects SHAPE."
  (when (and (not (and (not (zerop group)) (shape-group shape) (= group (shape-group shape))))
             (logand layers (shape-layers shape)))
    (shape-segment-query shape segment-point-a segment-point-b)))

(defgeneric shape-segment-query (shape a b))

;;;
;;; Circles
;;;
(defstruct (circle (:constructor %make-circle (body radius center))
                   (:include shape))
  center ; Center in body space coordinates.
  radius
  transformed-center); Transformed center. (world space coordinates)

(defun make-circle (body radius offset)
  (let ((circle (%make-circle body radius offset)))
    (shared-shape-init circle)
    circle))

(defun bbox-from-circle (vec r)
  (make-bbox (- (vec-x vec) r)
             (- (vec-y vec) r)
             (+ (vec-x vec) r)
             (+ (vec-y vec) r)))

(defmethod shape-cache-data ((circle circle) position rotation)
  (setf (circle-transformed-center circle)
        (vec+ position (vec-rotate (circle-center circle) rotation)))
  (bbox-from-circle (circle-transformed-center circle) (circle-radius circle)))

(defmethod shape-point-query ((circle circle) point)
  (vec-near (circle-transformed-center circle) point (circle-radius circle)))

(defmethod shape-segment-query ((circle circle) a b)
  (let ((center (circle-transformed-center circle))
        (radius (circle-radius circle)))
    (circle-segment-query circle center radius a b)))

(defun circle-segment-query (shape center radius a b)
  (let* ((a (vec- a center))
         (b (vec- b center))
         (qa (+ (- (vec. a a) (* 2 (vec. a b))) (vec. b b)))
         (qb (- (* 2 (vec. a b)) (* 2 (vec. a a))))
         (qc (- (vec. a a) (expt radius 2)))
         (det (- (expt qb 2) (* 4 qa qc))))
    (unless (minusp det)
      (let ((ratio (/ (- (- qb) (sqrt det))
                      (* 2 qa))))
        (when (<= 0 ratio 1)
          (values shape ratio
                  (vec-normalize (vec-lerp a b ratio))))))))

;;;
;;; Segments
;;;
(defstruct (segment (:constructor %make-segment (body a b radius &aux
                                                      (normal (vec-perp
                                                               (vec-normalize (vec- b a))))))
                    (:include shape))
  a b ; endpoints (body space coords)
  normal ; normal (body space coords)
  radius ; Thickness
  trans-a trans-b ;transformed endpoints (world space coords)
  trans-normal) ;transformed normal (world space coords)

(defun make-segment (body a b radius)
  (let ((segment (%make-segment body a b radius)))
    (shared-shape-init segment)
    segment))

(defmethod shape-cache-data ((seg segment) position rotation)
  (with-accessors ((seg-ta segment-trans-a) (seg-tb segment-trans-b)
                   (seg-a segment-a) (seg-b segment-b) (seg-tnormal segment-trans-normal)
                   (seg-normal segment-normal))
      seg
    (setf seg-ta (vec+ position (vec-rotate seg-a rotation))
          seg-tb (vec+ position (vec-rotate seg-b rotation))
          seg-tnormal (vec-rotate seg-normal rotation))
    (let (left right bottom top (rad (segment-radius seg)))
      (if (< (vec-x seg-ta) (vec-x seg-tb))
          (setf left (vec-x seg-ta)
                right (vec-x seg-tb))
          (setf left (vec-x seg-tb)
                right (vec-x seg-ta)))
      (if (< (vec-y seg-ta) (vec-y seg-tb))
          (setf bottom (vec-y seg-ta)
                top (vec-y seg-tb))
          (setf bottom (vec-y seg-tb)
                top (vec-y seg-ta)))
      (make-bbox (- left rad) (- bottom rad) (+ right rad) (+ top rad)))))

(defmethod shape-point-query ((seg segment) point)
  (when (bbox-containts-vec-p (shape-bbox seg) point)
    (with-accessors ((seg-ta segment-trans-a) (seg-tb segment-trans-b) (seg-r segment-radius)
                     (seg-a segment-a) (seg-b segment-b) (seg-tnormal segment-trans-normal)
                     (seg-normal segment-normal))
        seg
      ;; calculate normal distance from segment
      (let* ((dn (- (vec. seg-tnormal point) (vec. seg-ta seg-tnormal)))
             (dist (- (abs dn) seg-r)))
        (if (plusp dist)
            (return-from shape-point-query t)
            ;; calculate tangential distance along segment
            (let ((dt (- (vecx seg-tnormal point)))
                  (dt-min (- (vecx seg-tnormal seg-ta)))
                  (dt-max (- (vecx seg-tnormal seg-tb))))
              ;; decision tree to decide which feature of the segment to collide with
              (if (<= dt dt-min)
                  (if (< dt (- dt-min seg-r))
                      (return-from shape-point-query nil)
                      (return-from shape-point-query (< (vec-length-sq (vec- seg-ta point))
                                                        (expt seg-r 2))))
                  (if (< dt dt-max)
                      (return-from shape-point-query t)
                      (if (< dt (+ dt-max seg-r))
                          (return-from shape-point-query
                            (< (vec-length-sq (vec- seg-tb point))
                               (expt seg-r 2)))
                          (return-from shape-point-query nil))))
              (return-from shape-point-query t)))))))

(defmethod shape-segment-query ((seg segment) a b)
  (let ((n (segment-trans-normal seg)))
    (when (< (vec. a n) (vec. (segment-trans-a seg) n))
      (setf n (vec-neg n)))
    (let* ((an (vec. a n))
           (bn (vec. b n))
           (d (+ (vec. (segment-trans-a seg) n) (segment-radius seg)))
           (ratio (/ (- d an) (- bn an)))) ;adlai said t is 'ratio'
      (when (< 0 ratio 1)
        (let* ((point (vec-lerp a b ratio))
               (dt (- (vecx (segment-trans-normal seg) point)))
               (dt-min (- (vecx (segment-trans-normal seg) (segment-trans-a seg))))
               (dt-max (- (vecx (segment-trans-normal seg) (segment-trans-b seg)))))
          (when (< dt-min dt dt-max)
            (values seg ratio n)
            (return-from shape-segment-query (values seg ratio n)))))
      (unless (zerop (segment-radius seg))
        (multiple-value-bind (shape1 t1 n1)
            (circle-segment-query seg (segment-trans-a seg) (segment-radius seg) a b)
          (multiple-value-bind (shape2 t2 n2)
              (circle-segment-query seg (segment-trans-b seg) (segment-radius seg) a b)
            (cond ((and shape1 (null shape2))
                   (values shape1 t1 n1))
                  ((and shape2 (null shape1))
                   (values shape2 t2 n2))
                  (t (if (< t1 t2)
                         (values shape1 t1 n1)
                         (values shape2 t2 n2))))))))))

