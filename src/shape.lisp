;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defparameter *shape-id-counter* 0)

(defun reset-shape-id-counter ()
  (setf *shape-id-counter* 0))

(defstruct segment-query-info
  shape ; shape that was hit. NIL if no collision.
  t ; distance along query segment, will always be (or 0 1)
  n ; Normal vec of hit surface.
  )

(defstruct (shape (:constructor %make-shape (klass body)))
  klass ; The "class" of a shape as defined above.
  body ; body that the shape is attached to.
  bbox ; Cached Bounding Box for the shape.
  ;; Surface Properties
  ;; ------------------
  (elasticity 0) ; Coefficient of restitution.
  (friction 0) ; Coefficient of friction.
  (surface-velocity +zero-vector+) ; Surface velocity used when solving for friction
  ;; User-definable slots
  ;; --------------------
  data ; User defined data pointer for the shape.
  (collision-type 0) ; User defined collision type for the shape.
  (group 0); User defined collision group for the shape.
  (layers -1); User defined layer bitmask for the shape.
  ;; Internally used slots
  ;; ---------------------
  (id (prog1 *shape-id-counter* (incf *shape-id-counter*))) ; Unique id used as the hash value.
  )



(defstruct (segment-shape (:include shape))
  a b ; endpoints (body space coords)
  normal ; normal (body space coords)
  radius ; Thickness
  trans-a trans-b ;transformed endpoints (world space coords)
  trans-normal ;transformed normal (world space coords)
  )

(defun shape-cache-bbox (shape)
  (setf (shape-bbox shape)
        (shape-cache-data shape)))

(defgeneric shape-cache-data (shape)
  (:documentation "Cache the BBox of the shape."))

(defgeneric shape-point-query (shape point)
  (:documentation "Test if a point lies within a shape.")
  (:method :around ((shape shape) p layers group)
    ;; C version:
    ;; if(!(group && shape->group && group == shape->group) && (layers&shape->layers)){
    ;;    return shape->klass->pointQuery(shape, p);
    ;; }
    ;; return 0;
    (when (and (not (and group (shape-group shape) (eq group (shape-group shape))))
               (logand layers (shape-layers shape)))
      (call-next-method))))

(defgeneric shape-segment-query (x)
  (:method :around ((shape shape) a b layers group info)
    ;; if(!(group && shape->group && group == shape->group) && (layers&shape->layers)){
    ;;    shape->klass->segmentQuery(shape, a, b, info);
    ;; }
    ;; return (info->shape != NULL);
    (when (and (not (and group (shape-group shape) (eq group (shape-group shape))))
               (logand layers (shape-layers shape)))
      (call-next-method))
    (null (shape-info shape))))

(defun segment-query-hit-point (start end info)
  (vec-lerp start end (segment-query-info-t info)))

(defun segment-query-hit-dist (start end info)
  (* (vec-dist start end) (segment-query-info-t info)))

;;;
;;; Circles
;;;
(defstruct (circle (:include shape))
  center ; Center in body space coordinates.
  radius
  transformed-center; Transformed center. (world space coordinates)
  )

(defun bbox-from-circle (vec r)
  (make-bbox (- (vec-x vec) r)
             (- (vec-y vec) r)
             (+ (vec-x vec) r)
             (+ (vec-y vec) r)))

