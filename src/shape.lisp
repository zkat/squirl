(in-package :squirl)

(defparameter *shape-id-counter* 0)

(defun reset-shape-id-counter ()
  (setf *shape-id-counter* 0))

(defstruct segment-query-info
  shape ; shape that was hit. NIL if no collision.
  t ; distance along query segment, will always be (or 0 1)
  n ; Normal vec of hit surface.
  )

;; shape class. Holds function pointers and type data.
(defstruct shape-klass
  type
  cache-data ;function with ll (shape p rot), called by SHAPE-CACHE-BOX
  point-query ;function with ll (shape p), called by SHAPE-POINT-QUERY
  segment-query ; function with ll (shape a b info), called by SHAPE-SEGMENT-QUERY
  )

;; Basic shape struct that the others inherit from.
(defstruct (shape (:constructor %make-shape (klass body)))
  klass ; The "class" of a shape as defined above.
  body ; body that the shape is attached to.
  bbox ; Cached Bounding Box for the shape.
  ;; Surface Properties
  ;; ------------------
  (elasticity 0)                   ; Coefficient of restitution.
  (u 0)                            ; Coefficient of friction. ; TODO - should this just be "friction"?
  (surface-velocity +zero-vector+)               ; Surface velocity used when solving for friction
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

(defun make-shape (klass body)
  (let ((shape (%make-shape klass body)))
    (shape-cache-bbox shape)
    shape))

(defun shape-cache-bbox (shape)
  "Cache the BBox of the shape."
  (let* ((body (shape-body shape))
         (fun (shape-klass-cache-data (shape-klass shape))))
    (setf (shape-bbox shape)
          (funcall fun shape (body-position body) (body-rotation body)))))

(defun shape-point-query (shape p layers group)
  "Test if a point lies within a shape."
  ;; I'm not sure if this is supposed to return true/false/some value. Returning 0 for now.
  ;; C version:
  ;; if(!(group && shape->group && group == shape->group) && (layers&shape->layers)){
  ;;    return shape->klass->pointQuery(shape, p);
  ;; }
  ;; return 0;
  (if (and (not (and group (shape-group shape) (eq group (shape-group shape))))
           (logand layers (shape-layers shape)))
      (funcall (shape-klass-pointer-query (shape-klass shape)) shape p)
      0 ; is this really meant to be a "False"?
      ))

(defstruct circle-shape
  shape
  center ; Center in body space coordinates.
  radius
  transformed-center; Transformed center. (world space coordinates)
  )

(defstruct segment-shape
  shape
  a b ; endpoints (body space coords)
  normal ; normal (body space coords)
  radius ; Thickness
  trans-a trans-b ;transformed endpoints (world space coords)
  trans-normal ;transformed normal (world space coords)
  )

(defun segment-query-info-print (info)
  ;; TODO
  )

(defun shape-segment-query (shape a b layers group info)
  ;; if(!(group && shape->group && group == shape->group) && (layers&shape->layers)){
  ;;    shape->klass->segmentQuery(shape, a, b, info);
  ;; }

  ;; return (info->shape != NULL);
  (when (and (not (and group (shape-group shape) (eq group (shape-group shape))))
             (logand layers (shape-layers shape)))
    (funcall (shape-klass-segment-query (shape-klass shape)) shape a b info))
  (null (shape-info shape)))

(defun segment-query-hit-point (start end info)
  ;; TODO - We haven't done info yet. This accessor is probably wrong.
  (vec-lerp start end (info-t info)))

(defun segment-query-hit-dist (start end info)
  ;; TODO - We haven't done info yet. This accessor is probably wrong.
  (* (vec-dist start end) (info-t info)))