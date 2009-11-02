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
(defstruct shape-class
  type
  cache-data ;function with ll (shape p rot), called by SHAPE-CACHE-BOX
  point-query ;function with ll (shape p), called by SHAPE-POINT-QUERY
  segment-query ; function with ll (shape a b info), called by SHAPE-SEGMENT-QUERY
  )

;; Basic shape struct that the others inherit from.
(defstruct (shape (:constructor %make-shape))
  klass ; The "class" of a shape as defined above.
  body ; body that the shape is attached to.
  bbox ; Cached Bounding Box for the shape.
  ;; Surface Properties
  ;; ------------------
  elasticity ; Coefficient of restitution.
  u ; Coefficient of friction. ; TODO - should this just be "friction"?
  surface-velocity ; Surface velocity used when solving for friction
  ;; User-definable slots
  ;; --------------------
  data ; User defined data pointer for the shape.
  collision-type ; User defined collision type for the shape.
  group ; User defined collision group for the shape.
  layers ; User defined layer bitmask for the shape.
  ;; Internally used slots
  ;; ---------------------
  id ; Unique id used as the hash value.
  )

(defun make-shape (class body)
  ;; TODO
  )

(defun shape-cache-bbox (shape)
  "Cache the BBox of the shape."
  ;; TODO
  )

(defun shape-point-query (shape p layers group)
  "Test if a point lies within a shape."
  ;; TODO
  )

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
  ;; TODO
  )

(defun segment-query-hit-point (start end info)
  ;; TODO - We haven't done info yet. This accessor is probably wrong.
  (vec-lerp start end (info-t info)))

(defun segment-query-hit-dist (start end info)
  ;; TODO - We haven't done info yet. This accessor is probably wrong.
  (* (vec-dist start end) (info-t info)))