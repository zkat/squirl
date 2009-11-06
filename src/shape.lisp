;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defparameter *shape-id-counter* 0)

(defun reset-shape-id-counter ()
  (setf *shape-id-counter* 0))

;;;
;;; Shape
;;;
(defstruct shape
  body                           ; Body to which the shape is attached
  bbox                           ; Cached BBox for the shape
  ;; Surface Properties
  (elasticity 0)                   ; Coefficient of restitution.
  (friction 0)                     ; Coefficient of friction.
  (surface-velocity +zero-vector+) ; Surface velocity used when solving for friction
  ;; Unique ID, used internally for hashing
  (id (prog1 *shape-id-counter* (incf *shape-id-counter*))))

(defgeneric print-shape (shape)
  (:method-combination progn :most-specific-last))

(define-print-object (shape)
  (print-shape shape))

(defmethod print-shape progn ((shape shape))
  (format t "Body: ~a; "
          (remove #\Space
                  (with-output-to-string (*standard-output*)
                    (print-unreadable-object ((shape-body shape)
                                              *standard-output* :identity t :type nil))))))

(defun shared-shape-init (shape)
  (pushnew shape (body-shapes (shape-body shape)))
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

(defun point-inside-shape-p (shape point)
  (shape-point-query shape point))

(defgeneric shape-point-query (shape point)
  (:documentation "Test if a point lies within a shape."))

(defun segment-intersects-shape-p (shape segment-point-a segment-point-b)
  "Tests if the line segment that runs between point A and point B intersects SHAPE."
  (shape-segment-query shape segment-point-a segment-point-b))

(defgeneric shape-segment-query (shape a b))

;;;
;;; Circles
;;;
(defstruct (circle (:constructor %make-circle (body radius center))
                   (:include shape))
  radius
  ;; Center, in body-relative and world coordinates
  center transformed-center)

(defmethod print-shape progn ((circle circle))
  (format t "Center: ~a; Radius: ~a"
          (circle-center circle) (circle-radius circle)))

(defun make-circle (body radius &optional (offset +zero-vector+))
  (shared-shape-init (%make-circle body radius offset)))

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
  radius                                ; Thickness
  ;; Body-relative endpoints & normal
  a b normal
  ;; World-relative endpoints & normal
  trans-a trans-b trans-normal)

(defmethod print-shape progn ((segment segment))
  (format t "Point A: ~a; Point B: ~a; Radius: ~a"
          (segment-a segment) (segment-b segment)
          (segment-radius segment)))

(defun make-segment (body a b radius)
  (shared-shape-init (%make-segment body a b radius)))

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
            (let ((dt (- (vec-cross seg-tnormal point)))
                  (dt-min (- (vec-cross seg-tnormal seg-ta)))
                  (dt-max (- (vec-cross seg-tnormal seg-tb))))
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
                          (return-from shape-point-query nil))))))))))

(defmethod shape-segment-query ((seg segment) a b)
  (let ((n (segment-trans-normal seg)))
    (when (< (vec. a n) (vec. (segment-trans-a seg) n))
      (setf n (vec-neg n)))
    (let* ((an (vec. a n))
           (bn (vec. b n))
           (d (+ (vec. (segment-trans-a seg) n) (segment-radius seg)))
           (ratio (/ (- d an) (- bn an))))
      (when (< 0 ratio 1)
        (let* ((point (vec-lerp a b ratio))
               (dt (- (vec-cross (segment-trans-normal seg) point)))
               (dt-min (- (vec-cross (segment-trans-normal seg) (segment-trans-a seg))))
               (dt-max (- (vec-cross (segment-trans-normal seg) (segment-trans-b seg)))))
          (when (< dt-min dt dt-max)
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

