;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

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
  (restitution 0d0)                 ; Coefficient of restitution.
  (friction 0d0)                   ; Coefficient of friction.
  (surface-velocity +zero-vector+) ; Surface velocity used when solving for friction
  ;; Unique ID, used internally for hashing
  (id (prog1 *shape-id-counter* (incf *shape-id-counter*))))

(defgeneric print-shape (shape)
  (:method-combination progn :most-specific-last))

(define-print-object (shape)
  (print-shape shape))

(defmethod print-shape progn ((shape shape))
  (format t "Body: ~a, "
          (remove #\Space
                  (with-output-to-string (*standard-output*)
                    (print-unreadable-object ((shape-body shape)
                                              *standard-output* :identity t :type nil))))))

(defun attach-shape (shape body)
  "Attaches SHAPE to BODY. All shapes must be attached to a body before they're used."
  (setf (shape-body shape) body)
  (pushnew shape (body-%shapes (shape-body shape)))
  (shape-cache-data shape)
  (when (body-world body)
    (if (staticp body)
        (world-add-static-shape (body-world body) shape)
        (world-add-active-shape (body-world body) shape)))
  body)

(defun detach-shape (shape body)
  "Detaches SHAPE from BODY. If BODY is already attached to a world, the shape is removed from there."
  (setf (body-%shapes body) (delete shape (body-%shapes body)))
  (setf (shape-body shape) nil)
  (when (body-world body)
    (world-remove-shape (body-world body) shape))
  shape)

(defgeneric compute-shape-bbox (shape)
  (:documentation "Compute the BBox of a shape."))

(defgeneric shape-cache-data (shape)
  (:documentation "Cache any cachable data about SHAPE")
  (:method :after ((shape shape))
    (setf (shape-bbox shape) (compute-shape-bbox shape))))

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
(defstruct (circle (:constructor %make-circle (radius center restitution friction))
                   (:include shape))
  radius
  ;; Center, in body-relative and world coordinates
  center transformed-center)

(defun make-circle (radius &key (center +zero-vector+) (restitution 0d0) (friction 0d0))
  (%make-circle (float radius 1d0) center (float restitution 1d0) (float friction 1d0)))

(defmethod print-shape progn ((circle circle))
  (format t "Center: ~a, Radius: ~a"
          (circle-center circle) (circle-radius circle)))

(defmethod compute-shape-bbox ((circle circle))
  (with-vec (vec (circle-transformed-center circle))
    (let ((r (circle-radius circle)))
      (make-bbox (- vec.x r) (- vec.y r) (+ vec.x r) (+ vec.y r)))))

(defmethod shape-cache-data ((circle circle))
  (with-place (body. body-) (position rotation) (circle-body circle)
    (setf (circle-transformed-center circle)
          (vec+ body.position (vec-rotate (circle-center circle) body.rotation)))))

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
(defstruct (segment (:constructor %make-segment
                                  (a b friction restitution radius
                                     &aux (normal (vec-perp (vec-normalize (vec- b a))))))
                    (:include shape))
  radius                                ; Thickness
  ;; Body-relative endpoints & normal
  a b normal
  ;; World-relative endpoints & normal
  trans-a trans-b trans-normal)

(defun make-segment (a b &key (friction 0d0) (restitution 0d0) (radius 1d0))
  (%make-segment a b (float friction 1d0) (float restitution 1d0) (float radius 1d0)))

(defmethod print-shape progn ((segment segment))
  (format t "Point A: ~a, Point B: ~a, Radius: ~a"
          (segment-a segment) (segment-b segment)
          (segment-radius segment)))

(defmethod compute-shape-bbox ((seg segment))
  (with-place (|| segment-) ((ta trans-a) (tb trans-b) (r radius)) seg
    (with-vecs (ta tb)
      (flet ((box (left right)
               (if (< ta.y tb.y)
                   (make-bbox left (- ta.y r) right (+ tb.y r))
                   (make-bbox left (- tb.y r) right (+ tb.y r)))))
        (if (< ta.x tb.x)
            (box (- ta.x r) (+ tb.x r))
            (box (- tb.x r) (+ tb.x r)))))))

(defmethod shape-cache-data ((seg segment))
  (with-place (seg.t segment-trans-) (a b normal) seg
    (with-place (seg. segment-) (a b normal body) seg
      (with-place (body. body-) (position rotation) seg.body
        (flet ((rotate (vec) (vec-rotate vec body.rotation)))
          (setf seg.ta      (vec+ body.position (rotate seg.a))
                seg.tb      (vec+ body.position (rotate seg.b))
                seg.tnormal (rotate seg.normal)))))))

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

