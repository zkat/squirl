;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defvar *contact-persistence* 3)

;;; These names can be better
(defparameter *default-iterations* 10)
(defparameter *default-elastic-iterations* 0)
(defparameter *initial-cell-size* 100.0)
(defparameter *initial-count* 1000)
(defparameter *initial-array-length* 4)

(defstruct world
  ;; Number of iterations to use in the impulse solver to solve contacts.
  (iterations *default-iterations* :type fixnum)
  ;; Number of iterations to use in the impulse solver to solve elastic collisions.
  (elastic-iterations *default-elastic-iterations* :type fixnum)
  ;; Default gravity to supply when integrating rigid body motions.
  (gravity +zero-vector+ :type vec)
  ;; Default damping to supply when integrating rigid body motions.
  (damping 1.0)

  ;; Internal slots
  (stamp 0)      ; Time stamp, incremented on every call to WORLD-STEP
  ;; Static and active shape spatial hashes
  (static-shapes (make-world-hash *initial-cell-size* *initial-count* #'shape-bbox))
  (active-shapes (make-world-hash *initial-cell-size* *initial-count* #'shape-bbox))
  ;; Bodies in the system.
  (bodies (make-array *initial-array-length* :fill-pointer t :adjustable t))
  ;; Active arbiters for the impulse solver.
  (arbiters (make-array *initial-array-length* :fill-pointer t :adjustable t))
  (contact-set (make-hash-set 0 #'arbiter-shapes-equal)) ; Persistent contact set.
  ;; Constraints in the system.
  (constraints (make-array *initial-array-length* :fill-pointer t :adjustable t)))

(defgeneric collide (actor1 actor2 contacts)
  (:method (actor1 actor2 contacts)
    (declare (ignore actor1 actor2 contacts))
    t))

;;;
;;; Body, Shape, and Joint Management
;;;

(defun world-add-shape (world shape)
  (with-place (shape. shape-) (id bbox body) shape
    (assert shape.body)
    (world-hash-insert (world-active-shapes world) shape shape.id shape.bbox)))

(defun world-add-static-shape (world shape)
  (with-place (shape. shape-) (id bbox body) shape
    (assert shape.body)
    (shape-cache-bbox shape)
    (world-hash-insert (world-static-shapes world) shape shape.id shape.bbox)))

(defun world-add-body (world body)
  (vector-push-extend body (world-bodies world))
  body)

(defun world-add-constraint (world constraint)
  (vector-push-extend constraint (world-constraints world))
  constraint)

;;; FIXME: I'm ported literally from C!
(defun shape-removal-arbiter-reject (world shape)
  (with-accessors ((arbiters world-arbiters)) world
   (let ((new-array (make-array (length arbiters) :adjustable t :fill-pointer t)))
     (loop for arbiter across arbiters
        when (with-place (arb. arbiter-) ((a shape-a) (b shape-b)) arbiter
               (and (not (eq shape arb.a)) (not (eq shape arb.b)))) do
          (vector-push arbiter new-array)
        finally
          (setf arbiters new-array)))))

(defun world-remove-shape (world shape)
  (world-hash-remove (world-active-shapes world) shape (shape-id shape))
  (shape-removal-arbiter-reject world shape))

(defun world-remove-static-shape (world shape)
  (world-hash-remove (world-static-shapes world) shape (shape-id shape))
  (shape-removal-arbiter-reject world shape))

(defun world-remove-body (world body)
  (deletef (world-bodies world) body))

(defun world-remove-constraint (world constraint)
  (deletef (world-constraints world) constraint))

;;;
;;; Point Query Functions
;;;

(defun world-point-query (function world point layers groups)
  (flet ((query-point-and-shape (point shape)
           (when (point-inside-shape-p shape point layers groups)
             (funcall function shape))))
    (world-hash-point-query #'query-point-and-shape (world-active-shapes world) point)
    (world-hash-point-query #'query-point-and-shape (world-static-shapes world) point)))

;;; Unlike the C version, this actually returns the -first- shape
;;; encountered which matches the layers, groups, and point. It
;;; uses a functional RETURN-FROM rather than the pointer juggling
;;; from the C version, for speed and clarity.
(defun world-point-query-first (world point layers groups)
  (world-point-query (fun (return-from world-point-query-first _))
                     world point layers groups))

;;; Why is this here? Shouldn't it be in another section?
(defun map-world (function world)
  (map nil function (world-bodies world)))

;;;
;;; Segment Query Functions
;;;

(defun world-shape-segment-query (function world start end layers group)
  (let (collision-p)
    (flet ((query-shape (shape)
             (prog1 1.0
               (when (segment-intersects-shape-p shape start end layers group)
                 (when function (funcall function shape 0.0 +zero-vector+))
                 (setf collision-p t)))))
      (world-hash-query-segment #'query-shape (world-static-shapes world) start end)
      (world-hash-query-segment #'query-shape (world-active-shapes world) start end)
      collision-p)))

(defun world-shape-segment-query-first (world start end layers group)
  (let (first-shape min-ratio first-normal)
   (flet ((query-shape (shape)
            (multiple-value-bind (hitp ratio normal)
                (segment-intersects-shape-p shape start end layers group)
              (when (and hitp (< ratio min-ratio))
                (setf first-shape  shape
                      min-ratio    ratio
                      first-normal normal)))))
     (world-hash-query-segment #'query-shape (world-static-shapes world) start end)
     (world-hash-query-segment #'query-shape (world-active-shapes world) start end)
     (values first-shape min-ratio first-normal))))

;;;
;;; World Hash Management
;;;

(defun resize-world-static-hash (world dimension count)
  (resize-world-hash (world-static-shapes world) dimension count)
  (rehash-world-hash (world-static-shapes world)))

(defun resize-world-active-hash (world dimension count)
  (resize-world-hash (world-active-shapes world) dimension count))

(defun rehash-world-static-data (world)
  (map-world-hash #'shape-cache-bbox (world-static-shapes world))
  (rehash-world-hash (world-static-shapes world)))

;;;
;;; Collision Detection Functions
;;;

(defun collision-impossible-p (shape1 shape2)
  (with-place (a. shape-) ((bb bbox) body group layers) shape1
    (with-place (b. shape-) ((bb bbox) body group layers) shape2
      (or (not (bbox-intersects-p a.bb b.bb))
          (eq a.body b.body)
          (and a.group b.group (eq a.group b.group))
          (zerop (logand a.layers b.layers))))))
