;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize debug safety))
(defvar *contact-persistence* 3)

;;; These names can be better
(defparameter *default-iterations* 10)
(defparameter *default-elastic-iterations* 0)
(defparameter *initial-cell-size* 100.0)
(defparameter *initial-count* 1000)
(defparameter *initial-array-length* 4)

(defstruct (world
             (:constructor make-world
                           (&key iterations elastic-iterations gravity damping)))
  ;; Number of iterations to use in the impulse solver to solve contacts.
  (iterations *default-iterations* :type fixnum)
  ;; Number of iterations to use in the impulse solver to solve elastic collisions.
  (elastic-iterations *default-elastic-iterations* :type fixnum)
  ;; Default gravity to supply when integrating rigid body motions.
  (gravity +zero-vector+ :type vec)
  ;; Default damping to supply when integrating rigid body motions.
  (damping 1.0)

  ;; Internal slots
  (timestamp 0)      ; Time stamp, incremented on every call to WORLD-STEP
  ;; Static and active shape spatial hashes
  (static-shapes (make-world-hash *initial-cell-size* *initial-count*))
  (active-shapes (make-world-hash *initial-cell-size* *initial-count*))
  ;; Static and active bodies
  (static-bodies (make-adjustable-vector *initial-array-length*))
  (active-bodies (make-adjustable-vector *initial-array-length*))
  ;; Active arbiters for the impulse solver.
  (arbiters (make-adjustable-vector *initial-array-length*))
  (contact-set (make-hash-set 0 #'arbiter-shapes-equal)) ; Persistent contact set.
  ;; Constraints in the system.
  (constraints (make-adjustable-vector *initial-array-length*)))

(define-print-object (world)
  (format t "Iterations: ~a; Elastic iterations: ~a; Gravity: ~a; Body count: ~a"
          (world-iterations world)
          (world-elastic-iterations world)
          (world-gravity world)
          (+ (length (world-active-bodies world))
             (length (world-static-bodies world)))))

(defgeneric collide (actor1 actor2 contacts)
  (:method (actor1 actor2 contacts)
    (declare (ignore actor1 actor2 contacts))
    t))

;;;
;;; Body, Shape, and Joint Management
;;;

(defun world-add-static-shape (world shape)
  (with-place (shape. shape-) (id bbox body) shape
    (assert shape.body)
    (shape-cache-data shape)
    (world-hash-insert (world-static-shapes world)
                       shape shape.id shape.bbox)))

(defun world-add-active-shape (world shape)
  (with-place (shape. shape-) (id bbox body) shape
    (assert shape.body)
    (world-hash-insert (world-active-shapes world) shape shape.id shape.bbox)))

(defun world-add-body (world body)
  ;; FLET or MACROLET this up please
  (cond ((staticp body)
         (vector-push-extend body (world-static-bodies world))
         (dolist (shape (body-shapes body))
           (world-add-static-shape world shape)))
        (t (vector-push-extend body (world-active-bodies world))
           (dolist (shape (body-shapes body))
             (world-add-active-shape world shape))))
  body)

(defun world-add-constraint (world constraint)
  (vector-push-extend constraint (world-constraints world))
  constraint)

(defun shape-removal-arbiter-reject (world shape)
  (delete-iff (world-arbiters world)
              (fun (with-place (arb. arbiter-) ((a shape-a) (b shape-b)) _
                     (and (not (eq shape arb.a)) (not (eq shape arb.b)))))))

(defun world-remove-shape (world shape)
  (world-hash-remove (if (staticp (shape-body shape))
                         (world-static-shapes world)
                         (world-active-shapes world))
                     shape (shape-id shape))
  (shape-removal-arbiter-reject world shape))

(defun world-remove-body (world body)
  (map nil (fun (world-remove-shape world _))  (body-shapes body))
  (if (staticp body) ; Needs more macrolet
      (deletef (world-static-bodies world) body)
      (deletef (world-active-bodies world) body)))

(defun world-remove-constraint (world constraint)
  (deletef (world-constraints world) constraint))

;;;
;;; Point Query Functions
;;;

(defun world-point-query (function world point)
  (flet ((query-point-and-shape (point shape)
           (when (point-inside-shape-p shape point)
             (funcall function shape))))
    (world-hash-point-query #'query-point-and-shape (world-active-shapes world) point)
    (world-hash-point-query #'query-point-and-shape (world-static-shapes world) point)))

;;; Unlike the C version, this actually returns the -first- shape
;;; encountered which matches the layers, groups, and point. It
;;; uses a functional RETURN-FROM rather than the pointer juggling
;;; from the C version, for speed and clarity.
(defun world-point-query-first (world point)
  (world-point-query (fun (return-from world-point-query-first _))
                     world point))

;;;
;;; Body Convenience Functions
;;;

(defun map-world (function world)
  "Calls FUNCTION on each body in WORLD"
  (map nil function (world-static-bodies world))
  (map nil function (world-active-bodies world)))

(defun world-bodies (world)
  (with-place (|| world-) (active-bodies static-bodies) world
   (concatenate 'vector static-bodies active-bodies)))

;;;
;;; Segment Query Functions
;;;

(defun world-shape-segment-query (function world start end &aux collisionp)
  (flet ((query-shape (shape)
           (prog1 1.0
             (when (segment-intersects-shape-p shape start end)
               (when function (funcall function shape 0.0 +zero-vector+))
               (setf collisionp t)))))
    (world-hash-query-segment #'query-shape (world-static-shapes world) start end)
    (world-hash-query-segment #'query-shape (world-active-shapes world) start end)
    collisionp))

(defun world-shape-segment-query-first (world start end &aux first-shape min-ratio first-normal)
  (flet ((query-shape (shape)
           (multiple-value-bind (hitp ratio normal)
               (segment-intersects-shape-p shape start end)
             (when (and hitp (< ratio min-ratio))
               (setf first-shape  shape
                     min-ratio    ratio
                     first-normal normal)))))
    (world-hash-query-segment #'query-shape (world-static-shapes world) start end)
    (world-hash-query-segment #'query-shape (world-active-shapes world) start end)
    (values first-shape min-ratio first-normal)))

;;;
;;; World Hash Management
;;;

(defun resize-world-static-hash (world dimension count)
  (resize-world-hash (world-static-shapes world) dimension count)
  (rehash-world-hash (world-static-shapes world)))

(defun resize-world-active-hash (world dimension count)
  (resize-world-hash (world-active-shapes world) dimension count))

(defun rehash-world-static-data (world)
  (map-world-hash #'shape-cache-data (world-static-shapes world))
  (rehash-world-hash (world-static-shapes world)))

;;;
;;; Collision Detection Functions
;;;

(defun collision-possible-p (shape1 shape2)
  (with-place (a. shape-) ((bb bbox) body group layers) shape1
    (with-place (b. shape-) ((bb bbox) body group layers) shape2
      (and (not (eq a.body b.body))
           (bbox-intersects-p a.bb b.bb)))))

;;;
;;; Arbiter Frobbing Functions
;;;

(defun filter-world-arbiters (world)
  "Filter arbiter list based on collisions."
  (delete-iff (world-arbiters world)
              (fun (let ((a (body-actor (shape-body (arbiter-shape-a _))))
                         (b (body-actor (shape-body (arbiter-shape-b _)))))
                     (when (or a b) (not (collide a b (arbiter-contacts _))))))))

(defun flush-arbiters (world)
  "Flush outdated arbiters."
  (with-place (|| world-) (timestamp contact-set arbiters) world
    (hash-set-delete-if (fun (> (- timestamp (arbiter-stamp _)) *contact-persistence*))
                        contact-set)
    (setf (fill-pointer arbiters) 0)))

(defun ensure-arbiter (shape1 shape2 hash-set timestamp)
  (let* ((hash (hash-pair (shape-id shape1) (shape-id shape2)))
         (arbiter (hash-set-find-if (fun (arbiter-has-shapes-p _ shape1 shape2))
                                    hash-set hash)))
    (if arbiter
        (prog1 arbiter (setf (arbiter-stamp arbiter) timestamp))
        (hash-set-insert hash-set hash (make-arbiter nil shape1 shape2 timestamp)))))

;;;
;;; All-Important WORLD-STEP Function
;;;

(defun resolve-collisions (world)
  "Resolves collisions between objects in WORLD."
  (with-place (|| world-) (contact-set active-shapes static-shapes timestamp arbiters) world
    (map-world-hash #'shape-cache-data active-shapes) ; Pre-cache BBoxen
    (flet ((arbitrate (shape1 shape2)
             (when (collision-possible-p shape1 shape2)
               (awhen (collide-shapes shape1 shape2)
                 (let ((arbiter (ensure-arbiter shape1 shape2 contact-set timestamp)))
                   (vector-push-extend arbiter arbiters)
                   (arbiter-inject arbiter it))))))
      ;; Detect collisions between active and static shapes.
      (map-world-hash (fun (world-hash-query #'arbitrate static-shapes _ (shape-bbox _)))
                      active-shapes)
      ;; This seems to be detecting collisions between active shapes.
      (world-hash-query-rehash #'arbitrate active-shapes)))
  (filter-world-arbiters world))

(defun prestep-world (world dt dt-inv)
  (with-place (|| world-) (arbiters constraints) world
    ;; Prestep the arbiters
    (map nil (fun (arbiter-prestep _ dt-inv)) arbiters)
    ;; Prestep the constraints
    (map nil (fun (pre-step _ dt dt-inv)) constraints)))

(defun apply-elastic-impulses (world)
  (with-place (|| world-) (arbiters constraints elastic-iterations) world
    (loop repeat elastic-iterations do
         (map nil (fun (arbiter-apply-impulse _ 1.0)) arbiters)
         (map nil #'apply-impulse constraints))))

(defun integrate-velocities (world dt &aux (damping (expt (world-damping world) (- dt))))
  (with-place (|| world-) (active-bodies arbiters gravity) world
    ;; Apply gravity forces.
    (do-vector (body active-bodies)
      (body-update-velocity body gravity damping dt))
    ;; Apply cached arbiter impulses.
    (map nil #'arbiter-apply-cached-impulse arbiters)))

(defun solve-impulses (world)
  "Run the impulse solver, using the old-style elastic solver if elastic iterations are disabled"
  (with-place (|| world-) (iterations elastic-iterations arbiters constraints) world
    (loop with elastic-coef = (if (zerop elastic-iterations) 1.0 0.0)
       repeat iterations do
       (map nil (fun (arbiter-apply-impulse _ elastic-coef)) arbiters)
       (map nil #'apply-impulse constraints))))

(defun world-step (world dt &aux (dt-inv (/ dt))) ; This is our assertion
  "Step the physical state of WORLD by DT seconds."
  (with-place (|| world-) (active-bodies active-shapes) world
    (flush-arbiters world)
    (do-vector (body active-bodies)
      (body-update-position body dt)) ; Integrate positions
    (resolve-collisions world)
    (prestep-world world dt dt-inv)
    (apply-elastic-impulses world)
    (integrate-velocities world dt)
    (solve-impulses world)
    (incf (world-timestamp world))))

