;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defvar *contact-persistence* 3)

;;; These names can be better
(defparameter *default-iterations* 10)
(defparameter *default-elastic-iterations* 0)
(defparameter *initial-cell-size* 100d0)
(defparameter *initial-count* 1000)
(defparameter *initial-array-length* 4)

(defstruct (world
             (:constructor %make-world
                           (&key iterations elastic-iterations gravity damping)))
  ;; Number of iterations to use in the impulse solver to solve contacts.
  (iterations *default-iterations* :type fixnum)
  ;; Number of iterations to use in the impulse solver to solve elastic collisions.
  (elastic-iterations *default-elastic-iterations* :type fixnum)
  ;; Default gravity to supply when integrating rigid body motions.
  (gravity +zero-vector+ :type vec)
  ;; Default damping to supply when integrating rigid body motions.
  (damping 1d0 :type double-float)

  ;; Internal slots
  (timestamp 0 :type fixnum)  ; Time stamp, incremented on every call to WORLD-STEP
  ;; Static and active shape spatial hashes
  (static-shapes (make-world-hash *initial-cell-size* *initial-count*) :type world-hash)
  (active-shapes (make-world-hash *initial-cell-size* *initial-count*) :type world-hash)
  ;; Static and active bodies
  (static-bodies (make-adjustable-vector *initial-array-length*) :type (vector t))
  (active-bodies (make-adjustable-vector *initial-array-length*) :type (vector t))
  ;; Active arbiters for the impulse solver.
  (arbiters (make-adjustable-vector *initial-array-length*) :type (vector t))
  (contact-set (make-hash-set 0 #'arbiter-shapes-equal) :type hash-set) ; Persistent contact set.
  ;; Constraints in the system.
  (constraints (make-adjustable-vector *initial-array-length*) :type (vector t))
  arbitrator)

(defun make-world (&rest keys)
  (declare (dynamic-extent keys))
  (let ((world (apply #'%make-world keys)))
    (with-place (|| world-) (contact-set timestamp arbiters) world
      (setf (world-arbitrator world)
            ;; Let us thank Scott Lembcke, who had to hunt bugs down and code solutions
            ;; in C, while we can simply port said solutions into CL.
            (lambda (shape1 shape2)
              ;; This is a kludge. It might break on new shape types.
              (when (or (and (poly-p shape1) (circle-p shape2))
                        (and (poly-p shape1) (segment-p shape2)))
                (rotatef shape1 shape2))
              (when (collision-possible-p shape1 shape2)
                (awhen (collide-shapes shape1 shape2)
                  (let ((arbiter (ensure-arbiter world shape1 shape2 contact-set timestamp)))
                    ;; This is also a kludge... got any better ideas?
                    (setf (arbiter-shape-a arbiter) shape1
                          (arbiter-shape-b arbiter) shape2)
                    (vector-push-extend arbiter arbiters)
                    (arbiter-inject arbiter it)))))))
    world))

(define-print-object (world)
  (format t "Iterations: ~a, Elastic iterations: ~a, Gravity: ~a, Body count: ~a"
          (world-iterations world)
          (world-elastic-iterations world)
          (world-gravity world)
          (+ (length (world-active-bodies world))
             (length (world-static-bodies world)))))

(defgeneric collide (actor1 actor2 arbiter)
  (:method ((actor1 t) (actor2 t) (arbiter t)) t))

(defmacro defcollision (&body args)
  (multiple-value-bind (qualifiers lambda-list body)
      (parse-defmethod args)
    (destructuring-bind (arg-a arg-b arbiter) lambda-list
      (flet ((parse-specialized-arg (arg)
               (etypecase arg
                 (symbol (values arg t))
                 (list (destructuring-bind (arg-name specializer) arg
                         (values arg-name specializer))))))
        (multiple-value-bind (actor-a spec-a)
            (parse-specialized-arg arg-a)
          (multiple-value-bind (actor-b spec-b)
              (parse-specialized-arg arg-b)
            (with-gensyms (cnm-sym nmp-sym)
              (if (equal spec-a spec-b)
                  `(defmethod collide ,@qualifiers ,lambda-list ,@body)
                  `(flet ((handler (,actor-a ,actor-b ,arbiter ,cnm-sym ,nmp-sym)
                            ,@(pop-declarations body)
                            (flet ((call-next-method (&rest cnm-args)
                                     (apply ,cnm-sym cnm-args))
                                   (next-method-p () (funcall ,nmp-sym)))
                              (declare (ignorable #'call-next-method #'next-method-p))
                              ,@body)))
                     (defmethod collide ,@qualifiers (,arg-a ,arg-b ,arbiter)
                       (handler ,actor-a ,actor-b ,arbiter #'call-next-method #'next-method-p))
                     (defmethod collide ,@qualifiers (,arg-b ,arg-a ,arbiter)
                       (handler ,actor-a ,actor-b ,arbiter #'call-next-method #'next-method-p)))))))))))

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
         (assert (not (find body (world-static-bodies world))))
         (vector-push-extend body (world-static-bodies world))
         (dolist (shape (body-shapes body))
           (world-add-static-shape world shape)))
        (t (assert (not (find body (world-active-bodies world))))
           (vector-push-extend body (world-active-bodies world))
           (dolist (shape (body-shapes body))
             (world-add-active-shape world shape))))
  (setf (body-world body) world)
  body)

(defun world-add-constraint (world constraint)
  (assert (not (find constraint (world-constraints world))))
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
               (when function (funcall function shape 0d0 +zero-vector+))
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
  (resize-world-hash (world-static-shapes world) (float dimension 1d0) count)
  (rehash-world-hash (world-static-shapes world)))

(defun resize-world-active-hash (world dimension count)
  (resize-world-hash (world-active-shapes world) (float dimension 1d0) count))

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
                     (when (or a b) (not (collide a b _)))))))

(defun flush-arbiters (world)
  "Flush outdated arbiters."
  (with-place (|| world-) (timestamp contact-set arbiters) world
    (hash-set-delete-if (fun (> (- timestamp (arbiter-stamp _)) *contact-persistence*))
                        contact-set)
    (setf (fill-pointer arbiters) 0)))

(defun ensure-arbiter (world shape1 shape2 hash-set timestamp)
  (let* ((hash (hash-pair (shape-id shape1) (shape-id shape2)))
         (arbiter (hash-set-find-if (fun (arbiter-has-shapes-p _ shape1 shape2))
                                    hash-set hash)))
    (if arbiter
        (prog1 arbiter (setf (arbiter-stamp arbiter) timestamp))
        (hash-set-insert hash-set hash (make-arbiter world nil shape1 shape2 timestamp)))))

;;;
;;; All-Important WORLD-STEP Function
;;;

(defun resolve-collisions (world)
  "Resolves collisions between objects in WORLD."
  (with-place (|| world-) (active-shapes static-shapes arbiters arbitrator) world
    (map-world-hash #'shape-cache-data active-shapes) ; Pre-cache BBoxen
    ;; Detect collisions between active and static shapes.
    (map-world-hash (fun (world-hash-query arbitrator static-shapes _ (shape-bbox _)))
                    active-shapes)
    ;; This seems to be detecting collisions between active shapes.
    (world-hash-query-rehash arbitrator active-shapes))
  (filter-world-arbiters world))

(defun prestep-world (world dt dt-inv)
  (with-place (|| world-) (arbiters constraints) world
    ;; Prestep the arbiters
    (do-vector (arbiter arbiters)
      (arbiter-prestep arbiter dt-inv))
    ;; Prestep the constraints
    (do-vector (constraint constraints)
      (pre-step constraint dt dt-inv))))

(defun apply-elastic-impulses (world)
  (with-place (|| world-) (arbiters constraints elastic-iterations) world
    (loop repeat elastic-iterations
       do (do-vector (arbiter arbiters)
            (arbiter-apply-impulse arbiter t))
          (map nil #'apply-impulse constraints))))

(defun integrate-velocities (world dt &aux (damping (expt (/ (world-damping world)) (- dt))))
  (with-place (|| world-) (active-bodies arbiters gravity) world
    ;; Apply gravity forces.
    (do-vector (body active-bodies)
      (body-update-velocity body gravity damping dt))
    ;; Apply cached arbiter impulses.
    (map nil #'arbiter-apply-cached-impulse arbiters)))

(defun solve-impulses (world)
  "Run the impulse solver, using the old-style elastic solver if elastic iterations are disabled"
  (with-place (|| world-) (iterations elastic-iterations arbiters constraints) world
    (loop with old-style-p = (zerop elastic-iterations)
       repeat iterations do
       (do-vector (arbiter arbiters)
         (arbiter-apply-impulse arbiter old-style-p))
       (do-vector (constraint constraints)
         (apply-impulse constraint)))))

(defun world-step (world timestep)
  "Step the physical state of WORLD by DT seconds."
  (assert (not (zerop timestep)) (world) "Cannot step ~A by 0" world)
  (let* ((dt (float timestep 0d0)) (dt-inv (/ dt)))
    (with-place (|| world-) (active-bodies active-shapes) world
      (flush-arbiters world)
      (do-vector (body active-bodies)
        (body-update-position body dt)) ; Integrate positions
      (resolve-collisions world)
      (prestep-world world dt dt-inv)
      (apply-elastic-impulses world)
      (integrate-velocities world dt)
      (solve-impulses world)
      (incf (world-timestamp world)))))
