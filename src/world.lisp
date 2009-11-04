;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defvar *contact-persistence* 3)

;;; These names can be better
(defparameter *default-iterations* 10)
(defparameter *default-elastic-iterations* 0)
(defparameter *initial-cell-size* 100.0)
(defparameter *initial-count* 1000)
(defparameter *initial-array-length* 4)

(defun contact-set-equal (shape-pair arbiter)
  (or (and (eq (car shape-pair) (arbiter-shape-a arbiter))
           (eq (cdr shape-pair) (arbiter-shape-b arbiter)))
      (and (eq (car shape-pair) (arbiter-shape-b arbiter))
           (eq (cdr shape-pair) (arbiter-shape-a arbiter)))))

(defstruct world
  ;; Number of iterations to use in the impulse solver to solve contacts.
  (iterations *default-iterations* :type fixnum)
  ;; Number of iterations to use in the impulse solver to solve elastic collisions.
  (elastic-iterations *default-elastic-iterations* :type fixnum)
  ;; Default gravity to supply when integrating rigid body motions.
  (gravity +zero-vector+ :type vector)
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
  (contact-set (make-hash-set 0 #'contact-set-equal)) ; Persistent contact set.
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
