;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(define-constant +primes+
  (loop for x upfrom 2
     and offset in (list 1 3 1 5 3 3 1 9 7 5 3 17 27 3 1 29
                         3 21 7 17 15 9 43 35 15 29 3 -3)
     collect (+ (expt 2 x) offset)))

;;; I'm just sticking this bit here for now. Move it if you find a better place.
(define-constant +chipmunk-hash-constant+ 3344921057)

(declaim (ftype (function (fixnum fixnum) fixnum) hash-pair))
(defun hash-pair (x y)
  (logand (logxor (* x +chipmunk-hash-constant+)
                  (* y +chipmunk-hash-constant+))
          most-positive-fixnum))

(defun next-prime (n)
  (loop for prime in +primes+ when (>= prime n) return prime
     finally (error "Time to switch to native hashtables!")))

(defstruct (hash-set
             (:constructor
              make-hash-set (size test &aux
                                  (table (make-array (next-prime size)
                                                     :initial-element nil)))))
  (count 0 :type fixnum)
  (test (assert nil) :type (function (t t) boolean))
  (default-value nil)
  (table (assert nil) :type simple-vector))

(define-print-object (hash-set)
  (format t "Count: ~D" (hash-set-count hash-set)))

(defun hash-set-size (set)
  (length (hash-set-table set)))

(defun hash-set-chain (set index)
  (aref (hash-set-table set) index))
(defun (setf hash-set-chain) (new-chain set index)
  (setf (aref (hash-set-table set) index) new-chain))

(defun hash-set-full-p (set)
  (>= (hash-set-count set)
      (hash-set-size set)))

(defun hash-set-resize (set &aux (new-size (next-prime (1+ (hash-set-size set)))))
  "Adjusts `hash-set' SET to accomodate more elements"
  (let ((new-table (make-array new-size :initial-element nil)))
    (loop for chain across (hash-set-table set)
       do (loop for bin in chain for index = (mod (car bin) new-size)
             do (push bin (aref new-table index))))
    (setf (hash-set-table set) new-table)
    set))

(defun hash-set-insert (set code data &aux (index (mod code (hash-set-size set))))
  "Insert DATA into `hash-set' SET, using hash value CODE. Returns DATA if an
insertion was made, or NIL if DATA was already present in the table."
  (with-accessors ((test hash-set-test)) set
    (unless (find data (hash-set-chain set index) :test test :key #'cdr)
      (when (hash-set-full-p set)
        (hash-set-resize set))
      (push (cons code data) (hash-set-chain set index))
      (incf (hash-set-count set))
      data)))

(defun hash-set-find (set code data)
  "Searches for DATA in `hash-set' SET, using hash value CODE. On success, two
values are returned: the datum found within SET, and T. On failure, the values
are the `hash-set-default-value' for SET, and NIL. See `cl:gethash'."
  (let ((chain (hash-set-chain set (mod code (hash-set-size set)))))
    (dolist (bin chain (values (hash-set-default-value set) nil))
      (when (funcall (hash-set-test set) data (cdr bin))
        (return (values (cdr bin) t))))))

(defun hash-set-find-if (predicate set code)
  (let ((chain (hash-set-chain set (mod code (hash-set-size set)))))
    (dolist (bin chain (values (hash-set-default-value set) nil))
      (when (funcall predicate (cdr bin))
        (return (values (cdr bin) t))))))

(defun hash-set-remove (set code data)
  "Removes DATA from `hash-set' SET, using hash value CODE. On success, two
values are returned: the datum removed from SET, and T. On failure, the values
are the `hash-set-default-value' for SET, and NIL. See `cl:remhash'."
  (multiple-value-bind (datum found) (hash-set-find set code data)
    (when found
      (decf (hash-set-count set))
      (let ((index (mod code (hash-set-size set))))
        (deletef (hash-set-chain set index)
                 datum :test #'eq :key #'cdr)))
    (values datum found)))

(defun map-hash-set (function set)
  "Calls FUNCTION once on each datum in the `hash-set' SET, and returns NIL."
  (loop for chain across (hash-set-table set)
     do (dolist (bin chain)
          (funcall function (cdr bin)))))

(defun hash-set-delete-if (predicate set)
  "Deletes the items from `hash-set' SET on which PREDICATE is true. Returns NIL."
  (dotimes (index (hash-set-size set))
    (when (hash-set-chain set index)
      (let ((before (length (hash-set-chain set index))))
        (delete-iff (hash-set-chain set index) predicate :key #'cdr)
        (decf (hash-set-count set) (- before (length (hash-set-chain set index))))))))
