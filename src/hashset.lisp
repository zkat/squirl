;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defconstant *primes*
  (loop for x upfrom 2
     and offset in (list 1 3 1 5 3 3 1 9 7 5 3 17 27 3 1 29
                         3 21 7 17 15 9 43 35 15 29 3 11 3)
     collect (+ (expt 2 x) offset)))

(defun next-prime (n)
  (loop for prime in *primes* when (>= prime n) return prime
     finally (error "Time to switch to native hashtables!")))

(defun make-hash-set-bin (hash elt next)
  (acons hash elt next))

(macrolet ((define-accessor (name internal)
             `(progn (defun ,name (bin) (,internal bin))
                     (defun (setf ,name) (new-value bin)
                       (setf (,internal bin) new-value)))))
  (define-accessor hash-set-bin-elt cdar)
  (define-accessor hash-set-bin-hash caar)
  (define-accessor hash-set-bin-next cdr))

(defstruct (hash-set
             (:constructor
              make-hash-set (size test &aux
                                  (table (make-array (next-prime size)
                                                     :initial-element nil)))))
  (count 0) test
  (default-value nil) table)

(defun hash-set-size (set)
  (length (hash-set-table set)))

(defun hash-set-full-p (set)
  (> (hash-set-count set)
     (hash-set-size set)))

(defun hash-set-resize (set &aux (new-size (next-prime (1+ (hash-set-size set)))))
  "Adjusts `hash-set' SET to accomodate more elements"
  (let ((new-table (make-array new-size :initial-element nil)))
    (with-accessors ((table hash-set-table)) set
     (loop for chain across table
        do (loop for bin in chain for index = (mod (car bin) new-size)
              do (push bin (aref new-table index))))
     (setf table new-table)
     set)))

(defun hash-set-insert (set hash data &aux (index (mod hash (hash-set-size set))))
  "Insert DATA into `hash-set' SET, using hash value HASH. Returns DATA if an
insertion was made, or NIL if DATA was already present in the table."
  (with-accessors ((table hash-set-table) (test hash-set-test)) set
    (unless (find data (aref table index) :test test :key #'cdr)
      (push (cons hash data) (aref table index))
      (incf (hash-set-count set))
      (when (hash-set-full-p set)
        (hash-set-resize set))
      data)))

(defun hash-set-find (set hash data)
  "Searches for DATA in `hash-set' SET, using hash value HASH. On success, two
values are returned: the datum found within SET, and T. On failure, returns
the `hash-set-default-value' for SET, and NIL. See `cl:gethash'."
  (let ((chain (aref (hash-set-table set)
                     (mod hash (hash-set-size set)))))
    (dolist (bin chain (values (hash-set-default-value set) nil))
      (when (funcall (hash-set-test set) data (cdr bin))
        (return (values (cdr bin) t))))))

(defun hash-set-remove (set hash data)
  "Removes DATA from `hash-set' SET, using hash value HASH. Returns the datum
removed from SET, or NIL if no such datum was found."
  (let ((found (hash-set-find set hash data)))
    (when found
      (let ((index (mod hash (hash-set-size set))))
        (setf (aref (hash-set-table set) index)
              (delete data (aref (hash-set-table set) index)
                      :test (hash-set-test set) :key #'cdr))
        found))))

(defun hash-set-map (set function)
  (loop for bin across (hash-set-table set)
     do (loop while bin do
             (funcall function (hash-set-bin-elt bin))
             (setf bin (hash-set-bin-next bin)))))

(defun hash-set-reject (set function)
  (loop for index below (length (hash-set-table set))
     for bin = (aref (hash-set-table set) index) with prev-bin
     do (loop while bin with next = (hash-set-bin-next bin)
           do (if (funcall function (hash-set-bin-elt bin))
                  (setf prev-bin bin)
                  (progn
                    (if prev-bin
                        (setf (hash-set-bin-next prev-bin) next)
                        (setf (aref (hash-set-table set) index) next))
                    (decf (hash-set-count set))))
             (setf bin next))))
