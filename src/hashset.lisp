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

(defstruct hashset-bin
  elt hash next)

(defstruct (hashset
             (:constructor
              make-hashset (size test transformer &aux
                                 (table (make-array (next-prime size)
                                                    :element-type 'hashset-bin)))))
  (entries 0)
  test transformer
  (default-value nil) table)

(defun hashset-size (set)
  (length (hashset-table set)))

(defun set-full-p (set)
  (> (hashset-entries set)
     (hashset-size set)))

(defun set-resize (set &aux (new-size (next-prime (1+ (hashset-size set)))))
  (let ((new-table (make-array new-size :element-type 'hashset-bin)))
    (loop for initial-bin across (hashset-table set)
       do (loop
             for bin = initial-bin then next
             for next = (hashset-bin-next bin)
             with index = (mod (hashset-bin-hash bin) new-size)
             while bin do ; Note that these are sequential, not parallel:
               (setf (hashset-bin-next bin) (aref new-table index)
                     (aref new-table index) bin))
       finally
         (setf (hashset-table set) new-table
               (hashset-size set) new-size)))
  set)
