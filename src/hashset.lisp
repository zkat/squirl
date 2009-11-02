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

(defstruct hash-set-bin
  elt hash next)

(defstruct (hash-set
             (:constructor
              make-hash-set (size test transformer &aux
                                 (table (make-array (next-prime size)
                                                    :element-type 'hash-set-bin)))))
  (entries 0)
  test transformer
  (default-value nil) table)

(defun hash-set-size (set)
  (length (hash-set-table set)))

(defun hash-set-full-p (set)
  (> (hash-set-entries set)
     (hash-set-size set)))

(defun hash-set-resize (set &aux (new-size (next-prime (1+ (hash-set-size set)))))
  (let ((new-table (make-array new-size :element-type 'hash-set-bin)))
    (loop for initial-bin across (hash-set-table set)
       do (loop
             for bin = initial-bin then next
             for next = (hash-set-bin-next bin)
             with index = (mod (hash-set-bin-hash bin) new-size)
             while bin do ; Note that these are sequential, not parallel:
               (setf (hash-set-bin-next bin) (aref new-table index)
                     (aref new-table index) bin))
       finally
         (setf (hash-set-table set) new-table)))
  set)

(defun hash-set-insert (set hash ptr data
                        &aux (index (mod hash (hash-set-size set))))
  (let ((bin (aref (hash-set-table set) index)))
    (loop while (and bin (not (funcall (hash-set-test set) ptr
                                       (hash-set-bin-elt bin))))
       do (setf bin (hash-set-bin-next bin)))
    (unless bin
      (setf bin (make-hash-set-bin
                 :hash hash :elt (funcall (hash-set-transformer set) ptr data)
                 :next (aref (hash-set-table set) index))
            (aref (hash-set-table set) index) bin)
      (incf (hash-set-entries set))
      (when (hash-set-full-p set)
        (hash-set-resize set))
      (hash-set-bin-elt bin))))
