;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defconstant *primes*
  (loop for x upfrom 2
     and offset in (list 1 3 1 5 3 3 1 9 7 5 3 17 27 3 1 29
                         3 21 7 17 15 9 43 35 15 29 3 11 3)
     collect (+ (expt 2 x) offset)))

(defun next-prime (n)
  (loop for prime in *primes* when (> prime n) return prime
     finally (error "Time to switch to native hashtables!")))

(defstruct hashset-bin
  elt hash next)

(defstruct (hashset
             (:constructor
              make-hashset (size test transformer &aux
                                 (size (next-prime size))
                                 (bins (make-array size :element-type 'hashset-bin)))))
  (entries 0) size
  test transformer
  (default-value nil) bins)
