;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (groove-joint (:include constraint)
                         (:constructor
                          make-groove-joint
                          (body-a body-b groove-a groove-b anchor2
                                  &aux (groove-normal
                                        (vec-perp (vec-normalize (vec- groove-b groove-a)))))))
  groove-normal groove-a groove-b anchor2
  groove-transformed-normal clamp
  r1 r2 k1 k2 (j-acc +zero-vector+) j-max-length bias)


