;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defun relative-velocity (body1 body2 r1 r2)
  (vec+ (body-velocity body1)
        (vec* (vec-perp r1) (body-angular-velocity body1))
        (body-velocity body2)
        (vec* (vec-perp r2) (body-angular-velocity body2))))

(defun normal-relative-velocity (body1 body2 r1 r2 normal)
  (vec. (relative-velocity body1 body2 r1 r2) normal))

(defun apply-impulses (body1 body2 r1 r2 j)
  (body-apply-impulse body1 (vec-neg j) r1)
  (body-apply-impulse body2 j r2)
  (values))
