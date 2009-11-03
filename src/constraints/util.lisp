;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defun relative-velocity (body1 body2 r1 r2)
  (vec+ (body-velocity body1)
        (vec* (vec-perp r1) (body-angular-velocity body1))
        (body-velocity body2)
        (vec* (vec-perp r2) (body-angular-velocity body2))))
