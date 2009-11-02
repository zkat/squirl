;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct poly-shape-axis
  normal distance)

(defstruct (poly-shape (:constructor %make-poly-shape (body vertices offset))
                       (:include shape))
  vertices axes transformed-vertices transformed-axes)
