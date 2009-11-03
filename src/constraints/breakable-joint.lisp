;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct breakable-joint
  delegate space last-dt-inverse)
