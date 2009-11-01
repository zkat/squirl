;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defconstant +bias-coefficient+ 0.1
  "Determines how fast penetrations resolve themselves.")
(defconstant +collision-slop+ 0.1
  "Amount of allowed penetration.  Used to reduce vibrating contacts.")