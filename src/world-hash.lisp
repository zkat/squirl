;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defstruct handle
  "Used internally to track objects added to the hash"
  object                                ; Pointer to the object
  retain                                ; Retain count
  ;; Used to prevent duplicate identification of an object within one query
  stamp)
