;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defstruct handle
  "Used internally to track objects added to the hash"
  object                                ; Pointer to the object
  retain                                ; Retain count
  ;; Used to prevent duplicate identification of an object within one query
  stamp)

(defstruct world-hash
  "The spatial hash is SquirL's default (and currently only) spatial index"
  size                                  ; Number of cells in the table
  cell-size                             ; Size of the cells
  bbox-function                         ; Bounding box callback
  handle-set                            ; `hash-set' of all handles
  table                                 ; Bins in use
  junk-bins                             ; The "recycle bin"
  )
