;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defstruct (handle (:constructor make-handle (object)))
  "Used internally to track objects added to the hash"
  object                                ; Pointer to the object
  (retain 0)                            ; Retain count
  ;; Used to prevent duplicate identification of an object within one query
  (stamp 0))

;;; These seem to just be used for C memory management
(defun retain-handle (handle)
  (incf (handle-retain handle)))
(defun release-handle (handle)
  (decf (handle-retain handle)))

(defstruct world-hash
  "The spatial hash is SquirL's default (and currently only) spatial index"
  cell-size                             ; Size of the hash's cells
  bbox-function                         ; Bounding box callback
  handle-set                            ; `hash-set' of all handles
  table                                 ; Bins in use
  junk-bins                             ; The "recycle bin"
  )

(defun world-hash-size (hash)
  (length (world-hash-table hash)))

(defun world-hash-reset-table (hash size)
  (setf (world-hash-table hash)
        (make-array size :initial-element nil)))
