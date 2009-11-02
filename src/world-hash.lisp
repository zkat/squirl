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

;;; Used for the `world-hash-handle-set'
(defun handle-equal (object handle)
  (eql object (handle-object handle)))
(defun handle-transform (object)
  (make-handle object))

(defun make-world-hash-table (size)
  (make-array (next-prime size) :initial-element nil))

(defstruct (world-hash
             (:constructor make-world-hash
                           (cell-size size bbox-function &aux
                                      (table (make-world-hash-table size)))))
  "The spatial hash is SquirL's default (and currently only) spatial index"
  cell-size                             ; Size of the hash's cells
  bbox-function                         ; Bounding box callback
  (handle-set (make-hash-set 0 #'handle-equal)) ; `hash-set' of all handles
  table                                         ; Bins in use
  (junk nil)                                    ; The "recycle bin"
  (stamp 1)            ; Incremented on each query; see `handle-stamp'
  )

(defun world-hash-size (hash)
  (length (world-hash-table hash)))

(defun world-hash-chain (hash index)
  (aref (world-hash-table hash) index))
(defun (setf world-hash-chain) (new-chain hash index)
  (setf (aref (world-hash-table hash) index) new-chain))

(defun clear-hash-cell (hash index)
  "Releases the handles under INDEX in `world-hash' HASH, and links the
list structure into the `world-hash-junk'."
  (do* ((chain (world-hash-chain hash index) next)
        ;; We need to hang onto the CDR because we 'recycle' NODE
        (next (cdr chain) (cdr chain)))
       ((null chain) (setf (world-hash-chain hash index) nil))
    (release-handle (car chain)) ; Is this just reference counting?
    (push-cons chain (world-hash-junk hash))))

(defun clear-world-hash (hash)
  "Clear all cells in the `world-hash' HASH"
  (dotimes (index (world-hash-size hash))
    (clear-hash-cell hash index)))

(defun resize-world-hash (hash new-cell-size new-size)
  "Resize `world-hash' HASH to the specified dimensions"
  (clear-world-hash hash)
  (setf (world-hash-cell-size hash) new-cell-size
        (world-hash-table hash) (make-world-hash-table new-size)))

(defun get-new-node (hash)
  "Get a recycled node or cons a new one"
  (let ((node (pop (world-hash-junk hash))))
    (if (null node) (cons nil nil) node)))

(defun hash (x y n)
  "Hash X, Y, and N to generate a hash code"
  (expt-mod (* x 2185031351) (* y 4232417593) n))
