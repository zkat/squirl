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

(defun hash-handle (hash handle bbox)
  (let* ((size (world-hash-size hash))
         (dim (world-hash-cell-size hash))
         (bb.t (floor (/ (bbox-top    bbox) dim)))
         (bb.l (floor (/ (bbox-left   bbox) dim)))
         (bb.r (floor (/ (bbox-right  bbox) dim)))
         (bb.b (floor (/ (bbox-bottom bbox) dim))))
    (loop for i from bb.l to bb.r
       do (loop for j from bb.b to bb.t
             for index = (hash i j size)
             for chain list = (world-hash-chain hash index)
             unless (find handle chain :key #'eq) do
               (let ((node (get-new-node hash)))
                 (setf (car node) handle)
                 (push-cons node (world-hash-chain hash index)))))))

(defun world-hash-insert (hash object id bbox)
  (with-accessors ((handle-set world-hash-handle-set)) hash
    (let ((handle (make-handle object)))
      (hash-set-insert handle-set id handle)
      (hash-handle hash handle bbox))))

(defun world-hash-rehash-object (hash object id)
  (with-accessors ((bbox-function world-hash-bbox-function)
                   (handle-set world-hash-handle-set)) hash
    (hash-handle hash (hash-set-find handle-set id object)
                 (funcall bbox-function object))))

(defun world-hash-rehash (hash)
  (clear-world-hash hash)
  (let ((bbox-fn (world-hash-bbox-function hash)))
    (hash-set-map (lambda (handle &aux (object (handle-object handle)))
                    (hash-handle hash handle (funcall bbox-fn object)))
                  (world-hash-handle-set hash))))

(defun world-hash-remove (hash object id)
  (multiple-value-bind (handle foundp)
      (hash-set-remove (world-hash-handle-set hash) id object)
    (when foundp
      (setf (handle-object handle) nil)
      (release-handle handle))))

(defun world-hash-map (function hash)
  (hash-set-map (lambda (handle)
                  (funcall function (handle-object handle)))
                (world-hash-handle-set hash)))

(defun query (function hash chain object)
  (loop for handle in chain
     unless (or (= (handle-stamp handle) (world-hash-stamp hash))
                (eq object (handle-object handle))
                (null (handle-object handle))) do
       (funcall function object (handle-object handle))
       (setf (handle-stamp handle) (world-hash-stamp hash))))

(defun world-hash-point-query (function hash point)
  (let* ((dim (world-hash-cell-size hash))
         (idx (with-vec (pt point)
                (hash (floor (/ pt.x dim)) (floor (/ pt.y dim))
                      (world-hash-size hash)))))
    (query function hash (world-hash-chain hash idx) point))
  (incf (world-hash-stamp hash)))
