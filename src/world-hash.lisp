;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

;; Eventually, this should be (cons t fixnum)
(deftype handle ()
  "Used internally to track objects added to the hash"
  '(cons t integer))
(defun make-handle (object)
  (cons object 0))

(defun handle-object (handle) (car handle))
(defun handle-stamp  (handle) (cdr handle))

;;; Used for the `world-hash-handle-set'
(defun handle-equal (object handle)
  (eql object (handle-object handle)))
(defun handle-transform (object)
  (make-handle object))

(defun make-world-hash-table (size)
  (make-array (next-prime size) :initial-element nil))

(defstruct (world-hash
             (:constructor make-world-hash
                           (cell-size size &aux (table (make-world-hash-table size)))))
  "The spatial hash is SquirL's default (and currently only) spatial index"
  (cell-size (assert nil) :type double-float) ; Size of the hash's cells
  (handle-set (make-hash-set 0 #'handle-equal) :type hash-set) ; `hash-set' of all handles
  (table (assert nil) :type simple-vector) ; Bins in use
  ;;  (junk nil)                                    ; The "recycle bin"
  (stamp 1 :type fixnum) ; Incremented on each query; see `handle-stamp'
  )

(define-print-object (world-hash))

(defun stamp-handle (handle hash)
  (setf (cdr handle) (world-hash-stamp hash)))

(defun world-hash-size (hash)
  (length (world-hash-table hash)))

(defun world-hash-chain (hash index)
  (aref (world-hash-table hash) index))
(defun (setf world-hash-chain) (new-chain hash index)
  (setf (aref (world-hash-table hash) index) new-chain))

(defun clear-hash-cell (hash index)
  "Releases the handles under INDEX in `world-hash' HASH, and links the
list structure into the `world-hash-junk'."
  (setf (world-hash-chain hash index) nil)
  #+ (or) (do* ((chain (world-hash-chain hash index) next)
        ;; We need to hang onto the CDR because we 'recycle' NODE
        (next (cdr chain) (cdr chain)))
       ((null chain) (setf (world-hash-chain hash index) nil))
    (push-cons chain (world-hash-junk hash))))

(defun clear-world-hash (hash)
  "Clear all cells in the `world-hash' HASH"
  (declare (optimize speed (safety 0)))
  (let ((table (world-hash-table hash)))
    (declare (simple-vector table))
    (dotimes (index (the fixnum (length table)))
      (setf (svref table index) nil))))

(defun resize-world-hash (hash new-cell-size new-size)
  "Resize `world-hash' HASH to the specified dimensions"
  (clear-world-hash hash)
  (setf (world-hash-cell-size hash) new-cell-size
        (world-hash-table hash) (make-world-hash-table new-size)))

;; (defun get-new-node (hash)
;;   "Get a recycled node or cons a new one"
;;   (let ((node (pop (world-hash-junk hash))))
;;     (if (null node) (cons nil nil) node)))

;; (defmacro push-handle (handle hash chain)
;;   (with-gensyms (node)
;;     `(let ((,node (get-new-node ,hash)))
;;        (setf (car ,node) ,handle)
;;        (push-cons ,node ,chain))))

(declaim (ftype (function (fixnum fixnum fixnum) fixnum) hash))
(defun hash (x y n)
  "Hash X, Y, and N to generate a hash code"
  (mod (the fixnum
         (logand (logxor (* x 2185031351) (* y 4232417593))
                 most-positive-fixnum))
       n))

(defmacro do-bbox ((chain-macro hash-form bbox-form) &body body)
  (with-gensyms (hash bbox size dim bb.l bb.r bb.b bb.t i j index)
    `(let* ((,hash ,hash-form)
            (,bbox ,bbox-form)
            (,size (world-hash-size ,hash))
            (,dim (world-hash-cell-size ,hash))
            (,bb.t (floor (/ (bbox-top    ,bbox) ,dim)))
            (,bb.l (floor (/ (bbox-left   ,bbox) ,dim)))
            (,bb.r (floor (/ (bbox-right  ,bbox) ,dim)))
            (,bb.b (floor (/ (bbox-bottom ,bbox) ,dim))))
       (symbol-macrolet ((,chain-macro (world-hash-chain ,hash ,index)))
         (loop for ,i from ,bb.l to ,bb.r
            do (loop for ,j from ,bb.b to ,bb.t
                  for ,index = (hash ,i ,j ,size) ,@body))))))

(defun hash-handle (hash handle bbox)
  (do-bbox (chain hash bbox)
    unless (find handle chain) do
      (push handle chain)))

(defun world-hash-insert (hash object id bbox)
  (with-accessors ((handle-set world-hash-handle-set)) hash
    (let ((handle (make-handle object)))
      (hash-set-insert handle-set id handle)
      (hash-handle hash handle bbox)
      object)))

(defun world-hash-rehash-object (hash object id)
  (with-accessors ((handle-set world-hash-handle-set)) hash
    (hash-handle hash (hash-set-find handle-set id object)
                 (shape-bbox object))))

(defun rehash-world-hash (hash)
  (clear-world-hash hash)
  (map-hash-set (fun (hash-handle hash _ (shape-bbox (handle-object _))))
                (world-hash-handle-set hash)))

(defun world-hash-remove (hash object id)
  (multiple-value-bind (handle foundp)
      (hash-set-remove (world-hash-handle-set hash) id object)
    (when foundp (handle-object handle))))

(defun map-world-hash (function hash)
  (map-hash-set (fun (funcall function (handle-object _)))
                (world-hash-handle-set hash)))

(defun map-world-hash-chain (function hash chain object)
  (loop for handle in chain
     unless (or (= (handle-stamp handle) (world-hash-stamp hash))
                (eq object (handle-object handle))
                (null (handle-object handle))) do
       (funcall function object (handle-object handle))
       (stamp-handle handle hash)))

(defun world-hash-point-query (function hash point)
  (prog1 (let* ((dim (world-hash-cell-size hash))
                (idx (with-vec (pt point)
                       (hash (floor (/ pt.x dim)) (floor (/ pt.y dim))
                             (world-hash-size hash)))))
           (map-world-hash-chain function hash (world-hash-chain hash idx) point))
    (incf (world-hash-stamp hash))))

(defun world-hash-query (function hash object bbox)
  (do-bbox (chain hash bbox)
    do (map-world-hash-chain function hash chain object))
  (incf (world-hash-stamp hash)))

(defun world-hash-query-rehash (function hash)
  (clear-world-hash hash)
  (map-hash-set (fun
                  (let* ((object (handle-object _)))
                    (do-bbox (chain-form hash (shape-bbox object))
                      for chain = chain-form
                      unless (find _ chain) do
                        (map-world-hash-chain function hash chain object)
                        (push _ chain-form)))
                  (incf (world-hash-stamp hash)))
                (world-hash-handle-set hash)))

(defun query-segment (function hash chain)
  (dolist (handle chain 1.0)
    (unless (or (= (handle-stamp handle) (world-hash-stamp hash))
                (null (handle-object handle)))
      (stamp-handle handle hash)
      (return (funcall function (handle-object handle))))))

(defun world-hash-query-segment (function hash vec-a vec-b)
  (with-accessors ((cell-size world-hash-cell-size)) hash
    (with-vecs ((a (vec* vec-a (/ cell-size)))
                (b (vec* vec-b (/ cell-size))))
      (let ((dt/dx (/ (abs (- b.x a.x))))
            (dt/dy (/ (abs (- b.y a.y))))
            (cell-x (floor a.x))
            (cell-y (floor a.y))
            (ratio 0) (exit-ratio 1)
            x-inc y-inc next-v next-h)
        (if (> b.x a.x)
            (setf x-inc  1 next-h (* (- (floor (1+ a.x)) a.x) dt/dx))
            (setf x-inc -1 next-h (* (- a.x      (floor a.x)) dt/dx)))
        (if (> b.y a.y)
            (setf y-inc  1 next-v (* (- (floor (1+ a.x)) a.x) dt/dy))
            (setf y-inc -1 next-v (* (- a.y      (floor a.y)) dt/dy)))
        (let ((cell-size (world-hash-size hash)))
          (loop while (< ratio exit-ratio) for index = (hash cell-x cell-y cell-size)
             for new-ratio = (query-segment function hash (world-hash-chain hash index))
             do (setf exit-ratio (min exit-ratio new-ratio))
                (if (< next-v next-h) ; Note indentation
                    (progn
                      (incf cell-y y-inc)
                      (setf ratio next-v)
                      (incf next-v dt/dy))
                    (progn
                      (incf cell-x x-inc)
                      (setf ratio next-h)
                      (incf next-h dt/dx)))))
        (incf (world-hash-stamp hash))))))
