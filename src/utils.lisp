;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(locally (declare (optimize speed))

  (declaim (inline make-adjustable-vector clamp ensure-list)
           (ftype (function (fixnum) vector) make-adjustable-vector)
           (ftype (function (double-float double-float double-float) double-float))
           (ftype (function (t) list) ensure-list)
           (ftype (function (integer integer fixnum) fixnum) expt-mod))
  (defun make-adjustable-vector (length)
    (make-array length :adjustable t :fill-pointer 0))

  (defun ensure-list (x) (if (listp x) x (list x)))

  (defun clamp (n min max)
    (declare (double-float n min max))
    (min (max n min) max))

  (defun expt-mod (b e m &aux (result 1))
    (declare (fixnum b e m result) (optimize (safety 0)))
    (do ((expt e (ash expt -1))
         (base b (mod (* base base) m)))
        ((zerop expt) result)
      (declare (fixnum base expt))
      (when (oddp expt)
        (setf result (mod (* result base) m)))))

)                                     ; LOCALLY

(declaim (inline maybe/)
         (ftype (function (double-float double-float) double-float) maybe/))
(defun maybe/ (a b)
  ;; Don't declare me (optimize speed), because that chokes SBCL
  (if (zerop b) 0d0 (/ a b)))

(defmacro fun (&body body)
  `(lambda (&optional _) (declare (ignorable _)) ,@body))

;; from alexandria:
(declaim (inline delete/swapped-arguments delete-if/swapped-arguments))
(defun delete/swapped-arguments (sequence item &rest keyword-arguments)
  (apply #'delete item sequence keyword-arguments))
(defun delete-if/swapped-arguments (sequence predicate &rest keyword-arguments)
  (apply #'delete-if predicate sequence keyword-arguments))

(define-modify-macro deletef (item &rest remove-keywords)
  delete/swapped-arguments
  "Modify-macro for DELETE. Sets place designated by the first argument to
the result of calling DELETE with ITEM, place, and the REMOVE-KEYWORDS.")

(define-modify-macro delete-iff (predicate &rest remove-keywords)
  delete-if/swapped-arguments
  "Modify-macro for DELETE-IF. Sets place designated by the first argument to
the result of calling DELETE with PREDICATE, place, and the REMOVE-KEYWORDS.")

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect `(,var (gensym ,(symbol-name var))))
     ,@body))

(defmacro push-cons (cons place)
  "Like `cl:push', but reuses CONS"
  (with-gensyms (cons-sym)
    `(let ((,cons-sym ,cons))
       (setf (cdr ,cons-sym) ,place
             ,place ,cons-sym))))

(defmacro define-constant (name value &optional doc)
  "ANSI-compliant replacement for `defconstant'. cf SBCL Manual 2.3.4."
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro define-print-object ((class &key (identity t) (type t)) &body body)
  "Defines a `print-object' method on class CLASS, using the standard macro
`print-unreadable-object'. The IDENTITY and TYPE keyword arguments are passed
through to `print-unreadable-object', although they default to T if not supplied.

CLASS can be a list of the form (VARIABLE CLASS-NAME), in which case
the `print-object' method will be specialized on class CLASS-NAME and VARIABLE
will be used as the parameter name. Alternatively, as shorthand, CLASS can be a
single symbol, which will be used for both the variable and the class name."
  (let ((object (if (listp class) (car class) class))
        (class-name (if (listp class) (cadr class) class)))
    (with-gensyms (stream)
      `(defmethod print-object ((,object ,class-name) ,stream)
         (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
           (let ((*standard-output* ,stream)) ,@body))))))

(defmacro do-vector ((var vector-form &optional result) &body body)
  "See `dolist'. If VAR is a list of the form (INDEX VAR), then INDEX is used
as the index vector. Note that this macro doesn't handle declarations properly."
  (let ((var-name (if (listp var) (cadr var) var))
        (idx-name (if (listp var) (car var) (gensym "INDEX"))))
    (with-gensyms (vector)
      `(let ((,vector ,vector-form) ,var-name)
         (declare (ignorable ,var-name) (vector ,vector))
         (dotimes (,idx-name (length ,vector) ,result)
           (let ((,var-name (aref ,vector ,idx-name))) ,@body))))))

(defmacro with-place (conc-name (&rest slots) form &body body)
  (flet ((conc (a b) (intern (format nil "~A~A" a b))))
    (let ((sm-prefix (if (atom conc-name) conc-name (first conc-name)))
          (acc-prefix (if (atom conc-name) conc-name (second conc-name))))
      `(with-accessors
             ,(mapcar (fun `(,(conc sm-prefix (if (atom _) _ (car _)))
                              ,(conc acc-prefix (if (atom _) _ (cadr _)))))
                      slots)
           ,form
         ,@body))))

(defmacro aprog1 (result &body body)
  `(let ((it ,result)) ,@body it))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))
