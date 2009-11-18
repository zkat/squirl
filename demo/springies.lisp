(in-package :squirl-demo)

(defclass springies-demo (demo)
  ((static-body :initarg :box :accessor static-body))
  (:default-initargs :name "Sproing twang!"))

