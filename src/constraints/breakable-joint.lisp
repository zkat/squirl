;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (breakable-joint (:include constraint))
  delegate space last-dt-inverse)

(defmethod pre-step ((joint breakable-joint) dt dt-inverse)
  (let ((delegate (breakable-joint-delegate joint)))
    (if (>= (* (get-impulse delegate) (breakable-joint-last-dt-inverse joint))
            (breakable-joint-max-force joint))
        ;; remove the breakable joint from the space...
        (space-remove-constraint (breakable-joint-space joint) joint)
        ;; otherwise, call pre-step on its delegate.
        (prog1 (pre-step delegate dt dt-inverse)
          (setf (breakable-joint-last-dt-inverse joint) dt-inverse)))))

(defmethod apply-impulse ((joint breakable-joint))
  (apply-impule (breakable-joint-delegate joint)))

(defmethod get-impulse ((joint breakable-joint))
  (get-impulse (breakable-joint-delegate joint)))
