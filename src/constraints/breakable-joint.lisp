;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

;; What we have here is a textbook case of a proxy pattern.
;; Instead of creating an is-a relationship (which in this case
;; would involve multiple inheritance), we instead wrap any given
;; joint with this breakable one, which adds the breaking behavior.
;; As such, we forward most of the behavior over to the delegate, but
;; we pass -this- object around.
(defstruct (breakable-joint (:include constraint)
                            (:constructor make-breakable-joint (delegate space)))
  delegate space (last-dt-inverse 0d0))

(defmethod pre-step ((joint breakable-joint) dt dt-inverse)
  (let ((delegate (breakable-joint-delegate joint)))
    (if (>= (* (get-impulse delegate) (breakable-joint-last-dt-inverse joint))
            (breakable-joint-max-force joint))
        ;; remove the breakable joint from the space...
        (world-remove-constraint (breakable-joint-space joint) joint)
        ;; otherwise, call pre-step on its delegate.
        (prog1 (pre-step delegate dt dt-inverse)
          (setf (breakable-joint-last-dt-inverse joint) dt-inverse)))))

(defmethod apply-impulse ((joint breakable-joint))
  (apply-impulse (breakable-joint-delegate joint)))

(defmethod get-impulse ((joint breakable-joint))
  (get-impulse (breakable-joint-delegate joint)))
