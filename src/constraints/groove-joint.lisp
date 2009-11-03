;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (groove-joint (:include constraint)
                         (:constructor
                          make-groove-joint
                          (body-a body-b groove-a groove-b anchor2
                                  &aux (groove-normal
                                        (vec-perp (vec-normalize (vec- groove-b groove-a)))))))
  groove-normal groove-a groove-b anchor2
  groove-transformed-normal clamp
  r1 r2 k1 k2 (j-acc +zero-vector+) j-max-length bias)

(defmethod pre-step ((joint groove-joint) dt dt-inverse)
  (let* ((body-a (constraint-body-a joint))
         (body-b (constraint-body-b joint))
         (trans-a (body-local->world body-a (groove-joint-groove-a joint)))
         ;; The C source for this file uses body "a" for this one. Should it be body "b"?
         (trans-b (body-local->world body-a (groove-joint-groove-b joint)))
         ;; Calculate axis
         (normal (vec-rotate (groove-joint-groove-normal joint)
                             (body-rotation body-a)))
         (d (vec. trans-a normal))) ; distance? dot product? What?
    (setf (groove-joint-transformed-normal joint) normal
          (groove-joint-r2 joint) (vec-rotate (groove-joint-anchor2 joint)
                                              (body-rotation body-b)))
    ;; calculate tangential distance along the axis of r2
    (let ((td (vecx (vec+ (body-position bodyb) (groove-joint-r2 joint)) normal)))
      ;; Calculate the clamping factor and r2
      (cond ((<= td (vecx trans-a normal))
             (setf (groove-joint-clamp joint) 1.0
                   (groove-joint-r1 joint) (vec- trans-a (body-position body-a))))
            ((>= td (vecx trans-b normal))
             (setf (groove-joint-clamp joint) -1.0
                   (groove-joint-r1 joint) (vec- trans-b (body-position body-a))))
            (t
             (setf (groove-joint-clamp joint) 0.0
                   (groove-joint-r1 joint) (vec- (vec+ (vec* (vec-perp normal) (- td))
                                                       (vec* normal d))
                                                 (body-position body-a))))))
    ;; calculate the mass tensor
    (multiple-value-bind (k1-val k2-val)
        (k-tensor body-a body-b (groove-joint-r1 joint) (groove-joint-r2 joint))
      (setf (groove-joint-k1 joint) k1-val
            (groove-joint-k2 joint) k2-val))

    ;; compute max impulse
    (setf (joint-max-length joint) (impulse-max joint dt))

    ;; Calculate bias velocity
    (let ((delta (vec- (vec+ (body-position body-b) (groove-joint-r2 joint))
                       (vec+ (body-position body-a) (groove-joint-r1 joint)))))
      (setf (groove-joint-bias joint) (vec-clamp (vec* delta
                                                       (- (* (groove-joint-bias-coefficient joint)
                                                             dt-inverse)))
                                                 (groove-joint-max-bias joint))))

    ;; Apply accumulated impulse
    (apply-impulses body-a body-b
                    (groove-joint-r1 joint) (groove-joint-r2 joint)
                    (groove-joint-j-acc joint))))

(defun constrain-groove (joint j)
  (let* ((normal (groove-joint-groove-transformed-normal joint))
         (j-clamp (if (plusp (* (groove-joint-clamp joint) (vecx j normal)))
                      j (vec-project j normal))))
    (vec-clamp j-clamp (groove-joint-j-max-length joint))))

(defmethod apply-impulse ((joint groove-joint))
  (let* ((body-a (groove-joint-body-a joint))
         (body-b (groove-joint-body-b joint))
         (r1 (groove-joint-r1 joint))
         (r2 (groove-joint-r2 joint))
         ;; Compute impulse...
         (relative-velocity (relative-velocity body-a body-b r1 r2))
         (j (mult-k (vec- (groove-joint-bias joint) relative-velocity)
                    (groove-joint-k1 joint) (groove-joint-k2 joint)))
         (j-old (groove-joint-j-acc joint)))
    (setf (groove-joint-j-acc joint) (constrain-groove joint (vec+ j-old j)))
    ;; Apply impulses
    (apply-impulses body-a body-b (groove-joint-r1 joint) (groove-joint-r2 joint)
                    (vec- (groove-joint-j-acc joint) j-old))))

