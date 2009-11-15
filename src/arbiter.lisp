;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(define-constant +bias-coefficient+ 0.1d0
  "Determines how fast penetrations resolve themselves.")

(defstruct (contact (:constructor make-contact (point normal distance &optional hash)))
  ;; Contact point and normal
  (point +zero-vector+ :type vec)
  (normal +zero-vector+ :type vec)
  ;; Penetration distance
  (distance 0d0 :type double-float)
  ;; Calculated by arbiter-prestep
  (r1 +zero-vector+ :type vec)
  (r2 +zero-vector+ :type vec)
  (normal-mass 0d0 :type double-float)
  (tangent-mass 0d0 :type double-float)
  (bounce 0d0 :type double-float)
  ;; Persistant contact information
  (jn-acc 0d0 :type double-float)
  (jt-acc 0d0 :type double-float)
  (j-bias 0d0 :type double-float)
  (bias 0d0 :type double-float)
  ;; Hash value used as a (mostly) unique ID
  (hash 0 :type fixnum))

(defun contacts-sum-impulses (&rest contacts)
  (reduce #'vec+ contacts :initial-value +zero-vector+
          :key (fun (vec* (contact-normal _)
                          (contact-jn-acc _)))))

(defun contact-impulse-with-friction (contact)
  (vec-rotate (contact-normal contact)
              (vec (contact-jn-acc contact)
                   (contact-jt-acc contact))))

(defun contacts-sum-impulses-with-friction (&rest contacts)
  (reduce #'vec+ contacts :initial-value +zero-vector+
          :key #'contact-impulse-with-friction))

(defun contacts-estimate-crushing-impulse (&rest contacts)
  (loop for contact in contacts
     for impulse = (contact-impulse-with-friction contact)
     for vec-sum = +zero-vector+ then (vec+ vec-sum impulse)
     sum (vec-length impulse) into scalar-sum
     finally (return (- 1 (/ (vec-length vec-sum) scalar-sum)))))

(defstruct (arbiter (:constructor make-arbiter (contacts shape-a shape-b stamp)))
  ;; Information on the contact points between the objects
  (contacts (assert nil) :type list)
  ;; The two shapes involved in the collision
  (shape-a (assert nil) :type shape)
  (shape-b (assert nil) :type shape)
  ;; Calculated by arbiter-prestep
  (u 0d0 :type double-float)
  (target-v +zero-vector+ :type vec)
  ;; Timestamp of the arbiter (from world)
  (stamp (assert nil) :type fixnum))

(defun arbiter-shapes-equal (arbiter1 arbiter2)
  (or (and (eq (arbiter-shape-a arbiter1) (arbiter-shape-a arbiter2))
           (eq (arbiter-shape-b arbiter1) (arbiter-shape-b arbiter2)))
      (and (eq (arbiter-shape-b arbiter1) (arbiter-shape-b arbiter2))
           (eq (arbiter-shape-a arbiter1) (arbiter-shape-a arbiter2)))))

(defun arbiter-has-shapes-p (arbiter shape1 shape2)
  (or (and (eq shape1 (arbiter-shape-a arbiter))
           (eq shape2 (arbiter-shape-b arbiter)))
      (and (eq shape2 (arbiter-shape-a arbiter))
           (eq shape1 (arbiter-shape-b arbiter)))))

(defun arbiter-inject (arbiter contacts)
  "Replaces ARBITER's contacts with the supplied set, saving state for persistent contacts."
  (dolist (old-contact (arbiter-contacts arbiter))
    (dolist (new-contact contacts)
      (when (= (contact-hash new-contact)
               (contact-hash old-contact))
        (setf (contact-jn-acc new-contact) (contact-jn-acc old-contact)
              (contact-jt-acc new-contact) (contact-jt-acc old-contact)))))
  (setf (arbiter-contacts arbiter) contacts)
  arbiter)

(defun arbiter-prestep (arbiter dt-inverse)
  (declare (optimize speed) (double-float dt-inverse))
  (let* ((shape-a (arbiter-shape-a arbiter))
         (shape-b (arbiter-shape-b arbiter))
         (body-a (shape-body shape-a))
         (body-b (shape-body shape-b)))
    (setf (arbiter-u arbiter) (* (shape-friction shape-a)
                                 (shape-friction shape-b))
          (arbiter-target-v arbiter) (vec- (shape-surface-velocity shape-b)
                                           (shape-surface-velocity shape-a)))
    (dolist (contact (arbiter-contacts arbiter))
      (setf (contact-r1 contact)
            (vec- (contact-point contact)
                  (body-position body-a))
            (contact-r2 contact)
            (vec- (contact-point contact)
                  (body-position body-b))
            (contact-normal-mass contact)
            (/ (k-scalar body-a body-b
                         (contact-r1 contact)
                         (contact-r2 contact)
                         (contact-normal contact)))
            (contact-tangent-mass contact)
            (/ (k-scalar body-a body-b
                         (contact-r1 contact)
                         (contact-r2 contact)
                         (vec-perp (contact-normal contact))))
            (contact-bias contact)
            (* (- +bias-coefficient+)
               dt-inverse
               (min 0d0 (+ (contact-distance contact)
                             +collision-slop+)))
            (contact-j-bias contact)
            0d0
            (contact-bounce contact)
            (* (shape-restitution shape-a)
               (shape-restitution shape-b)
               (normal-relative-velocity body-a body-b
                                         (contact-r1 contact)
                                         (contact-r2 contact)
                                         (contact-normal contact)))))))

(defun arbiter-apply-cached-impulse (arbiter)
  (let ((shape-a (arbiter-shape-a arbiter))
        (shape-b (arbiter-shape-b arbiter)))
    (setf (arbiter-u arbiter) (* (shape-friction shape-a)
                                 (shape-friction shape-b))
          (arbiter-target-v arbiter) (vec- (shape-surface-velocity shape-b)
                                           (shape-surface-velocity shape-a)))
    (let ((body-a (shape-body shape-a))
          (body-b (shape-body shape-b)))
      (dolist (contact (arbiter-contacts arbiter))
        (apply-impulses body-a body-b
                        (contact-r1 contact)
                        (contact-r2 contact)
                        (vec-rotate (contact-normal contact)
                                    (vec (contact-jn-acc contact)
                                         (contact-jt-acc contact))))))))

(declaim (ftype (function (arbiter boolean)) arbiter-apply-impulse))
(defun arbiter-apply-impulse (arbiter elasticp)
  (declare (arbiter arbiter) (optimize speed))
  (let* ((e-coefficient (if elasticp 1d0 0d0))
         (body-a (shape-body (arbiter-shape-a arbiter)))
         (body-b (shape-body (arbiter-shape-b arbiter)))
         (vb-a (body-velocity-bias body-a))
         (vb-b (body-velocity-bias body-b))
         (avb-a (body-angular-velocity-bias body-a))
         (avb-b (body-angular-velocity-bias body-b)))
    (dolist (contact (arbiter-contacts arbiter))
      (flet ((relative-bias-velocity (vb r avb)
               (vec+ vb (vec* (vec-perp r) avb))))
        (let* ((n (contact-normal contact))
               (r1 (contact-r1 contact))
               (r2 (contact-r2 contact))
               ;; Relative bias velocities
               (vb1 (relative-bias-velocity vb-a r1 avb-a))
               (vb2 (relative-bias-velocity vb-b r2 avb-b))
               (vbn (vec. (vec- vb2 vb1) n)))
          ;; Calculate and clamp bias impulse
          (let ((jbn (* (- (contact-bias contact) vbn)
                        (contact-normal-mass contact)))
                (jbn-old (contact-j-bias contact)))
            (setf (contact-j-bias contact) (max 0d0 (+ jbn-old jbn))
                  jbn (- (contact-j-bias contact) jbn-old))
            ;; Apply bias impulse
            (body-apply-bias-impulse body-a (vec* n (- jbn)) r1)
            (body-apply-bias-impulse body-b (vec* n jbn) r2))
          ;; Calculate relative velocity
          (let* ((relative-velocity (relative-velocity body-a body-b r1 r2))
                 (n-relative-velocity (vec. relative-velocity n)))
            (flet ((calculate-normal-impulse (contact e-coef nrv)
                     (* (- (+ (* (contact-bounce contact) e-coef) nrv))
                        (contact-normal-mass contact))))
              (let ((jn (calculate-normal-impulse contact e-coefficient n-relative-velocity))
                    (jn-old (contact-jn-acc contact)))
                (setf (contact-jn-acc contact) (max 0d0 (+ jn-old jn))
                      jn (- (contact-jn-acc contact) jn-old))
                (let* ((relative-tangent-velocity (vec. (vec+ relative-velocity
                                                              (arbiter-target-v arbiter))
                                                        (vec-perp n)))
                       ;; Calculate and clamp friction impulse
                       (jt-max (* (arbiter-u arbiter)
                                  (contact-jn-acc contact)))
                       (jt (* (- relative-tangent-velocity) (contact-tangent-mass contact)))
                       (jt-old (contact-jt-acc contact)))
                  (setf (contact-jt-acc contact) (clamp (+ jt-old jt) (- jt-max) jt-max)
                        jt (- (contact-jt-acc contact) jt-old))
                  (apply-impulses body-a body-b r1 r2
                                  (vec-rotate n (vec jn jt))))))))))))
