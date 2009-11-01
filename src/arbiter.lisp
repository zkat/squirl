;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defconstant +bias-coefficient+ 0.1
  "Determines how fast penetrations resolve themselves.")
(defconstant +collision-slop+ 0.1
  "Amount of allowed penetration.  Used to reduce vibrating contacts.")

(defstruct (contact (:constructor make-contact (point normal distance hash)))
  ;; Contact point and normal
  point normal
  ;; Penetration distance
  distance
  ;; Calculated by arbiter-prestep
  r1 r2
  normal-mass tangent-mass bounce
  ;; Persistant contact information
  (jn-acc 0) (jt-acc 0) (j-bias 0)
  bias
  ;; Hash value used as a (mostly) unique ID
  hash)

(defun contacts-sum-impulses (&rest contacts)
  (reduce (lambda (sum contact)
            (vec+ sum (vec* (contact-normal contact)
                            (contact-jn-acc contact))))
          contacts :initial-value +zero-vector+))

(defun contacts-sum-impulses-with-friction (&rest contacts)
  (reduce (lambda (sum contact)
            (vec+ sum
                  (vec* (vec-perp (contact-normal contact))
                        (contact-jt-acc contact))
                  (vec* (contact-normal contact)
                        (contact-jn-acc contact))))
          contacts :initial-value +zero-vector+))

(defstruct (arbiter (:constructor make-arbiter (contacts shape-a shape-b stamp)))
  ;; Information on the contact points between the objects
  contacts
  ;; The two shapes involved in the collision
  shape-a shape-b
  ;; Calculated by arbiter-prestep
  u target-v
  ;; Timestamp of the arbiter (from space)
  stamp)

(defun arbiter-inject (arbiter &rest contacts)
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
  (let* ((shape-a (arbiter-shape-a arbiter))
         (shape-b (arbiter-shape-b arbiter))
         (body-a (shape-body shape-a))
         (body-b (shape-body shape-b))
         (mass-sum (+ (body-inverse-mass body-a)
                      (body-inverse-mass body-b))))
    (setf (arbiter-u arbiter) (* (shape-u shape-a)
                                 (shape-u shape-b))
          (arbiter-target-v arbiter) (vec- (shape-surface-v shape-b)
                                           (shape-surface-v shape-a)))
    (mapc (lambda (contact)
            (setf (contact-r1 contact)
                  (vec- (contact-point contact)
                        (body-position body-a))
                  (contact-r2 contact)
                  (vec- (contact-point contact)
                        (body-position body-b))
                  (contact-normal-mass contact)
                  (/ (+ mass-sum
                        (* (body-inverse-inertia body-a)
                           (expt (vecx (contact-r1 contact)
                                       (contact-normal contact))
                                 2))
                        (* (body-inverse-inertia body-b)
                           (expt (vecx (contact-r2 contact)
                                       (contact-normal contact))
                                 2))))
                  (contact-tangent-mass contact)
                  (let ((tangent (vec-perp (contact-normal contact))))
                    (/ (+ mass-sum
                          (* (body-inverse-inertia body-a)
                             (expt (vecx (contact-r1 contact)
                                         tangent)
                                   2))
                          (* (body-inverse-inertia body-b)
                             (expt (vecx (contact-r2 contact)
                                         tangent)
                                   2)))))
                  (contact-bias contact)
                  (* (- +bias-coefficient+)
                     dt-inverse
                     (min 0 (+ (contact-distance contact)
                               +collision-slop+)))
                  (contact-j-bias contact)
                  0
                  (contact-bounce contact)
                  (* (shape-elasticity shape-a)
                     (shape-elasticity shape-b)
                     (vec. (contact-normal contact)
                           (vec- (vec+ (body-velocity body-b)
                                       (vec* (vec-perp (contact-r2 contact))
                                             (body-angular-velocity body-b)))
                                 (vec+ (body-velocity body-a)
                                       (vec* (vec-perp (contact-r1 contact))
                                             (body-angular-velocity body-a))))))))
          (arbiter-contacts arbiter))))

(defun arbiter-apply-cached-impulse (arbiter)
  (let ((shape-a (arbiter-shape-a arbiter))
        (shape-b (arbiter-shape-b arbiter)))
    (setf (arbiter-u arbiter) (* (shape-u shape-a)
                                 (shape-u shape-b))
          (arbiter-target-v arbiter) (vec- (shape-surface-v shape-b)
                                           (shape-surface-v shape-a)))
    (let ((body-a (shape-body shape-a))
          (body-b (shape-body shape-b)))
      (dolist (contact (arbiter-contacts arbiter))
        (let ((j (vec+ (vec* (contact-normal contact)
                             (contact-jn-acc contact))
                       (vec* (vec-perp (contact-normal contact))
                             (contact-jt-acc contact)))))
          (body-apply-impulse body-a (vec- j) (contact-r1 contact))
          (body-apply-impulse body-b j (contact-r2 contact)))))))
