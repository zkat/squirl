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
  (reduce #'vec+ contacts :initial-value +zero-vector+
          :key (lambda (contact)
                 (vec* (contact-normal contact)
                       (contact-jn-acc contact)))))

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
  contacts
  ;; The two shapes involved in the collision
  shape-a shape-b
  ;; Calculated by arbiter-prestep
  u target-v
  ;; Timestamp of the arbiter (from world)
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
         (body-b (shape-body shape-b)))
    (setf (arbiter-u arbiter) (* (shape-u shape-a)
                                 (shape-u shape-b))
          (arbiter-target-v arbiter) (vec- (shape-surface-v shape-b)
                                           (shape-surface-v shape-a)))
    ;; TODO: Move these functions into constraints/util
    (flet ((k-scalar (body-a body-b r1 r2 normal)
             (let ((r1cn (vecx r1 normal))
                   (r2cn (vecx r2 normal)))
               (+ (body-inverse-mass body-a)
                  (body-inverse-mass body-b)
                  (* (body-inverse-inertia body-a)
                     (expt r1cn 2))
                  (* (body-inverse-inertia body-b)
                     (expt r2cn 2)))))
           (normal-relative-velocity (body-a body-b r1 r2 normal)
             (vec. (vec- (vec+ (body-velocity body-b)
                               (vec* (vec-perp r2)
                                     (body-angular-velocity body-b)))
                         (vec+ (body-velocity body-a)
                               (vec* (vec-perp r1)
                                     (body-angular-velocity body-a))))
                   normal)))
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
                 (min 0 (+ (contact-distance contact)
                           +collision-slop+)))
              (contact-j-bias contact)
              0
              (contact-bounce contact)
              (* (shape-elasticity shape-a)
                 (shape-elasticity shape-b)
                 (normal-relative-velocity body-a body-b
                                           (contact-r1 contact)
                                           (contact-r2 contact)
                                           (contact-normal contact))))))))

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
        ;; TODO: Use apply-impulses from constraints/util
        (let ((impulse (vec-rotate (contact-normal contact)
                                   (vec (contact-jn-acc contact)
                                        (contact-jt-acc contact)))))
          (body-apply-impulse body-a (vec- impulse) (contact-r1 contact))
          (body-apply-impulse body-b impulse (contact-r2 contact)))))))
