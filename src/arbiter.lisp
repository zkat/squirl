;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(define-constant +bias-coefficient+ 0.1
  "Determines how fast penetrations resolve themselves.")
(define-constant +collision-slop+ 0.1
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

(defun arbiter-apply-impulse (arbiter e-coefficient)
  (let ((body-a (shape-body (arbiter-shape-a arbiter)))
        (body-b (shape-body (arbiter-shape-b arbiter))))
    (dolist (contact (arbiter-contacts arbiter))
      (let* ((n (contact-normal contact))
             (r1 (contact-r1 contact))
             (r2 (contact-r2 contact))
             ;; Relative bias velocities
             (vb1 (vec+ (body-velocity-bias body-a)
                        (vec* (vec-perp r1)
                              (body-angular-velocity-bias body-a))))
             (vb2 (vec+ (body-velocity-bias body-b)
                        (vec* (vec-perp r2)
                              (body-angular-velocity-bias body-b))))
             (vbn (vec. (vec- vb2 vb1) n)))
        ;; Calculate and clamp bias impulse
        (let ((jbn (* (- (contact-bias contact) vbn)
                      (contact-normal-mass contact)))
              (jbn-old (contact-j-bias contact)))
          (setf (contact-j-bias contact) (max 0 (+ jbn-old jbn))
                jbn (- (contact-j-bias contact) jbn-old))
          ;; Apply bias impulse
          ;; TODO: Use apply-bias-impulses from constraints/util
          (let ((impulse (vec* n jbn)))
            (body-apply-bias-impulse body-a (vec- impulse) r1)
            (body-apply-bias-impulse body-b impulse r2)))
        ;; Calculate relative velocity
        ;; TODO: relative-velocity from constraints/util
        (let* ((vr (vec- (vec+ (body-velocity body-b)
                               (vec* (vec-perp r2)
                                     (body-angular-velocity body-b)))
                         (vec+ (body-velocity body-a)
                               (vec* (vec-perp r1)
                                     (body-angular-velocity body-a)))))
               (vrn (vec. vr n)))
          ;; Calculate and clamp the normal impulse
          (let ((jn (* (- (+ (* (contact-bounce contact)
                                e-coefficient)
                             vrn))
                       (contact-normal-mass contact)))
                (jn-old (contact-jn-acc contact)))
            (setf (contact-jn-acc contact) (max 0 (+ jn-old jn))
                  jn (- (contact-jn-acc contact) jn-old))
            (let*
                ;; Calculate relative tangent velocity
                ((vrt (vec. (vec+ vr (arbiter-target-v arbiter))
                            (vec-perp n)))
                 ;; Calculate and clamp friction impulse
                 (jt-max (* (arbiter-u arbiter)
                           (contact-jn-acc contact)))
                 (jt (* (- vrt) (contact-tangent-mass contact)))
                 (jt-old (contact-jt-acc contact)))
              (setf (contact-jt-acc contact) (clamp (+ jt-old jt)
                                                    (- jt-max)
                                                    jt-max)
                    jt (- (contact-jt-acc contact) jt-old))
              (let ((impulse (vec-rotate n (vec jn jt))))
                ;; TODO: apply-impulse from constraints/util
                (body-apply-impulse body-a (vec- impulse) r1)
                (body-apply-impulse body-b impulse r2)))))))))
