;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (damped-spring (:include spring)
                          (:constructor
                           make-damped-spring
                           (body-a body-b anchor1 anchor2 rest-length stiffness damping)))
  anchor1 anchor2 rest-length damping target-vrn r1 r2 n-mass normal)

(defmethod spring-force ((spring damped-spring) distance)
  (* (- (damped-spring-rest-length spring) distance)
     (spring-stiffness spring)))

(defmethod pre-step ((spring damped-spring) dt dt-inverse)
  (declare (ignore dt-inverse))
  (let ((body-a (spring-body-a spring))
        (body-b (spring-body-b spring)))
    (setf (damped-spring-r1 spring) (vec-rotate (damped-spring-anchor1 spring)
                                                (body-rotation body-a))
          (damped-spring-r2 spring) (vec-rotate (damped-spring-anchor2 spring)
                                                (body-rotation body-b)))
    (let* ((delta (vec- (vec+ (body-position body-b) (damped-spring-r2 spring))
                        (vec+ (body-position body-a) (damped-spring-r1 spring))))
           (distance (vec-length delta)))
      (setf (damped-spring-normal spring) (vec* delta (maybe/ 1 distance))
            ;; calculate mass normal
            (damped-spring-n-mass spring) (/ (k-scalar body-a body-b
                                                       (damped-spring-r1 spring)
                                                       (damped-spring-r2 spring)
                                                       (damped-spring-normal spring)))
            (spring-dt spring) dt
            (damped-spring-target-vrn spring) 0d0)
      ;; apply spring force.
      (apply-impulses body-a body-b (damped-spring-r1 spring) (damped-spring-r2 spring)
                      (vec* (damped-spring-normal spring)
                            (* dt (spring-force spring distance))))
      (values))))

(defmethod apply-impulse ((spring damped-spring))
  (let* ((body-a (spring-body-a spring))
         (body-b (spring-body-b spring))
         (normal (damped-spring-normal spring))
         (r1 (damped-spring-r1 spring))
         (r2 (damped-spring-r2 spring))
         (n-mass (damped-spring-n-mass spring))
         ;; compute relative velocity
         (vrn (- (normal-relative-velocity body-a body-b r1 r2 normal)
                 (damped-spring-target-vrn spring)))
         ;; compute velocity loss from drag.
         ;; C source sez: "not 100% certain this is derived correctly, though it makes sense"
         (v-damp (- (* vrn (- 1d0 (exp (- (/ (* (damped-spring-damping spring)
                                                (damped-spring-dt spring))
                                             n-mass))))))))
    (setf (damped-spring-target-vrn spring) (+ vrn v-damp))
    (apply-impulses body-a body-b r1 r2 (vec* normal (* v-damp n-mass)))
    (values)))
