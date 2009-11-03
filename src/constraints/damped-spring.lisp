;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defstruct (damped-spring (:include spring))
  anchor1 anchor2 rest-length damping target-vrn r1 r2 n-mass normal)

(defmethod spring-force ((spring damped-spring) distance)
  (* (- (damped-spring-rest-length spring) distance)
     (spring-stiffness spring)))

(defmethod pre-step ((spring damped-spring) dt dt-inverse)
  (let ((body-a (spring-body-a spring))
        (body-b (spring-body-b spring)))
    (setf (damped-spring-r1 spring) (vec-rotate (damped-spring-anchor1 spring)
                                                (body-rotation body-a))
          (damped-spring-r2 spring) (vec-rotate (damped-spring-anchor2 spring)
                                                (body-rotation body-b)))
    (let* ((delta (vec- (vec+ (body-position body-b) (damped-spring-r2 spring))
                        (vec+ (body-position body-a) (damped-spring-r1 spring))))
           (distance (vec-length delta)))
      (setf (damped-spring-normal spring) (vec* delt (maybe/ distance))
            ;; calculate mass normal
            (damped-spring-n-mass spring) (/ (k-scalar body-a body-b
                                                       (damped-spring-r1 spring)
                                                       (damped-spring-r2 spring)
                                                       (damped-spring-normal spring)))
            (spring-dt spring) dt
            (damped-spring-target-vrn spring) 0.0)
      ;; apply spring force.
      (apply-impulses body-a body-b (damped-spring-r1 spring) (damped-spring-r2 spring)
                      (vec* (damped-spring-normal spring)
                            (* dt (spring-force spring)))))))
