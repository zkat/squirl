;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defun circle-to-circle-query (p1 p2 r1 r2)
  (let* ((delta (vec- p2 p1))
         (mindist (+ r1 r2))
         (distsq (vec-length-sq delta)))
   (when (< distsq
            (expt mindist 2))
     (let* ((dist (sqrt distsq)))
       (flet ((maybe/ (a b)
                (if (= b 0) 0
                    (/ a b))))
        (make-contact (vec+ p1 (vec* delta
                                     (+ 0.5 (maybe/ (- r1 (/ mindist 2))
                                                    dist))))
                      (vec* delta (maybe/ 1 dist))
                      (- dist mindist)
                      0))))))

(defun circle-to-circle (shape-1 shape-2)
  "Collide circles."
  (circle-to-circle-query (circle-transformed-center shape-1)
                          (circle-transformed-center shape-2)
                          (circle-radius shape-1)
                          (circle-radius shape-2)))
