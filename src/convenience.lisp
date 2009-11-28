;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defun make-rectangle (width height &key (restitution 0d0) (friction 0d0) (offset +zero-vector))
  (let* ((width/2 (/ width 2))
         (height/2 (/ height 2))
         (verts (list (vec (- width/2) height/2)
                      (vec width/2 height/2)
                      (vec width/2 (- height/2))
                      (vec (- width/2) (- height/2)))))
    (make-poly verts :restitution restitution :friction friction :offset offset)))

(defun make-circle-body ()
  ;; todo
  )
(defun make-segment-body ()
  ;; todo
  )
(defun make-poly-body ()
  ;; todo
  )
(defun make-rectangle-body ()
  ;; todo
  )