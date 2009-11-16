(defpackage #:squirl-demo-pyramid
  (:use :cl :uid :squirl :sheeple)
  (:export :run-demo))
(in-package :squirl-demo-pyramid)

(defvar *world*)
(defvar *floor*)

(defparameter *elapsed* 0)

(defparameter *step* (/ 1 60 2))

(defproto =squirl-demo-pyramid= (=engine=)
  ((window-width 640)
   (window-height 480)))

(defreply update ((demo =squirl-demo-pyramid=) delta &key)
  (declare (ignore delta))
  (world-step *world* *step*))

(defun ortho-projection (demo)
  (with-properties (window-width window-height) demo
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:viewport 0 0 window-width window-height)
    (let ((x (/ window-width 2))
          (y (/ window-height 2)))
     (gl:ortho (- x) x (- y) y -1 1))
    (gl:matrix-mode :modelview)))

(defreply init :after ((demo =squirl-demo-pyramid=))
  (reset-shape-id-counter)
  (ortho-projection demo)
  (setf *world* (make-world :iterations 20
                            :gravity (vec 0 -300)))
  (resize-world-active-hash *world* 40.0 2999)
  (resize-world-static-hash *world* 40.0 999)
  (setf *floor* (make-body))
  (attach-shape (make-segment (vec -600 -240)
                              (vec 600 -240)
                              :radius 1d0
                              :restitution 1
                              :friction 1)
                *floor*)
  (world-add-body *world* *floor*)
  (let ((friction 0.6)
        (verts (list (vec -3 -20)
                     (vec -3 20)
                     (vec 3 20)
                     (vec 3 -20))))
    (loop
       with n = 9
       for i from 1 to n
       for offset = (vec (- (/ (* i 60) 2))
                         (* (- n i) 52))
       do (loop
             for j from 0 below i do
               (mapc (lambda (body)
                       (attach-shape (make-poly verts
                                                :friction friction
                                                :restitution 0)
                                     body)
                       (world-add-body *world* body))
                     (nconc
                      (list (make-body
                             :mass 1
                             :inertia (moment-for-poly 1 verts)
                             :position (vec+ (vec (* j 60) -220)
                                             offset))
                            (make-body
                             :mass 1
                             :inertia (moment-for-poly 1 verts)
                             :position (vec+ (vec (* j 60) -197)
                                             offset)
                             :angle (/ pi 2)))
                      (unless (= j (1- i))
                        (list (make-body
                               :mass 1
                               :inertia (moment-for-poly 1 verts)
                               :position (vec+ (vec (+ (* j 60) 30)
                                                    -191)
                                               offset)
                               :angle (/ pi 2)))))))
         (mapc (lambda (body)
                 (attach-shape (make-poly verts
                                          :friction friction
                                          :restitution 0)
                               body)
                 (world-add-body *world* body))
               (list (make-body
                      :mass 1
                      :inertia (moment-for-poly 1 verts)
                      :position (vec+ (vec (- 17)
                                           -174)
                                      offset))
                     (make-body
                      :mass 1
                      :inertia (moment-for-poly 1 verts)
                      :position (vec+ (vec (+ (* (1- i) 60) 17)
                                           -174)
                                      offset)))))))

(defreply draw ((demo =squirl-demo-pyramid=) &key)
  (map-world #'draw-body *world*))

(defun draw-body (body)
  (let* ((position (body-position body))
         (x (vec-x position))
         (y (vec-y position)))
    (with-color *green*
      (map nil #'draw-shape (body-shapes body)))
    (draw-circle (make-point x y) 2 :resolution 30 :color *red*)))

(defgeneric draw-shape (shape)
  (:method ((circle circle))
    (let* ((circle-center (circle-transformed-center circle))
           (edge (vec* (body-rotation (shape-body circle))
                       (circle-radius circle)))
           (edge-t (vec+ edge circle-center))
           (edge-neg-t (vec- circle-center edge)))
      (draw-circle (make-point (vec-x circle-center) (vec-y circle-center))
                   (round (circle-radius circle)) :filledp nil)
      (draw-line (make-point (vec-x edge-t) (vec-y edge-t))
                 (make-point (vec-x edge-neg-t) (vec-y edge-neg-t)))))
  (:method ((seg segment))
    (let ((a (segment-trans-a seg))
          (b (segment-trans-b seg)))
      (draw-line (make-point (vec-x a) (vec-y a))
                 (make-point (vec-x b) (vec-y b)))))
  (:method ((poly poly))
    (let ((vertices (poly-transformed-vertices poly)))
      (loop for i below (length vertices)
         for a = (elt vertices i)
         for b = (elt vertices (mod (1+ i) (length vertices)))
         do (draw-line (make-point (vec-x a) (vec-y a))
                       (make-point (vec-x b) (vec-y b)))))))

(defun run-demo ()
  (run =squirl-demo-pyramid=))
