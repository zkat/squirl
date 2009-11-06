(defpackage #:squirl-demo
  (:use :cl :uid :squirl :sheeple))
(in-package :squirl-demo)

(defproto =squirl-demo= (=engine=)
  ((title "Demo for SquirL")
   (window-width 500)
   (window-height 500)
   (world nil)
   (accumulator 0)
   (physics-timestep 1/100)))

(defun draw-a-circle (circle)
  (let* ((position (body-position circle))
         (x (vec-x position))
         (y (vec-y position)))
    (draw-circle (make-point x y) 15 :resolution 30)))

(defun draw-body (body)
  (let* ((position (body-position body))
         (x (vec-x position))
         (y (vec-y position)))
    (with-color *green*
     (map nil #'draw-shape (body-shapes body)))
    (draw-circle (make-point x y) 2 :resolution 30 :color *red*)))

(defreply init ((demo =squirl-demo=))
  (setf (world demo) (make-world :gravity (vec 0 -100)))
  (let ((body (make-body))
        (floor (make-segment (vec 0 10) (vec 500 10) :elasticity 1 :friction 0.001)))
    (attach-shape floor body)
    (world-add-body (world demo) body)))

(defgeneric draw-shape (shape)
  (:method ((circle circle))
    (let ((circle-center (circle-transformed-center circle)))
      (draw-circle (make-point (vec-x circle-center) (vec-y circle-center))
                   (round (circle-radius circle)) :filledp nil)))
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

(defreply draw ((demo =squirl-demo=) &key)
  (map nil #'draw-body (world-bodies (world demo))))

;; This allows us to fix the physics timestep without fixing the framerate.
;; This means the physics -should- run at the same perceived speed no matter
;; how fast your computer's calculating :)
(defreply update ((demo =squirl-demo=) dt &key)
  (update-world-state demo dt)
  (empty-out-bottomless-pit (world demo)))

(defun update-world-state (demo dt)
  (incf (accumulator demo) (if (> dt 0.5) 0.5 dt))
  (loop while (>= (accumulator demo) (physics-timestep demo))
     do (world-step (world demo) (physics-timestep demo))
     (decf (accumulator demo) (physics-timestep demo))))

(defun empty-out-bottomless-pit (world)
  "Get rid of any bodies that have fallen into the bottomless pit."
  (map nil (lambda (c) (world-remove-body world c))
       (remove-if (lambda (c) (> (vec-y (body-position c)) -100))
                  (world-bodies world))))

(defun add-circle (demo x y)
  (let* ((mass 1)
         (radius 15)
         (inertia (moment-for-circle mass 0 radius (vec 0 0)))
         (body (make-body :mass mass :inertia inertia :position (vec x y))))
    (attach-shape (make-circle radius :elasticity 0.5 :friction 0.01) body)
    (world-add-body (world demo) body)))

(defreply mouse-down ((engine =squirl-demo=) button)
  (case button
    (0 (add-circle engine (mouse-x engine) (mouse-y engine)))))
