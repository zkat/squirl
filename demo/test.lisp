(defpackage #:squirl-demo
  (:use :cl :uid :squirl :sheeple))
(in-package :squirl-demo)

(defproto =squirl-demo= (=engine=)
  ((title "Demo for SquirL")
   (window-width 500)
   (window-height 500)
   (world (make-world :gravity (vec 0 -100)))
   (circles nil)
   (accumulator 0)
   (physics-timestep 1/100)))

(defun draw-a-circle (circle)
  (let* ((position (body-position circle))
         (x (vec-x position))
         (y (vec-y position)))
    (draw-circle (make-point x y) 15)))

(defreply draw ((demo =squirl-demo=) &key)
  (map nil #'draw-a-circle (circles demo)))

;; This allows us to fix the physics timestep without fixing the framerate.
;; This means the physics -should- run at the same perceived speed no matter
;; how fast your computer's calculating :)
(defreply update ((demo =squirl-demo=) dt &key)
  ;; update the physics world
  (incf (accumulator demo) dt)
  (loop while (>= (accumulator demo) (physics-timestep demo))
     do (world-step (world demo) (physics-timestep demo))
       (decf (accumulator demo) (physics-timestep demo)))

  ;; remove shapes that are long gone
  (map nil (lambda (c) (setf (circles demo)
                             (delete c (circles demo)))
                   (world-remove-body (world demo) c)
                   (map nil (lambda (x) (world-remove-shape (world demo) x)) (squirl::body-shapes c)))
       (remove-if (lambda (c) (> (vec-y (body-position c)) -100)) (circles demo))))

(defun add-circle (demo x y)
  (let* ((mass 1)
         (radius 15)
         (inertia (moment-for-circle mass 0 radius (vec 0 0)))
         (body (make-body mass inertia x y)))
    (world-add-shape (world demo) (make-circle body radius (vec 0 0)))
    (world-add-body (world demo) body)
    (push body (circles demo))))

(defreply mouse-down ((engine =squirl-demo=) button)
  (case button
    (0 (add-circle engine (mouse-x engine) (mouse-y engine)))))
