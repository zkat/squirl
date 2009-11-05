(defpackage #:squirl-demo
  (:use :cl :uid :squirl :sheeple))
(in-package :squirl-demo)

(defproto =squirl-demo= (=engine=)
  ((title "Demo for SquirL")
   (window-width 500)
   (window-height 500)
   (world (squirl::make-world :gravity (squirl::vec 0 -9.8)))))

(defparameter *blocks* nil)

(defun draw-block (blk)
  (let ((position (squirl::body-position blk)))
    (draw-rectangle (squirl::vec-x position) (squirl::vec-y position) 15 15)))

(defreply draw ((demo =squirl-demo=) &key)
  (map nil #'draw-block *blocks*))

(defreply update ((demo =squirl-demo=) dt &key)
  (squirl::world-step (world demo) dt))

(defun add-circle (demo x y)
  (let* ((mass 1)
         (radius 15)
         (inertia (squirl::moment-for-circle mass 0 radius (squirl::vec 0 0)))
         (body (squirl::make-body mass inertia)))
    (setf (squirl::body-position body) (squirl::vec x y))
    (squirl::world-add-body (world demo) body)
    (push body *blocks*)
    (squirl::make-circle body radius (squirl::vec 0 0))))

(defreply mouse-down ((engine =squirl-demo=) button)
  (case button
    (0 (add-circle engine (mouse-x engine) (mouse-y engine)))))