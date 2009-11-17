(in-package :squirl-demo)

(defparameter *logo* #(15 -16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 -64 15 63 -32 -2 0 0 0 0 0 0 0
                       0 0 0 0 0 0 0 0 0 0 0 31 -64 15 127 -125 -1 -128 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       0 0 0 127 -64 15 127 15 -1 -64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 -64 15 -2
                       31 -1 -64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 -64 0 -4 63 -1 -32 0 0 0 0 0 0
                       0 0 0 0 0 0 0 0 0 0 1 -1 -64 15 -8 127 -1 -32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                       1 -1 -64 0 -8 -15 -1 -32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -31 -1 -64 15 -8 -32
                       -1 -32 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 7 -15 -1 -64 9 -15 -32 -1 -32 0 0 0 0 0
                       0 0 0 0 0 0 0 0 0 0 31 -15 -1 -64 0 -15 -32 -1 -32 0 0 0 0 0 0 0 0 0 0 0 0 0
                       0 0 63 -7 -1 -64 9 -29 -32 127 -61 -16 63 15 -61 -1 -8 31 -16 15 -8 126 7 -31
                       -8 31 -65 -7 -1 -64 9 -29 -32 0 7 -8 127 -97 -25 -1 -2 63 -8 31 -4 -1 15 -13
                       -4 63 -1 -3 -1 -64 9 -29 -32 0 7 -8 127 -97 -25 -1 -2 63 -8 31 -4 -1 15 -13
                       -2 63 -1 -3 -1 -64 9 -29 -32 0 7 -8 127 -97 -25 -1 -1 63 -4 63 -4 -1 15 -13
                       -2 63 -33 -1 -1 -32 9 -25 -32 0 7 -8 127 -97 -25 -1 -1 63 -4 63 -4 -1 15 -13
                       -1 63 -33 -1 -1 -16 9 -25 -32 0 7 -8 127 -97 -25 -1 -1 63 -4 63 -4 -1 15 -13
                       -1 63 -49 -1 -1 -8 9 -57 -32 0 7 -8 127 -97 -25 -8 -1 63 -2 127 -4 -1 15 -13
                       -1 -65 -49 -1 -1 -4 9 -57 -32 0 7 -8 127 -97 -25 -8 -1 63 -2 127 -4 -1 15 -13
                       -1 -65 -57 -1 -1 -2 9 -57 -32 0 7 -8 127 -97 -25 -8 -1 63 -2 127 -4 -1 15 -13
                       -1 -1 -57 -1 -1 -1 9 -57 -32 0 7 -1 -1 -97 -25 -8 -1 63 -1 -1 -4 -1 15 -13 -1
                       -1 -61 -1 -1 -1 -119 -57 -32 0 7 -1 -1 -97 -25 -8 -1 63 -1 -1 -4 -1 15 -13 -1
                       -1 -61 -1 -1 -1 -55 -49 -32 0 7 -1 -1 -97 -25 -8 -1 63 -1 -1 -4 -1 15 -13 -1
                       -1 -63 -1 -1 -1 -23 -49 -32 127 -57 -1 -1 -97 -25 -1 -1 63 -1 -1 -4 -1 15 -13
                       -1 -1 -63 -1 -1 -1 -16 -49 -32 -1 -25 -1 -1 -97 -25 -1 -1 63 -33 -5 -4 -1 15
                       -13 -1 -1 -64 -1 -9 -1 -7 -49 -32 -1 -25 -8 127 -97 -25 -1 -1 63 -33 -5 -4 -1
                       15 -13 -1 -1 -64 -1 -13 -1 -32 -49 -32 -1 -25 -8 127 -97 -25 -1 -2 63 -49 -13
                       -4 -1 15 -13 -1 -1 -64 127 -7 -1 -119 -17 -15 -1 -25 -8 127 -97 -25 -1 -2 63
                       -49 -13 -4 -1 15 -13 -3 -1 -64 127 -8 -2 15 -17 -1 -1 -25 -8 127 -97 -25 -1
                       -8 63 -49 -13 -4 -1 15 -13 -3 -1 -64 63 -4 120 0 -17 -1 -1 -25 -8 127 -97 -25
                       -8 0 63 -57 -29 -4 -1 15 -13 -4 -1 -64 63 -4 0 15 -17 -1 -1 -25 -8 127 -97
                       -25 -8 0 63 -57 -29 -4 -1 -1 -13 -4 -1 -64 31 -2 0 0 103 -1 -1 -57 -8 127 -97
                       -25 -8 0 63 -57 -29 -4 -1 -1 -13 -4 127 -64 31 -2 0 15 103 -1 -1 -57 -8 127
                       -97 -25 -8 0 63 -61 -61 -4 127 -1 -29 -4 127 -64 15 -8 0 0 55 -1 -1 -121 -8
                       127 -97 -25 -8 0 63 -61 -61 -4 127 -1 -29 -4 63 -64 15 -32 0 0 23 -1 -2 3 -16
                       63 15 -61 -16 0 31 -127 -127 -8 31 -1 -127 -8 31 -128 7 -128 0 0))

(defparameter *image-width* 188)
(defparameter *image-height* 35)
(defparameter *image-row-length* 24)
(defvar *bullet*)

(defclass logo-smash (demo)
  ()
  (:default-initargs :name "Smash that damn logo."))

(defun get-pixel (x y)
  (logand (ash (svref *logo* (+ (ash x -3) (* y *image-row-length*)))
               (- (logand (lognot x) 7))) 1))

(defun make-ball (x y)
  (make-body :mass 1.0 :position (vec x y) :shapes (list (make-circle 0.95))))

(defmethod draw-demo ((demo logo-smash))
  (gl:point-size 0.95)
  (gl:with-primitives :points
    (map-world #'set-body-point (world demo))))

(defmethod update-demo ((demo logo-smash) dt)
  (world-step (world demo) 1/60))

(defmethod init-demo ((demo logo-smash))
  (setf (world demo) (make-world :iterations 1))
  (resize-world-active-hash (world demo) 2.0 10000)
  (loop for y below *image-height*
     do (loop for x below *image-width*
           for x-jitter = (random 0.05) for y-jitter = (random 0.05)
           unless (zerop (get-pixel x y))
           do (let ((ball (make-ball (* 2 (- x (/ *image-width* 2) (- x-jitter)))
                                     (* 2 (- (/ *image-height* 2) y (- y-jitter))))))
                (world-add-body (world demo) ball))))
  (let ((bullet (make-body :position (vec -800 -10)
                           :velocity (vec 400 0)
                           :mass 100000 :inertia 100000
                           :actor :bullet)))
    (attach-shape (make-circle 8) bullet)
    (world-add-body (world demo) bullet)
    (setf *bullet* bullet))
  (world demo))

(defmethod grabbablep ((obj (eql :bullet))) nil)

(pushnew 'logo-smash *demos*)