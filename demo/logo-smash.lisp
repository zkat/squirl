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

(defun get-pixel (x y)
  (logand (ash (svref *logo* (+ (ash x -3) (* y *image-row-length*)))
               (logand (1+ (lognot x)) #x7)) 1))

(defproto =logo-smash= (=engine=)
  ((title "Murder that logo!")
   (window-width 500)
   (window-height 500)
   (clear-color *white*)
   (world nil)))

(defun ortho-projection (demo)
  (with-properties (window-width window-height) demo
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:viewport 0 0 window-width window-height)
    (gl:ortho (- (/ window-width 2)) (/ window-width 2) (- (/ window-height 2)) (/ window-height 2) 10 0)
    (gl:matrix-mode :modelview)))

(defreply update ((engine =logo-smash=) dt &key)
  (world-step (world engine) (* 1d0 dt)))

(defun draw-pixel (body)
  (let* ((position (body-position body))
         (x (vec-x position))
         (y (vec-y position)))
    (if (eq body *bullet*)
        (draw-circle (make-point x y) 8 :resolution 50 :color *red*)
        (draw-circle (make-point x y) 1 :resolution 50 :color *black*))))

(defreply draw ((engine =logo-smash=) &key)
  (map-world #'draw-pixel (world engine)))

(defun make-ball (x y)
  (let ((body (make-body :mass 1.0 :position (vec x y))))
    (attach-shape (make-circle 0.95) body)
     body))

(defreply init :after ((engine =logo-smash=) &key)
  "foo"
  (ortho-projection engine)
  (setf (world engine) (make-world :iterations 1))
  (loop for y below *image-height*
     do (loop for x below *image-width*
           for x-jitter = (random 0.05) for y-jitter = (random 0.05)
           unless (zerop (get-pixel x y))
           do (let ((ball (make-ball (* 2 (- x (/ *image-width* 2) (- x-jitter)))
                                     (* 2 (- (/ *image-height* 2) y (- y-jitter))))))
                (world-add-body (world engine) ball))))
  (let ((bullet (make-body :position (vec -1000 -10)
                           :velocity (vec 400 0))))
    (attach-shape (make-circle 8) bullet)
    (world-add-body (world engine) bullet)
    (setf *bullet* bullet)))
