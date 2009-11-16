(defpackage #:squirl-demo
  (:use :cl :uid :squirl :sheeple)
  (:export :run-demo))
(in-package :squirl-demo)

(defparameter *sleep-ticks* 16)

(defvar *world*)
(defvar *current-demo*)

(defvar *mouse-point*)
(defvar *mouse-point-last*)
(defvar *mouse-body*)
(defvar *mouse-joint*)

(defvar *key-up* nil)
(defvar *key-down* nil)
(defvar *key-left* nil)
(defvar *key-right* nil)

(defvar *arrow-direction* +zero-vector+)

(defun draw-string (x y string)
  (gl:color 0 0 0)
  (gl:raster-pos x y)
  (glut:bitmap-string :bitmap-helvetica-10 string))

(defun draw-instructions ()
  (let ((x -300)
        (y 220))
    (draw-string x y "Controls:")
    (draw-string x (- y 16) "TODO.")))

(defun display ()
  (gl:clear :color-buffer-bit)
  (draw-world world)
  (draw-instructions)
  (glut:swap-buffers)
  (let ((new-point (vec-lerp *mouse-point-last* *mouse-point* 1/4)))
    (setf (body-position *mouse-body*) new-point
          (body-velocity *mouse-body*) (vec* (vec- new-point *mouse-point-last*) 60)
          *mouse-point-last* new-point))
  (update *current-demo*))

(defun demo-title (demo)
  (concatenate 'string "Demo: " (demo-name demo)))

(defun run-demo (demo)
  (setf *current-demo* demo
        *mouse-joint* nil
        *world* (init-demo demo))
  (glut:set-window-title (demo-title demo)))

(defun keyboard (key)
  ;; todo
  )

(defun mouse-to-space (x y)
  (let ((model (gl:get-double :modelview-matrix))
        (proj (gl:get-double :projection-matrix))
        (view (gl:get-double :viewport)))
    (multiple-value-bind (mx my mz)
        (glu:un-project x (- (glut:get :window-height) y) 0
                        :modelview model :projection proj :viewport view)
      (vec mx my))))

(defun mouse (x y)
  (setf *mouse-point* (mouse-to-space x y)))

(defun click (button state x y)
  ;; todo
  )

(defun timer-call (value)
  ;; todo
  (glut:post-redisplay))

(defun set-arrow-direction ()
  (let ((x 0) (y 0))
    (when *key-up* (incf y))
    (when *key-down* (decf y))
    (when *key-right* (incf x))
    (when *key-left* (decf x))
    (setf *arrow-direction* (vec x y))))

(defun arrow-key-down (key x y)
  ;; todo
  )

(defun arrow-key-up (key x y)
  ;; todo
  )

(defun idle ()
  (glut:post-redisplay))

(defun init-gl ()
  (gl:clear-color 1 1 1 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -320 320 -240 240 -1 1)
  (gl:translate 1/2 1/2 0)
  (gl:enable-client-state :vertex-array))

(defun glut-stuff ()
  ;; todo
  (glut:init)
  (glut:init-display-mode (logior :double :rgba))
  (glut:init-window-size 640 480)
  (glut:create-window "Squirl Demo")
  (init-gl)
  ;; ...
  )

(defun run-demo ()
  (setf *mouse-body* (make-body))
  ;; todo
  )
