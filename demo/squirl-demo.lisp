(defpackage #:squirl-demo
  (:use :cl :uid :squirl :sheeple)
  (:export :run-demo))
(in-package :squirl-demo)

(defparameter *sleep-ticks* 16)

(defvar *world*)
(defvar *demos* nil)
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

(defclass squirl-demo (glut:window)
  ()
  (:default-initargs :width 640 :height 480 :mode '(:double :rgba) :title (demo-title (car *demos*))))

(defun draw-string (x y string)
  (gl:color 0 0 0)
  (gl:raster-pos x y)
  (glut:bitmap-string :bitmap-helvetica-10 string))

(defun draw-instructions ()
  (let ((x -300)
        (y 220))
    (draw-string x y "Controls:")
    (draw-string x (- y 16) "TODO.")))

(defmethod glut:display ((w squirl-demo))
  (gl:clear :color-buffer-bit)
  (draw-world world)
  (draw-instructions)
  (glut:swap-buffers)
  (let ((new-point (vec-lerp *mouse-point-last* *mouse-point* 1/4)))
    (setf (body-position *mouse-body*) new-point
          (body-velocity *mouse-body*) (vec* (vec- new-point *mouse-point-last*) 60)
          *mouse-point-last* new-point)
    (update *current-demo*)))

(defun demo-title (demo)
  (concatenate 'string "Demo: " (demo-name demo)))

(defun run-demo (demo)
  (setf *current-demo* demo
        *mouse-joint* nil
        *world* (init-demo demo))
  (glut:set-window-title (demo-title demo)))

(defmethod glut:keyboard ((w squirl-demo) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\Return (run-demo *current-demo*))
    (#\\ (gl:enable :line-smooth)
         (gl:enable :point-smooth)
         (gl:enable :blend)
         (gl:blend-func :src-alpha :one-minus-src-alpha)
         (gl:hint :line-smooth-hint :dont-care)
         (gl:hint :point-smooth-hint :dont-care))))

(defun mouse-to-space (x y)
  (let ((model (gl:get-double :modelview-matrix))
        (proj (gl:get-double :projection-matrix))
        (view (gl:get-double :viewport)))
    (multiple-value-bind (mx my mz)
        (glu:un-project x (- (glut:get :window-height) y) 0
                        :modelview model :projection proj :viewport view)
      (vec mx my))))

(defmethod glut:passive-motion ((w squirl-demo) button state x y)
  (setf *mouse-point* (mouse-to-space x y)))

(defmethod glut:mouse ((w squirl-demo) button state x y)
  (if (eq button :left-button)
      (if (eq state :down)
          (let* ((point (mouse-to-space x y))
                 (shape (world-point-query-first *world* point)))
            (when shape
              (let ((body (shape-body shape)))
                (setf *mouse-joint* (make-pivot-joint *mouse-body* body 
                                                      +zero-vector+ 
                                                      (world->body-local body point))
                      (squirl::constraint-max-force *mouse-joint*) 50000
                      (squirl::constraint-bias-coefficient *mouse-joint* 0.15))
                (world-add-constraint *world* *mouse-joint*))))
          (progn (world-remove-constraint *world* *mouse-joint*) (setf *mouse-joint* nil)))))

(defmethod glut:idle ((w squirl-demo))
  (glut:post-redisplay))

(defun set-arrow-direction ()
  (let ((x 0) (y 0))
    (when *key-up* (incf y))
    (when *key-down* (decf y))
    (when *key-right* (incf x))
    (when *key-left* (decf x))
    (setf *arrow-direction* (vec x y))))

(defmethod glut:special ((w squirl-demo) key x y)
  (case key
    (:key-up (setf *key-up* t))
    (:key-down (setf *key-down* t))
    (:key-left (setf *key-left* t))
    (:key-right (setf *key-right* t)))
  (set-arrow-direction))

(defmethod glut:special-up ((w squirl-demo) key x y)
  (case key
    (:key-up (setf *key-up* nil))
    (:key-down (setf *key-down* nil))
    (:key-left (setf *key-left* nil))
    (:key-right (setf *key-right* nil)))
  (set-arrow-direction))

(defmethod glut:display-window ((w squirl-demo))
  (gl:clear-color 1 1 1 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -320 320 -240 240 -1 1)
  (gl:translate 1/2 1/2 0)
  (gl:enable-client-state :vertex-array))

(defun run-demo ()
  (setf *mouse-body* (make-body))
  (glut:display-window (make-instance 'squirl-demo))
  (when *demos*
    (run-demo (car *demos*))))
