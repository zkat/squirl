(defpackage #:squirl-demo
  (:use :cl :squirl)
  (:export :run-all-demos))
(in-package :squirl-demo)

(defparameter *sleep-ticks* 16)

(defvar *world* nil)
(defvar *demos* nil)
(defvar *current-demo*)

(defvar *mouse-position* +zero-vector+)
(defvar *mouse-point-last* +zero-vector+)
(defvar *mouse-body* (make-body))
(defvar *mouse-joint*)

(defvar *key-up* nil)
(defvar *key-down* nil)
(defvar *key-left* nil)
(defvar *key-right* nil)

(defvar *ticks* 0)

(defvar *arrow-direction* +zero-vector+)

(defclass squirl-window (glut:window)
  ()
  (:default-initargs :width 640 :height 480 :mode '(:double :rgba) :title "Squirl Demo App"))

(defun draw-string (x y string)
  (gl:color 0 0 0)
  (gl:raster-pos x y)
  (glut:bitmap-string glut:+bitmap-helvetica-10+ string))

(defun draw-instructions ()
  (let ((x -300) (y 220))
    (draw-string x y (format nil
                             "Controls:~@
                              N chooses the next demo~@
                              Use the mouse to grab objects~@
                              Arrow keys control some demos~@
                              \\ enables anti-aliasing."))))

(defclass demo () ((name :initarg :name :accessor demo-name)))
(defgeneric update-demo (demo ticks))
(defgeneric init-demo (demo))

(defmethod glut:idle ((w squirl-window))
  (sleep 0.016)
  (glut:post-redisplay))

(defmethod glut:display ((w squirl-window))
  (gl:clear :color-buffer-bit)
  (draw-world *world*)
  (draw-instructions)
  (glut:swap-buffers)
  (incf *ticks*)
  (let ((new-point (vec-lerp *mouse-point-last* *mouse-position* 1/4)))
    (setf (body-position *mouse-body*) new-point
          (body-velocity *mouse-body*) (vec* (vec- new-point *mouse-point-last*) 60d0)
          *mouse-point-last* new-point)
    (update-demo *current-demo* *ticks*)))

(defun demo-title (demo)
  (concatenate 'string "Demo: " (demo-name demo)))

(defun run-demo (demo-class)
  (let ((demo (make-instance demo-class)))
    (setf *current-demo* demo
          *mouse-joint* nil
          *ticks* 0
          *world* (init-demo demo))
    #+nil(glut:set-window-title (demo-title demo))))

(defmethod glut:keyboard ((w squirl-window) key x y)
  (declare (ignore x y))
  (case key
    (#\Esc (glut:destroy-current-window))
    (#\Return (run-demo (class-of *current-demo*)))
    (#\n (run-demo (elt *demos*
                        (mod (1+ (position (class-name (class-of *current-demo*)) *demos*))
                             (length *demos*)))))
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
    (multiple-value-bind (mx my)
        (glu:un-project x (- (glut:get :window-height) y) 0
                        :modelview model :projection proj :viewport view)
      (vec mx my))))

(defmethod glut:motion ((w squirl-window) x y)
  (setf *mouse-position* (mouse-to-space x y)))
(defmethod glut:passive-motion ((w squirl-window) x y)
  (setf *mouse-position* (mouse-to-space x y)))

(defmethod glut:mouse ((w squirl-window) button state x y)
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
                      (squirl::constraint-bias-coefficient *mouse-joint*) 0.15)
                (world-add-constraint *world* *mouse-joint*))))
          (progn (world-remove-constraint *world* *mouse-joint*) (setf *mouse-joint* nil)))))

(cffi:defcallback timercall :void ((value :int))
  (declare (ignore value))
  (glut:timer-func 16 (cffi:callback timercall) 0)
  (glut:post-redisplay))

(defun set-arrow-direction ()
  (let ((x 0) (y 0))
    (when *key-up* (incf y))
    (when *key-down* (decf y))
    (when *key-right* (incf x))
    (when *key-left* (decf x))
    (setf *arrow-direction* (vec x y))))

(defmethod glut:special ((w squirl-window) key x y)
  (case key
    (:key-up (setf *key-up* t))
    (:key-down (setf *key-down* t))
    (:key-left (setf *key-left* t))
    (:key-right (setf *key-right* t)))
  (set-arrow-direction))

(defmethod glut:special-up ((w squirl-window) key x y)
  (case key
    (:key-up (setf *key-up* nil))
    (:key-down (setf *key-down* nil))
    (:key-left (setf *key-left* nil))
    (:key-right (setf *key-right* nil)))
  (set-arrow-direction))

(defmethod glut:display-window :before ((w squirl-window))
  (gl:clear-color 1 1 1 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -320 320 -240 240 -1 1)
  (gl:translate 1/2 1/2 0)
  (gl:enable-client-state :vertex-array)
  #+nil(glut:timer-func 16 (cffi:callback timercall) 0))

(defun run-all-demos ()
  (setf *mouse-body* (make-body))
  (when *demos*
    (run-demo (car *demos*)))
  (glut:display-window (make-instance 'squirl-window))
  ;; this is a kludge around an apparent cl-glut bug.
  (setf glut::*glut-initialized-p* nil))
