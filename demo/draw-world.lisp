(in-package :squirl-demo)

;;;
;;; Configuration
;;;
(defparameter *line-color* '(0 0 0 1))
(defparameter *collision-color* '(1 0 0 1))
(defparameter *body-color* '(0 0 1 1))
(defparameter *line-width* 2.5)
(defparameter *bb-color* '(1 0 0 1))
(defparameter *bb-line-width* 2)

;;;
;;; Primitives
;;;
(defun draw-circle (x y radius &key (resolution 10) (filled t))
  (let* ((theta (* 2 (/ pi resolution)))
         (tangential-factor (tan theta))
         (radial-factor (- 1 (cos theta))))
    (gl:with-primitives (if filled :triangle-fan :line-loop)
      (loop with curr-x = (+ x radius)
         with curr-y = y
         repeat resolution
         do (gl:vertex curr-x curr-y )
         (let ((tx (- (- curr-y y)))
               (ty (- curr-x x)))
           (incf curr-x (* tx tangential-factor))
           (incf curr-y (* ty tangential-factor)))
         (let ((rx (- x curr-x))
               (ry (- y curr-y)))
           (incf curr-x (* rx radial-factor))
           (incf curr-y (* ry radial-factor)))))))

(defun draw-line (x1 y1 x2 y2)
  (gl:with-primitives :lines
    (gl:vertex x1 y1)
    (gl:vertex x2 y2)))

(defun draw-poly (vertices &key (filled t))
  (gl:with-primitives (if filled :polygon :lines)
    (loop for i below (length vertices)
       for a = (elt vertices i)
       for b = (elt vertices (mod (1+ i) (length vertices)))
       do (gl:vertex (vec-x a) (vec-y a))
       (gl:vertex (vec-x b) (vec-y b)))))

;;;
;;; Bodies and shapes
;;;
(defparameter *color-hash* (make-hash-table :test 'eq))
(defun clear-color-hash ()
  (clrhash *color-hash*))

(defun ensure-color (shape)
  (or (gethash shape *color-hash*)
      (setf (gethash shape *color-hash*)
            (list (random 0.9) (random 0.9) (random 0.9) 1))))

(defun draw-body (body)
  (map nil #'draw-shape (body-shapes body)))

(defun draw-bbox (body)
  (map nil #'draw-shape-bbox (body-shapes body)))

(defun draw-shape-bbox (shape)
  (let ((bbox (squirl::shape-bbox shape)))
    (apply #'gl:color *bb-color*)
    (gl:line-width *bb-line-width*)
    (gl:with-primitives :line-loop
      (gl:vertex (squirl::bbox-left bbox) (squirl::bbox-bottom bbox))
      (gl:vertex (squirl::bbox-left bbox) (squirl::bbox-top bbox))
      (gl:vertex (squirl::bbox-right bbox) (squirl::bbox-top bbox))
      (gl:vertex (squirl::bbox-right bbox) (squirl::bbox-bottom bbox)))))

(defgeneric draw-shape (shape))
(defmethod draw-shape :before (shape)
  (apply #'gl:color (ensure-color shape)))

(defmethod draw-shape ((circle circle))
  (let* ((body (shape-body circle))
         (center (circle-transformed-center circle))
         (x (vec-x center))
         (y (vec-y center))
         (radius (circle-radius circle))
         (edge (vec* (body-rotation body) radius))
         (edge-t (vec+ edge center)))
    (draw-circle x y (round radius) :resolution 30 :filled t)
    (gl:color 0 0 0)
    (gl:line-width *line-width*)
    (draw-circle x y (round radius) :resolution 30 :filled nil)
    (draw-line (vec-x edge-t) (vec-y edge-t) (vec-x center) (vec-y center))))



(defparameter *pill-var* '((0.0000 . 1.0000)
                           (0.2588 . 0.9659)
                           (0.5000 . 0.8660)
                           (0.7071 . 0.7071)
                           (0.8660 . 0.5000)
                           (0.9659 . 0.2588)
                           (1.0000 . 0.0000)
                           (0.9659 . -0.2588)
                           (0.8660 . -0.5000)
                           (0.7071 . -0.7071)
                           (0.5000 . -0.8660)
                           (0.2588 . -0.9659)
                           (0.0000 . -1.0000)
                           (0.0000 . -1.0000)
                           (-0.2588 . -0.9659)
                           (-0.5000 . -0.8660)
                           (-0.7071 . -0.7071)
                           (-0.8660 . -0.5000)
                           (-0.9659 . -0.2588)
                           (-1.0000 . -0.0000)
                           (-0.9659 . 0.2588)
                           (-0.8660 . 0.5000)
                           (-0.7071 . 0.7071)
                           (-0.5000 . 0.8660)
                           (-0.2588 . 0.9659)
                           (0.0000 . 1.0000)))

(defmethod draw-shape ((seg segment))
  (let ((a (vec+ (body-position (shape-body seg))
                 (vec-rotate (squirl::segment-a seg)
                             (body-rotation (shape-body seg)))))
        (b (vec+ (body-position (shape-body seg))
                 (vec-rotate (squirl::segment-b seg)
                             (body-rotation (shape-body seg))))))
    (if (< 0 (squirl::segment-radius seg))
        (let* ((delta (vec- b a))
               (length (/ (vec-length delta) (squirl::segment-radius seg)))
               (verts (loop with verts = (copy-tree *pill-var*)
                         for i below (/ (length *pill-var*) 2)
                         do (incf (car (elt verts i)) length)
                         finally (return verts))))
          (gl:with-pushed-matrix
            (let ((x (vec-x a))
                  (y (vec-y a))
                  (cos (/ (vec-x delta) length))
                  (sin (/ (vec-y delta) length)))
              (gl:mult-matrix (make-array '(4 4)
                                          :initial-contents
                                          (list (list cos sin 0 0)
                                                (list (- sin) cos 0 0)
                                                (list 0 0 1 1)
                                                (list x y 0 1))))
              (gl:with-primitives :triangle-fan
                (loop for (x . y) in verts
                   do (gl:vertex x y)))
              (apply #'gl:color *line-color*)
              (gl:with-primitives :line-loop
                (loop for (x . y) in verts
                   do (gl:vertex x y))))))
        (progn (gl:line-width (squirl::segment-radius seg))
               (draw-line (vec-x a) (vec-y a) (vec-x b) (vec-y b))))))

(defmethod draw-shape ((poly poly))
  (let ((vertices (poly-transformed-vertices poly)))
    (draw-poly vertices :filled t)
    (gl:color 0 0 0)
    (gl:line-width *line-width*)
    (draw-poly vertices :filled nil)))

;;;
;;; Constraints
;;;
(defgeneric draw-constraint (constraint))

(defmethod draw-constraint ((constraint squirl::constraint))
  (let* ((position-a (body-position (constraint-body-a constraint)))
         (position-b (body-position (constraint-body-b constraint))))
    (gl:point-size 5)
    (gl:with-primitives :points
      (gl:vertex (vec-x position-a) (vec-y position-a))
      (gl:vertex (vec-x position-b) (vec-y position-b)))))

(defmethod draw-constraint ((joint pivot-joint))
  (let* ((body-a (constraint-body-a joint))
         (body-b (constraint-body-b joint))
         (point-a (vec+ (body-position body-a)
                        (vec-rotate (squirl::pivot-joint-anchor1 joint)
                                    (body-rotation body-a))))
         (point-b (vec+ (body-position body-b)
                        (vec-rotate (squirl::pivot-joint-anchor2 joint)
                                    (body-rotation body-b)))))
    (gl:point-size 10)
    (gl:with-primitives :points
      (gl:vertex (vec-x point-a) (vec-y point-a))
      (gl:vertex (vec-x point-b) (vec-y point-b)))))

(defmethod draw-constraint ((joint pin-joint))
  (let* ((body-a (constraint-body-a joint))
         (body-b (constraint-body-b joint))
         (point-a (vec+ (body-position body-a)
                        (vec-rotate (squirl::pin-joint-anchor1 joint)
                                    (body-rotation body-a))))
         (point-b (vec+ (body-position body-b)
                        (vec-rotate (squirl::pin-joint-anchor2 joint)
                                    (body-rotation body-b)))))
    (gl:point-size 5)
    (gl:with-primitives :points
      (gl:vertex (vec-x point-a) (vec-y point-a))
      (gl:vertex (vec-x point-b) (vec-y point-b)))
    (gl:with-primitives :lines
      (gl:vertex (vec-x point-a) (vec-y point-a))
      (gl:vertex (vec-x point-b) (vec-y point-b)))))

(defmethod draw-constraint ((joint slide-joint))
  (let* ((body-a (constraint-body-a joint))
         (body-b (constraint-body-b joint))
         (point-a (vec+ (body-position body-a)
                        (vec-rotate (squirl::slide-joint-anchor1 joint)
                                    (body-rotation body-a))))
         (point-b (vec+ (body-position body-b)
                        (vec-rotate (squirl::slide-joint-anchor2 joint)
                                    (body-rotation body-b)))))
    (gl:point-size 5)
    (gl:with-primitives :points
      (gl:vertex (vec-x point-a) (vec-y point-a))
      (gl:vertex (vec-x point-b) (vec-y point-b)))
    (gl:with-primitives :lines
      (gl:vertex (vec-x point-a) (vec-y point-a))
      (gl:vertex (vec-x point-b) (vec-y point-b)))))

(defmethod draw-constraint ((joint breakable-joint))
  (draw-constraint (squirl::breakable-joint-delegate joint)))

(defmethod draw-constraint ((spring damped-spring))
  (let* ((body-a (constraint-body-a spring))
         (body-b (constraint-body-b spring))
         (point-a (vec+ (body-position body-a)
                        (vec-rotate (squirl::damped-spring-anchor1 spring)
                                    (body-rotation body-a))))
         (point-b (vec+ (body-position body-b)
                        (vec-rotate (squirl::damped-spring-anchor2 spring)
                                    (body-rotation body-b))))
         (delta (vec- point-a point-b))
         (ziggy (floor (/ (spring-stiffness spring) 10)))
         (width (/ ziggy 3)))
    (gl:line-width 2)
    (gl:with-pushed-matrix
      (gl:translate (vec-x point-a) (vec-y point-a) 0)
      (gl:rotate (+ 90 (* (vec->angle delta) (/ 180 pi))) 0 0 1)
      (gl:scale width (/ (vec-length delta) ziggy) 1)
      (gl:with-primitive :line-strip
        (loop for i from 0 below ziggy do
             (gl:vertex -1 i)
             (gl:vertex 1 (+ i 1/2)))
        (gl:vertex 0 ziggy)))))

;;;
;;; Drawing the world.
;;;
(defun set-body-point (body)
  (gl:vertex (vec-x (body-position body))
             (vec-y (body-position body))))

(defun set-collision-points (arbiter)
  (loop for contact in (squirl::arbiter-contacts arbiter)
     for contact-position = (squirl::contact-point contact)
     do (gl:vertex (vec-x contact-position) (vec-y contact-position))))

(defun draw-world (world &key (line-thickness 1)
                   draw-bb-p (draw-shapes-p t) (body-point-size 2) (collision-point-size 2))
  (gl:line-width line-thickness)
  (when draw-shapes-p
    (map-world #'draw-body world))
  (when draw-bb-p
    (map-world #'draw-bbox world))
  ;; draw constraints
  (gl:color 0.5 1 0.5)
  (map nil #'draw-constraint (world-constraints world))
  (when (> body-point-size 0)
    (gl:point-size body-point-size)
    (gl:with-primitives :points
      (apply #'gl:color *line-color*)
      (map-world #'set-body-point world)))
  (when (> collision-point-size 0)
    (gl:point-size collision-point-size)
    (gl:with-primitives :points
      (apply #'gl:color *collision-color*)
      (map nil #'set-collision-points (squirl::world-arbiters world)))))
