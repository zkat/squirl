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

(defmethod draw-shape ((seg segment))
  (let ((a (segment-trans-a seg))
        (b (segment-trans-b seg)))
    (gl:line-width (squirl::segment-radius seg))
    (draw-line (vec-x a) (vec-y a) (vec-x b) (vec-y b))))
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

(defparameter *spring-vertices* '((0 . 0)
                                  (0.2 . 0)
                                  (0.25 . 3)
                                  (0.3 . -6.0)
                                  (0.35 . 6)
                                  (0.4 . -6)
                                  (0.45 . 6)
                                  (0.5 . -6)
                                  (0.55 . 6)
                                  (0.60 . -6)
                                  (0.65 . 6)
                                  (0.7 . -3)
                                  (0.75 . 6)
                                  (0.8 . 0)
                                  (1 . 0)))

(defmethod draw-constraint ((spring damped-spring))
  (let* ((body-a (constraint-body-a spring))
         (body-b (constraint-body-b spring))
         (point-a (vec+ (body-position body-a)
                        (vec-rotate (squirl::damped-spring-anchor1 spring)
                                    (body-rotation body-a))))
         (point-b (vec+ (body-position body-b)
                        (vec-rotate (squirl::damped-spring-anchor2 spring)
                                    (body-rotation body-b))))
         (delta (vec- point-b point-a)))
    (gl:point-size 5)
    (gl:with-primitives :points
      (gl:vertex (vec-x point-a) (vec-y point-a))
      (gl:vertex (vec-x point-b) (vec-y point-b)))
    (gl:with-pushed-matrix
      (let ((x (vec-x point-a))
            (y (vec-y point-a))
            (cos (vec-x delta))
            (sin (vec-y delta))
            (s (/ (vec-length delta))))
        (gl:mult-matrix (make-array '(4 4)
                                    :initial-contents
                                    (list (list cos sin 0 0)
                                          (list (* (- sin) s) (* cos s) 0 0)
                                          (list 0 0 1 1)
                                          (list x y 0 1))))
        (gl:line-width *line-width*)
        (gl:with-primitives :line-strip
          (loop for (x . y) in *spring-vertices*
             do (gl:vertex x y)))))))

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
