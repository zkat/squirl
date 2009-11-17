(in-package :squirl-demo)

;;;
;;; Configuration
;;;
(defparameter *line-color* '(0 0 0 1))
(defparameter *collision-color* '(1 0 0 1))
(defparameter *body-color* '(0 0 1 1))
(defparameter *shapes-filled-p* t)

;;;
;;; Primitives
;;;
(defun draw-circle (x y radius &key (resolution 10))
  (let* ((theta (* 2 (/ pi resolution)))
         (tangential-factor (tan theta))
         (radial-factor (- 1 (cos theta))))
    (gl:with-primitives (if *shapes-filled-p* :triangle-fan :line-loop)
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

;;;
;;; Bodies and shapes
;;;
(defun draw-body (body)
  (map nil #'draw-shape (body-shapes body)))

(defun draw-bbox (body)
  (map nil #'draw-shape-bbox (body-shapes body)))

(defun draw-shape-bbox (shape)
  (let ((bbox (squirl::shape-bbox shape)))
    (gl:with-primitives :line-loop
      (gl:vertex (squirl::bbox-left bbox) (squirl::bbox-bottom bbox))
      (gl:vertex (squirl::bbox-left bbox) (squirl::bbox-top bbox))
      (gl:vertex (squirl::bbox-right bbox) (squirl::bbox-top bbox))
      (gl:vertex (squirl::bbox-right bbox) (squirl::bbox-bottom bbox)))))

(defgeneric draw-shape (shape))
(defmethod draw-shape ((circle circle))
  (apply #'gl:color *body-color*)
  (let* ((body (shape-body circle))
         (center (circle-transformed-center circle))
         (x (vec-x center))
         (y (vec-y center))
         (radius (circle-radius circle))
         (edge (vec* (body-rotation body) radius))
         (edge-t (vec+ edge center)))
    (draw-circle x y (round radius) :resolution 30)
    (gl:color 0 0 0)
    (draw-line (vec-x edge-t) (vec-y edge-t) (vec-x center) (vec-y center))))

(defmethod draw-shape ((seg segment))
  (apply #'gl:color *line-color*)
  (let ((a (segment-trans-a seg))
        (b (segment-trans-b seg)))
    (draw-line (vec-x a) (vec-y a) (vec-x b) (vec-y b))))
(defmethod draw-shape ((poly poly))
  (apply #'gl:color '(0 1 0))
  (let ((vertices (poly-transformed-vertices poly)))
    (gl:with-primitives (if *shapes-filled-p* :polygon :lines)
      (loop for i below (length vertices)
         for a = (elt vertices i)
         for b = (elt vertices (mod (1+ i) (length vertices)))
         do (gl:vertex (vec-x a) (vec-y a))
         (gl:vertex (vec-x b) (vec-y b))))))

;;;
;;; Constraints
;;;
(defgeneric draw-constraint (constraint))

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
                                    (body-rotation body-b)))))
    (gl:point-size 5)
    (gl:with-primitives :points
      (gl:vertex (vec-x point-a) (vec-y point-a))
      (gl:vertex (vec-x point-b) (vec-y point-b)))
    (gl:with-primitives :lines
      (gl:vertex (vec-x point-a) (vec-y point-a))
      (gl:vertex (vec-x point-b) (vec-y point-b)))))

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
  (gl:line-width 1)
  (when draw-bb-p
    (gl:color 0.6 1.0 0.6)
    (map-world #'draw-bbox world))
  (gl:line-width line-thickness)
  (when draw-shapes-p
    (map-world #'draw-body world))
  ;; draw constraints
  (gl:color 0.5 1 0.5)
  (map nil #'draw-constraint (world-constraints world))
  (when body-point-size
    (gl:point-size body-point-size)
    (gl:with-primitives :points
      (apply #'gl:color *line-color*)
      (map-world #'set-body-point world)))
  (when collision-point-size
    (gl:point-size collision-point-size)
    (gl:with-primitives :points
      (apply #'gl:color *collision-color*)
      (map nil #'set-collision-points (squirl::world-arbiters world)))))
