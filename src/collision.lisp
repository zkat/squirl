;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(define-constant +collision-slop+ 0.1
  "Amount of allowed penetration.  Used to reduce vibrating contacts.")

;;;
;;; Collision resolution functions
;;;
(defun circle-to-circle-query (p1 p2 r1 r2)
  (let* ((delta (vec- p2 p1))
         (mindist (+ r1 r2))
         (distsq (vec-length-sq delta)))
   (when (< distsq (* mindist mindist))
     (let* ((dist (sqrt distsq)))
       (make-contact (vec+ p1 (vec* delta
                                    (+ 0.5 (maybe/ (- r1 (/ mindist 2))
                                                   dist))))
                     (vec* delta (maybe/ 1 dist)) ; Same as (vec-normalize delta)
                     (- dist mindist)
                     0)))))

(defun circle-to-segment (circle segment)
  (let* ((radius-sum (+ (circle-radius circle) (segment-radius segment)))
         (normal-distance (- (vec. (segment-trans-normal segment)
                                   (circle-transformed-center circle))
                             (vec. (segment-trans-a segment)
                                   (segment-trans-normal segment))))
         (distance (- (abs normal-distance) radius-sum)))
    (unless (plusp distance)
      (let ((tangent-distance (- (vec-cross (segment-trans-normal segment)
                                            (circle-transformed-center circle))))
            (tangent-distance-min (- (vec-cross (segment-trans-normal segment)
                                                (segment-trans-a segment))))
            (tangent-distance-max (- (vec-cross (segment-trans-normal segment)
                                                (segment-trans-b segment)))))
        (cond
          ((< tangent-distance tangent-distance-min)
           (when (>= tangent-distance (- tangent-distance-min radius-sum))
             (circle-to-circle-query (circle-transformed-center circle)
                                     (segment-trans-a segment)
                                     (circle-radius circle)
                                     (segment-radius segment))))
          ((< tangent-distance tangent-distance-max)
           (let ((normal (if (minusp normal-distance)
                             (segment-trans-normal segment)
                             (vec- (segment-trans-normal segment)))))
             (make-contact (vec+ (circle-transformed-center circle)
                                 (vec* normal (+ (circle-radius circle)
                                                 (/ distance 2))))
                           normal distance 0)))
          ((< tangent-distance (+ tangent-distance-max radius-sum))
           (circle-to-circle-query (circle-transformed-center circle)
                                   (segment-trans-b segment)
                                   (circle-radius circle)
                                   (segment-radius segment)))
          (t nil))))))

(defun find-min-separating-axis (poly1 poly2)
  (loop with msa with min-distance
     for axis across (poly-transformed-axes poly2)
     for distance = (poly-value-on-axis poly1 (poly-axis-normal axis) (poly-axis-distance axis))
     if (plusp distance)
     return nil
     else if (or (null min-distance) (> distance min-distance))
     do (setf min-distance distance
              msa axis)
     finally (return (values msa min-distance))))

(defun find-vertices (poly1 poly2 normal distance &aux contacts)
  "Add contacts for penetrating vertices"
  (loop
     for i from 0
     for vertex across (poly-vertices poly1)
     when (partial-poly-contains-vertex-p poly2 vertex (vec-neg normal))
     do (push (make-contact vertex normal distance (hash-pair (shape-id poly1) i)) contacts))
  (loop
     for i from 0
     for vertex across (poly-vertices poly2)
     when (partial-poly-contains-vertex-p poly1 vertex (vec-neg normal))
     do (push (make-contact vertex normal distance (hash-pair (shape-id poly2) i)) contacts))
  contacts)

(defun segment-value-on-axis (segment normal distance)
  (- (min (- (vec. normal (segment-trans-a segment)) (segment-radius segment))
          (- (vec. normal (segment-trans-b segment)) (segment-radius segment)))
     distance))

(defun find-points-behind-segment (segment poly p-dist coefficient)
  "Identify vertices that have penetrated the segment."
  (let ((dta (vec-cross (segment-trans-normal segment)
                   (segment-trans-a segment)))
        (dtb (vec-cross (segment-trans-normal segment)
                   (segment-trans-b segment)))
        (normal (vec* (segment-trans-normal segment)
                      coefficient)))
    (loop
       with contacts
       for i from 0
       for vertex across (poly-vertices poly)
       when (< (vec. vertex normal)
               (+ (* (vec. (segment-trans-normal segment)
                           (segment-trans-a segment))
                     coefficient)
                  (segment-radius segment)))
       do (let ((dt (vec-cross (segment-trans-normal segment)
                          vertex)))
            (when (and (>= dta dt)
                       (>= dt dtb))
              (push (make-contact vertex normal p-dist (hash-pair (shape-id poly) i))
                    contacts)))
       finally (return contacts))))

(defun segment-to-poly (segment poly)
  (let* (contacts
         (axes (poly-transformed-axes poly))
         (segD (vec. (segment-trans-normal segment)
                     (segment-trans-a segment)))
         (min-norm (- (poly-value-on-axis poly
                                          (segment-trans-normal segment)
                                          segD)
                     (segment-radius segment)))
         (min-neg (- (poly-value-on-axis poly
                                         (vec-neg (segment-trans-normal segment))
                                         (- segD))
                     (segment-radius segment))))
    (unless (or (> min-neg 0) (> min-norm 0))
      (let ((min-i 0)
            (poly-min (segment-value-on-axis segment
                                             (poly-axis-normal (svref axes 0))
                                             (poly-axis-distance (svref axes 0)))))
        (unless (or (plusp poly-min)
                    (not (loop
                            for i from 0
                            for axis across axes
                            for distance = (segment-value-on-axis segment
                                                                  (poly-axis-normal axis)
                                                                  (poly-axis-distance axis))
                            when (> distance 0) return nil
                            when (> distance poly-min)
                            do (setf poly-min distance
                                     min-i i)
                            finally (return t))))
          (let* ((poly-normal (vec-neg (poly-axis-normal (svref axes min-i))))
                 (vertex-a (vec+ (segment-trans-a segment)
                                 (vec* poly-normal (segment-radius segment))))
                 (vertex-b (vec+ (segment-trans-b segment)
                                 (vec* poly-normal (segment-radius segment)))))
            (loop
               for i from 0
               for vertex in (list vertex-a vertex-b)
               when (poly-contains-vertex-p poly vertex)
               do (push (make-contact vertex-a poly-normal poly-min
                                      (hash-pair (shape-id segment) i))
                        contacts))
            ;; "Floating point precision problems here.
            ;;  This will have to do for now."
            (decf poly-min +collision-slop+)
            (when (or (>= min-norm poly-min)
                      (>= min-neg poly-min))
              (if (> min-norm min-neg)
                  (setf contacts
                        (append contacts
                                (find-points-behind-segment segment
                                                            poly
                                                            min-norm
                                                            1)))
                  (setf contacts
                        (append contacts
                                (find-points-behind-segment segment
                                                            poly
                                                            min-neg
                                                            (- 1))))))
            ;; If no other collision points were found, try colliding endpoints.
            (unless contacts
              (loop
                 with seg-t-a = (segment-trans-a segment)
                 with seg-t-b = (segment-trans-b segment)
                 with poly-t-v = (poly-transformed-vertices poly)
                 with vert-a = (svref poly-t-v min-i)
                 with vert-b = (svref poly-t-v
                                      (rem (1+ min-i)
                                           (length poly-t-v)))
                 for point in (list seg-t-a seg-t-b
                                    seg-t-a seg-t-b)
                 for vertex in (list vert-a vert-a
                                     vert-b vert-b)
                 for collision = (circle-to-circle-query point vertex
                                                         (segment-radius segment) 0)
                 when collision return (push collision contacts)))))))))

(defun circle-to-poly (circle poly)
  (let* ((axes (poly-transformed-axes poly))
         (min-i 0)
         (min (- (vec. (poly-axis-normal (svref axes 0))
                       (circle-transformed-center circle))
                 (poly-axis-distance (svref axes 0))
                 (circle-radius circle))))
    (when (loop
             for i from 0
             for axis across axes
             for distance = (- (vec. (poly-axis-normal axis)
                                     (circle-transformed-center circle))
                               (poly-axis-distance axis)
                               (circle-radius circle))
             when (> distance 0) return nil
             when (> distance min)
             do (setf min distance
                      min-i i)
             finally (return t))
      (let* ((normal (poly-axis-normal (svref axes min-i)))
             (a (svref (poly-transformed-vertices poly) min-i))
             (b (svref (poly-transformed-vertices poly)
                       (rem (1+ min-i) (length (poly-transformed-vertices poly)))))
             (dta (vec-cross normal a))
             (dtb (vec-cross normal b))
             (dt (vec-cross normal (circle-transformed-center circle))))
        (cond
          ((< dt dtb)
           (circle-to-circle-query (circle-transformed-center circle)
                                   b
                                   (circle-radius circle)
                                   0))
          ((< dt dta)
           (list (make-contact (vec- (circle-transformed-center circle)
                                     (vec* normal
                                           (+ (circle-radius circle)
                                              (/ min 2))))
                               (vec-neg normal)
                               min
                               0)))
          (t
           (circle-to-circle-query (circle-transformed-center circle)
                                   a
                                   (circle-radius circle)
                                   0)))))))
(defun poly-to-poly (poly1 poly2)
  (multiple-value-bind (msa1 min1) (find-min-separating-axis poly2 poly1)
    (multiple-value-bind (msa2 min2) (find-min-separating-axis poly1 poly2)
      (cond ((not (and msa1 msa2))
             nil)
            ((> min1 min2)
             (find-vertices poly1 poly2 (poly-axis-normal msa1) min1))
            (t
             (find-vertices poly1 poly2 (vec-neg (poly-axis-normal msa2)) min2))))))

;;;
;;; Generic function
;;;
(defgeneric collide-shapes (a b)
  (:documentation "Collide shapes A and B together!")
  ;; Note that we don't handle segment-to-segment (yet?)
  (:method ((shape-1 circle) (shape-2 circle))
    (circle-to-circle-query (circle-transformed-center shape-1)
                            (circle-transformed-center shape-2)
                            (circle-radius shape-1)
                            (circle-radius shape-2)))
  (:method ((segment segment) (circle circle))
    (circle-to-segment circle segment))
  (:method ((circle circle) (segment segment))
    (circle-to-segment circle segment))
  (:method ((segment segment) (poly poly))
    (segment-to-poly segment poly))
  (:method ((poly poly) (segment segment))
    (segment-to-poly segment poly))
  (:method ((circle circle) (poly poly))
    (circle-to-poly circle poly))
  (:method ((poly poly) (circle circle))
    (circle-to-poly circle poly))
  (:method ((poly1 poly) (poly2 poly))
    (poly-to-poly poly1 poly2)))

