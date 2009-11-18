(in-package :squirl-demo)

(defclass tumble-demo (demo)
  ((box :initarg :box :accessor demo-box))
  (:default-initargs :name "Tumbling along."))

(defmethod update-demo ((demo tumble-demo) dt)
  (incf (accumulator demo) (if (> dt *dt-threshold*) *dt-threshold* dt))
  (loop while (>= (accumulator demo) (physics-timestep demo))
     do (world-step (world demo) (physics-timestep demo))
     ;; manually update the position of the box so that it rotates.
       (body-update-position (demo-box demo) (physics-timestep demo))
     ;; Because the box was added as static and we moved it, we need to manually
     ;; rehash the static spatial hash
       (squirl::rehash-world-static-data (world demo))
     (decf (accumulator demo) (physics-timestep demo))))

(defun build-demo-box ()
  (let ((a (vec -200 -200))
        (b (vec -200 200))
        (c (vec 200 200))
        (d (vec 200 -200)))
    (make-body :actor :not-grabbable
               :angular-velocity 0.4
               :shapes
               (list (make-segment a b :radius 0 :friction 1 :restitution 1)
                     (make-segment b c :radius 0 :friction 1 :restitution 1)
                     (make-segment c d :radius 0 :friction 1 :restitution 1)
                     (make-segment d a :radius 0 :friction 1 :restitution 1)))))

(defmethod init-demo ((demo tumble-demo))
  (reset-shape-id-counter)
  (setf (world demo) (make-world :gravity (vec 0 -600)))
  (resize-world-active-hash (world demo) 30 999)
  (resize-world-static-hash (world demo) 200 99)
  (setf (demo-box demo) (world-add-body (world demo) (build-demo-box)))
  ;; add the bricks
  (let* ((verts (list (vec -30 -15)
                      (vec -30 15)
                      (vec 30 15)
                      (vec 30 -15)))
         (inertia (moment-for-poly 1 verts)))
    (dotimes (i 3)
      (dotimes (j 7)
        (world-add-body (world demo)
                        (make-body :mass 1 :inertia inertia :position (vec (- (* i 60) 150)
                                                                           (- (* j 30) 150))
                                   :shapes (list (make-poly verts :friction 0.7)))))))
  (world demo))

(pushnew 'tumble-demo *demos*)
