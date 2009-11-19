(in-package :squirl-demo)

(defclass pyramid-stack (demo)
  ((static-body :accessor static-body))
  (:default-initargs :name "Pyramid Stack"))

(defmethod init-demo ((demo pyramid-stack))
  (reset-shape-id-counter)
  (setf (world demo) (make-world :iterations 20 :gravity (vec 0 -250)))
  ;; Create segments around the edge of the screen
  (setf (static-body demo)
        (world-add-body (world demo)
                        (make-body :actor :not-grabbable
                         :shapes (list (make-segment (vec -320 -240) (vec -320 240)
                                                     :restitution 1 :friction 1)
                                       (make-segment (vec 320 -240) (vec 320 240)
                                                     :restitution 1 :friction 1)
                                       (make-segment (vec -320 -240) (vec 320 -240)
                                                     :restitution 1 :friction 1)))))
  (resize-world-active-hash (world demo) 40 1000)
  (resize-world-static-hash (world demo) 40 1000)
  (let ((verts (list (vec -15 -15)
                     (vec -15 15)
                     (vec 15 15)
                     (vec 15 -15))))
    ;; add lots of boxen
    (loop for i below 14 do
         (loop for j upto i do
              (world-add-body
               (world demo)
               (make-body :mass 1 :position (vec (- (* j 32) (* i 16))
                                                 (- 300 (* i 32)))
                          :shapes (list (make-poly verts :friction 0.8))))))
    ;; add ball to make things interesting
    (let ((radius 15))
      (world-add-body (world demo) (make-body :mass 10 :position (vec 0 (+ -240 radius))
                                              :shapes (list (make-circle radius :friction 0.9)))))
    (world demo)))

(pushnew 'pyramid-stack *demos*)