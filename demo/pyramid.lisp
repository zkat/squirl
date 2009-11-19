(in-package :squirl-demo)

(defparameter *step* (/ 1 60 2))

(defclass pyramid-demo (demo)
  ((floor :accessor demo-floor))
  (:default-initargs :name "Pyramid Topple"))

;; uncomment this bit to let the pyramid demo run at maximum speed.
#+nil(defmethod update-demo ((demo pyramid-demo) dt)
  (declare (ignore dt))
  (world-step (world demo) (physics-timestep demo)))

(defmethod init-demo ((demo pyramid-demo))
  (reset-shape-id-counter)
  (setf (world demo) (make-world :iterations 20
                            :gravity (vec 0 -300)))
  (resize-world-active-hash (world demo) 40.0 2999)
  (resize-world-static-hash (world demo) 40.0 999)
  (setf (demo-floor demo) (make-body :actor :not-grabbable
                                     :shapes (list (make-segment (vec -600 -240)
                                                                 (vec 600 -240)
                                                                 :restitution 1
                                                                 :friction 1))))
  (world-add-body (world demo) (demo-floor demo))
  (let ((friction 0.6)
        (verts (list (vec -3 -20)
                     (vec -3 20)
                     (vec 3 20)
                     (vec 3 -20))))
    (loop
       with n = 9
       for i from 1 to n
       for offset = (vec (- (/ (* i 60) 2))
                         (* (- n i) 52))
       do (loop
             for j from 0 below i do
               (mapc (lambda (body)
                       (attach-shape (make-poly verts :friction friction)
                                     body)
                       (world-add-body (world demo) body))
                     (nconc
                      (list (make-body
                             :mass 1
                             :position (vec+ (vec (* j 60) -220)
                                             offset))
                            (make-body
                             :mass 1
                             :position (vec+ (vec (* j 60) -197)
                                             offset)
                             :angle (/ pi 2)))
                      (unless (= j (1- i))
                        (list (make-body
                               :mass 1
                               :position (vec+ (vec (+ (* j 60) 30)
                                                    -191)
                                               offset)
                               :angle (/ pi 2)))))))
         (mapc (lambda (body)
                 (attach-shape (make-poly verts :friction friction)
                               body)
                 (world-add-body (world demo) body))
               (list (make-body
                      :mass 1
                      :position (vec+ (vec -17
                                           -174)
                                      offset))
                     (make-body
                      :mass 1
                      :position (vec+ (vec (+ (* (1- i) 60) 17)
                                           -174)
                                      offset))))))
  (world demo))

(pushnew 'pyramid-demo *demos*)
