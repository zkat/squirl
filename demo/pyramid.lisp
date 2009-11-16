(in-package :squirl-demo)

(defvar *floor*)

(defparameter *elapsed* 0)

(defparameter *step* (/ 1 60 2))

(defclass pyramid-demo (demo)
  ()
  (:default-initargs :name "Pyramid Topple"))

<<<<<<< HEAD
(defreply update ((demo =squirl-demo-pyramid=) delta &key)
  (declare (ignore delta))
  (world-step *world* *step*))
=======
(defmethod update-demo ((demo pyramid-demo) ticks)
  (declare (ignore ticks))
  (world-step *world* 1/120))
>>>>>>> Pyramid demo is now functional.

(defmethod init-demo ((demo pyramid-demo))
  (reset-shape-id-counter)
  (setf *world* (make-world :iterations 20
                            :gravity (vec 0 -300)))
  (resize-world-active-hash *world* 40.0 2999)
  (resize-world-static-hash *world* 40.0 999)
  (setf *floor* (make-body))
  (attach-shape (make-segment (vec -600 -240)
                              (vec 600 -240)
                              :radius 1d0
                              :restitution 1
                              :friction 1)
                *floor*)
  (world-add-body *world* *floor*)
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
                       (attach-shape (make-poly verts
                                                :friction friction
                                                :restitution 0)
                                     body)
                       (world-add-body *world* body))
                     (nconc
                      (list (make-body
                             :mass 1
                             :inertia (moment-for-poly 1 verts)
                             :position (vec+ (vec (* j 60) -220)
                                             offset))
                            (make-body
                             :mass 1
                             :inertia (moment-for-poly 1 verts)
                             :position (vec+ (vec (* j 60) -197)
                                             offset)
                             :angle (/ pi 2)))
                      (unless (= j (1- i))
                        (list (make-body
                               :mass 1
                               :inertia (moment-for-poly 1 verts)
                               :position (vec+ (vec (+ (* j 60) 30)
                                                    -191)
                                               offset)
                               :angle (/ pi 2)))))))
         (mapc (lambda (body)
                 (attach-shape (make-poly verts
                                          :friction friction
                                          :restitution 0)
                               body)
                 (world-add-body *world* body))
               (list (make-body
                      :mass 1
                      :inertia (moment-for-poly 1 verts)
                      :position (vec+ (vec -17
                                           -174)
                                      offset))
                     (make-body
                      :mass 1
                      :inertia (moment-for-poly 1 verts)
                      :position (vec+ (vec (+ (* (1- i) 60) 17)
                                           -174)
                                      offset))))))
  *world*)

(pushnew 'pyramid-demo *demos*)