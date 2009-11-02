;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(declaim (optimize safety debug))

(defun clamp (n min max)
  (min (max n min) max))

(defun moment-for-circle (m inner-diameter outer-diameter offset)
  "Calculate the moment of inertia for a circle. r1 and r2 are the inner and outer diameters.
A solid circle has an inner diameter of 0."
  ;; c version: return (1.0f/2.0f)*m*(inner*inner + outer*outer) + m*cpvdot(offset, offset);
  (+
   (* m 1/2 (+ (expt inner-diameter 2)
               (expt outer-diameter 2)))
   (* m (vec. offset offset))))

(defun moment-for-segment (m a b)
  "Calculate the moment of inertia for a line segment. Beveling radius not supported."
  ;; C version:
  ;; cpFloat length = cpvlength(cpvsub(b, a));
  ;; cpVect offset = cpvmult(cpvadd(a, b), 1.0f/2.0f);
  ;; return m*length*length/12.0f + m*cpvdot(offset, offset);
  (let ((length (length (vec- b a)))
        (offset (vec* (vec+ a b) 1/2)))
    (+
     (* m length (/ length 12))
     (* m (vec. offset offset)))))

(defun moment-for-poly (m num-verts verts offset)
  "Calculate the moment of inertia for a solid palygon shape."
  ;; C version:
  ;; cpVect *tVerts = (cpVect *)calloc(numVerts, sizeof(cpVect));
  ;; for(int i=0; i<numVerts; i++)
  ;;  tVerts[i] = cpvadd(verts[i], offset);

  ;; cpFloat sum1 = 0.0f;
  ;; cpFloat sum2 = 0.0f;
  ;; for(int i=0; i<numVerts; i++){
  ;;  cpVect v1 = tVerts[i];
  ;;  cpVect v2 = tVerts[(i+1)%numVerts];

  ;;  cpFloat a = cpvcross(v2, v1);
  ;;  cpFloat b = cpvdot(v1, v1) + cpvdot(v1, v2) + cpvdot(v2, v2);

  ;;  sum1 += a*b;
  ;;  sum2 += a;
  ;; }

  ;; free(tVerts);
  ;; return (m*sum1)/(6.0f*sum2);
  (let ((t-verts (make-array num-verts)))
    (loop for i below num-verts
       do (setf (svref t-verts i) (vec+ (elt verts i) offset)))
    (loop with sum1 = 0 with sum2 = 0
       for i below num-verts
       for v1 across t-verts
       for v2 = (elt t-verts (mod (1+ i) num-verts))
       for a = (vecx v1 v2)
       for b = (+ (vec. v1 v1) (vec. v1 v2) (vec. v2 v2))
       do (incf sum1 (* a b))
         (incf sum2 a)
       finally (return (/ (* m sum1)
                          (* 6 sum2))))))
