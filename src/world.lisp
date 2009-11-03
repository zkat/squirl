;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defvar *contact-persistence* 3)

(defstruct world
  ;; User definable slots
  iterations ; Number of iterations to use in the impulse solver to solve contacts.
  elastic-iterations ; Number of iterations to use in the impulse solver to solve elastic collisions.
  gravity ; Default gravity to supply when integrating rigid body motions.
  damping ; Default damping to supply when integrating rigid body motions.

  ;; Internal slots
  stamp          ; Time stamp, incremented on every call to WORLD-STEP
  static-shapes active-shapes ; Static and active shape spatial hashes
  bodies                      ; List of bodies in the system.
  arbiters           ; List of active arbiters for the impulse solver.
  contact-set        ; Persistent contact set.
  constraints        ; List of constraints in the system.
  collision-function-set            ; Set of collision-pair functions.
  default-collision-pair-function   ; Default collision pair function.
  )

