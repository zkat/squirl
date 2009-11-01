;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(in-package :squirl)

(defvar *contact-persistence*)

(defstruct space
  ;; User definable slots
  ;; --------------------

  ;; Number of iterations to use in the impulse solver to solve contacts.
  iterations

  ;; Number of iterations to use in the impulse solver to solve elastic collisions.
  elastic-iterations

  ;; Default gravity to supply when integrating rigid body motions.
  gravity

  ;; Default damping to supply when integrating rigid body motions.
  damping

  ;; Internal slots
  ;; --------------

  ;; Time stamp, incremented on every call to SPACE-STEP
  stamp

  ;; Static and active shape spatial hashes
  ;; TODO - Are spatial hashes just hash tables? I haven't looked yet... -zkat
  static-shapes active-shapes

  ;; List of bodies in the system.
  bodies

  ;; List of active arbiters for the impulse solver.
  arbiters

  ;; Persistent contact set.
  contact-set

  ;; List of constraints in the system.
  constraints

  ;; Set of collision-pair functions.
  collision-function-set

  ;; Default collision pair function.
  default-collision-pair-function ; Yeah. That's right.
  )

