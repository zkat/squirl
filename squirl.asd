;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem squirl
  :version "0.1 (unreleased)"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :components
  ((:module "src"
            :components
            ((:file "arbiter"
                    :depends-on ("vec" "constraints"))
             (:file "body"
                    :depends-on ("vec"))
             (:file "bounding-box"
                    :depends-on ("vec"))
             (:file "collision"
                    :depends-on ("shape" "poly-shape"))
             (:file "hash-set"
                    :depends-on ("utils"))
             (:file "package")
             (:file "shape"
                    :depends-on ("vec" "bounding-box" "body"))
             (:file "poly-shape"
                    :depends-on ("shape"))
             (:file "squirl"
                    :depends-on ("vec"))
             (:file "utils"
                    :depends-on ("package"))
             (:file "vec"
                    :depends-on ("utils"))
             (:file "world"
                    :depends-on ("vec"))
             (:file "world-hash"
                    :depends-on ("hash-set" "vec"))
             (:module "constraints"
                      :components
                      ((:file "breakable-joint"
                              :depends-on ("constraints"))
                       (:file "constraints"
                              :depends-on ("util"))
                       (:file "damped-rotary-spring"
                              :depends-on ("spring"))
                       (:file "damped-spring"
                              :depends-on ("spring"))
                       (:file "gear-joint"
                              :depends-on ("constraints"))
                       (:file "groove-joint"
                              :depends-on ("constraints"))
                       (:file "pin-joint"
                              :depends-on ("constraints"))
                       (:file "pivot-joint"
                              :depends-on ("constraints"))
                       (:file "ratchet-joint"
                              :depends-on ("constraints"))
                       (:file "rotary-limit-joint"
                              :depends-on ("constraints"))
                       (:file "simple-motor"
                              :depends-on ("constraints"))
                       (:file "slide-joint"
                              :depends-on ("constraints"))
                       (:file "spring"
                              :depends-on ("constraints"))
                       (:file "util"))                      
                      :depends-on ("shape" "poly-shape" "world" "collision"))))))

