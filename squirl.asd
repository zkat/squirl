;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem squirl
  :version "0.1 (unreleased)"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :components
  ((:module "src"
            :components
            ((:file "body"
                    :depends-on ("vect"))
             (:file "bounding-box"
                    :depends-on ("vect"))
             (:file "chipmunk"
                    :depends-on ("vect"))
             (:file "package")
             (:file "vect"
                    :depends-on ("package"))))))
