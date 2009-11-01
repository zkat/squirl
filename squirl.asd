;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem squirl
  :version "0.1 (unreleased)"
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :licence "MIT"
  :components
  ((:module "src"
            :components
            ((:file "arbiter"
                    :depends-on ("vec"))
             (:file "body"
                    :depends-on ("vec"))
             (:file "bounding-box"
                    :depends-on ("vec"))
             (:file "chipmunk"
                    :depends-on ("vec"))
             (:file "package")
             (:file "space"
                    :depends-on ("vec"))
             (:file "vec"
                    :depends-on ("package"))))))

