;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem sheeple
  :version "3.0.2"
  :description "Cheeky prototypes for Common Lisp"
  :author "Kat Marchan <zkat at sykosomatic-dot-org>"
  :licence "MIT"
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "packages")
             (:file "sheeple-garbage")
             (:file "functions")
             (:file "utils")
             (:file "ports")
             (:file "conditions")
             (:file "backend")
             (:file "objects")
             (:file "properties")
             (:file "builtins")
             (:file "lambda-lists")
             (:file "messages")
             (:file "reply-definition")
             (:file "reply-dispatch")
             (:file "init")
             (:file "boxed")
             (:file "post-boot")
             (:module "mop"
                      :serial t
                      :components
                      ((:file "objects")
                       (:file "properties")))))))

(asdf:defsystem sheeple-tests
  :version "Baahh"
  :description "Unit tests for Sheeple"
  :author "Kat Marchan <zkat at sykosomatic-dot-org>"
  :licence "MIT"
  :depends-on (:sheeple :Eos)
  :serial t
  :components
  ((:module "tests"
            :serial t
            :components
            ((:file "setup-tests")
             (:file "utils")
             (:file "objects")
             (:file "properties")
             (:file "builtins")
             (:file "lambda-lists")
             (:file "messages")
             (:file "reply-definition")
             (:file "reply-dispatch")
             (:file "bootstrap")))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :sheeple))))
  (format t "~&~%*******************~%~
                 ** Loading tests **~%~
                 *******************~%")
  (asdf:oos 'asdf:load-op 'sheeple-tests)
  (asdf:oos 'asdf:test-op 'sheeple-tests))
