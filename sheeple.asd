;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem sheeple
  :version "3.0.2"
  :description "Cheeky prototypes for Common Lisp"
  :author "Josh Marchan <sykopomp at sykosomatic-dot-org>"
  :licence "MIT"
  :depends-on ()
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "packages")
             (:file "sheeple-garbage")
             (:file "functions")
             (:file "utils")
             (:file "conditions")
             (:file "objects")
             (:file "properties")
             (:file "builtins")
             (:file "lambda-lists")
             (:file "messages")
             (:file "reply-definition")
             (:file "reply-dispatch")
             (:file "bootstrap")
             (:file "post-boot")
             (:module mop
                      :serial t
                      :components
                      ((:file "properties")))))))

(asdf:defsystem sheeple-tests
  :version "Baahh"
  :description "Unit tests for Sheeple"
  :author "Josh Marchan <sykopomp at sykosomatic-dot-org>"
  :licence "MIT"
  :depends-on (:sheeple :fiveam)
  :serial t
  :components
  ((:module tests
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
