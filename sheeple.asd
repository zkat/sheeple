;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; indent-tabs-mode: nil -*-

(asdf:defsystem sheeple
  :version "2.0"
  :description "Cheeky prototypes for Common Lisp"
  :author "Kat Marchan <zkat at sykosomatic-dot-org>"
  :licence "MIT"
  :depends-on ()
  :serial t
  :components
  ((:module src
            :serial t
            :components
            ((:file "packages")
             (:file "sheeple-garbage")
             (:file "conditions")
             (:file "utils")
             (:file "sheeple")
             (:file "properties")
             (:file "builtins")
             (:file "parse-lambda-list")
             ;; (:file "messages")
             ;; (:file "reply-definition")
             ;; (:file "reply-dispatch")
             ;; (:file "bootstrap")
             ;; (:file "post-boot")
             ;; (:file "glue") ;this file should go away.
             ))))

(asdf:defsystem sheeple-tests
  :version "Baahh"
  :description "Unit tests for Sheeple"
  :author "Kat Marchan <zkat at sykosomatic-dot-org>"
  :licence "MIT"
  :depends-on (:sheeple :fiveam)
  :serial t
  :components
  ((:module tests
            :serial t
            :components
            ((:file "setup-tests")
             (:file "utils")
             (:file "sheeple")
             (:file "properties")
             (:file "builtins")
             (:file "parse-lambda-list")
             ;; (:file "messages")
             ;; (:file "reply-definition")
             ;; (:file "reply-dispatch")
             ;; (:file "bootstrap")
             ;; (:file "post-boot")
             ))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :sheeple))))
  (format t "~&~%*******************~%~
                 ** Loading tests **~%~
                 *******************~%")
  (asdf:oos 'asdf:load-op 'sheeple-tests)
  (asdf:oos 'asdf:test-op 'sheeple-tests))
