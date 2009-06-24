(asdf:defsystem sheeple
  :version "0.1"
  :description "Cheeky prototypes for Common Lisp"
  :author "Kat Marchan <zkat at sykosomatic-dot-org>"
  :licence "MIT"
  :depends-on ()
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "trivial-garbage")
	     (:file "packages")
	     (:file "conditions")
	     (:file "utils")
	     (:file "sheeple")
	     (:file "properties")
	     (:file "clone")
	     (:file "wolves")
	     (:file "parse-lambda-list")
	     (:file "buzzwords")
	     (:file "message-generation")
	     (:file "message-dispatch")
	     (:file "bootstrap")
	     (:file "post-boot")
	     (:file "glue")))))

#+nil(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :sheeple))))
  (format t "~&Loading sheeple-tests...~%")
  (asdf:oos 'asdf:load-op 'sheeple-tests)
  (format t "~&Starting test...~%")
  (asdf:oos 'asdf:test-op 'sheeple-tests))
