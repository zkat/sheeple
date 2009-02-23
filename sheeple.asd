(asdf:defsystem sheeple
  :version "0.1"
  :description "Cheeky prototypes for Common Lisp"
  :author "Kat Marchan <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on ()
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "trivial-garbage")
	     (:file "packages")
	     (:file "utils")
	     (:file "properties")
	     (:file "sheeple")
	     (:file "clone")
	     (:file "wolves")
	     (:file "buzzwords")
	     (:file "message-generation")
	     (:file "message-dispatch")
	     (:file "bootstrap")
	     (:file "glue")
	     (:file "post-boot")))))


