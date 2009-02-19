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
	     (:file "property-access")
	     (:file "sheep-creation")
	     (:file "wolves")
	     (:file "buzzwords")
	     (:file "message-generation")
	     (:file "message-dispatch")
	     (:file "bootstrap")
	     (:file "post-boot")
	     (:file "glue")))))


