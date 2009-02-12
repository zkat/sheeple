(asdf:defsystem sheeplette
  :version "Baahh"
  :description "Closette:CLOS::Sheeplette:Sheeple"
  :author "Josh Marchan <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on ()
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "trivial-garbage")))
   (:module sheeplette
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "utils")
	     (:file "property-access")
	     (:file "sheep-creation")
	     (:file "buzzwords")
	     (:file "message-generation")
	     (:file "message-dispatch")
	     (:file "bootstrap")
	     (:file "post-boot")))))

