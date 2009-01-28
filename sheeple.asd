(asdf:defsystem sheeple
  :version "Baahh"
  :description "Clone-crazy hackery tool"
  :maintainer "Kat <zkat@Dagon>"
  :author "Kat <zkat@Dagon>"
  :licence "MIT"
  :depends-on ()
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "utils")
	     (:file "sheeple")
	     (:file "clone")
	     (:file "compatibility")
	     (:file "buzzwords")))))

