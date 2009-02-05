(asdf:defsystem sheeple
  :version "Baahh"
  :description "Clone-crazy hackery tool"
  :maintainer "Josh <sykopomp@Dagon>"
  :author "Josh <sykopomp@Dagon>"
  :licence "MIT"
  :depends-on (:trivial-garbage)
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

