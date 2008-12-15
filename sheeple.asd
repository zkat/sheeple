(asdf:defsystem sheeple
  :version "Baahh"
  :description "Clone-crazy hackery tool"
  :maintainer "Josh <sykopomp@Dagon>"
  :author "Josh <sykopomp@Dagon>"
  :licence "BSD-style"
  :depends-on ()
  :components 
  :serial t
  ((:module src
	    :components
	    :serial t
	    ((:file "packages")
	     (:file "sheeple")))))

