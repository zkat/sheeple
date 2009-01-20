(asdf:defsystem sheeple
  :version "Baahh"
  :description "Clone-crazy hackery tool, unit tests."
  :maintainer "Josh <sykopomp@Dagon>"
  :author "Josh <sykopomp@Dagon>"
  :licence "MIT"
  :depends-on (:sheeple :fiveam)
  :serial t
  :components 
  ((:module tests
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "sheeple")))))