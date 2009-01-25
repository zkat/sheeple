(asdf:defsystem sheeple-test
  :version "Baahh"
  :description "Clone-crazy hackery tool, unit tests."
  :maintainer "Kat <zkat@Dagon>"
  :author "Kat <zkat@Dagon>"
  :licence "MIT"
  :depends-on (:fiveam :sheeple)
  :serial t
  :components 
  ((:module tests
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "sheeple")))))