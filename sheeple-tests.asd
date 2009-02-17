(asdf:defsystem sheeple-tests
  :version "Baahh"
  :description "Clone-crazy hackery tool, unit tests."
  :maintainer "Kat Marchan <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (:sheeple :fiveam)
  :serial t
  :components 
  ((:module tests
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "sheeple")
	     (:file "compatibility")
	     (:file "buzzwords")))))
