(asdf:defsystem sheeple
  :version "Baahh"
  :description "Clone-crazy hackery tool"
  :author "Kat <kzm@sykosomatic.org>"
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
	     (:file "sheeple")
	     (:file "clone")
	     (:file "compatibility")
	     (:file "buzzwords")
	     (:file "post-boot")))))


