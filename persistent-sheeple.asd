(asdf:defsystem persistent-sheeple
  :version "Baahh"
  :description "Clone-crazy hackery tool"
  :maintainer "Josh <sykopomp@sykosomatic.org>"
  :author "Josh <sykopomp@sykosomatic.org>"
  :licence "MIT"
  :depends-on (:sheeple :bknr.datastore :bknr.indices)
  :serial t
  :components 
  ((:module psheep
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "psheep")))))

