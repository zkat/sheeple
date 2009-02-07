(asdf:defsystem persistent-sheeple
  :version "Baahh"
  :description "Clone-crazy hackery tool"
  :maintainer "Kat <kzm@sykosomatic.org>"
  :author "Kat <kzm@sykosomatic.org>"
  :licence "MIT"
  :depends-on (:sheeple :bknr.datastore :bknr.indices)
  :serial t
  :components 
  ((:module psheep
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "psheep")))))

