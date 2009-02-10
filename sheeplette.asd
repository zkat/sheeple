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
	    ((:file "trivial-garbage")
	     (:file "packages")
	     (:file "utils")
	     (:file "sheeplette")))))
