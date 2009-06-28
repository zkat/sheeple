(asdf:defsystem sheeple
  :version "2.0"
  :description "Cheeky prototypes for Common Lisp"
  :author "Josh Marchan <sykopomp at sykosomatic-dot-org>"
  :licence "MIT"
  :depends-on ()
  :serial t
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "trivial-garbage")
	     (:file "packages")
	     (:file "conditions")
	     (:file "utils")
	     (:file "sheeple")
	     (:file "properties")
             (:file "protos")
	     (:file "builtins")
	     (:file "parse-lambda-list")
	     (:file "messages")
	     (:file "reply-definition")
	     (:file "reply-dispatch")
	     (:file "bootstrap")
	     (:file "post-boot")
	     (:file "glue")))))

(asdf:defsystem sheeple-tests
  :version "Baahh"
  :description "Clone-crazy hackery tool, unit tests."
  :author "Josh Marchan <sykopomp at sykosomatic-dot-org>"
  :licence "MIT"
  :depends-on (:sheeple :fiveam)
  :serial t
  :components 
  ((:module tests
	    :serial t
	    :components
	    ((:file "sheeple")
             (:file "properties")
             (:file "protos")
	     (:file "compatibility")
             ;; (:file "messages")
             ))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :sheeple))))
  (format t "~&~%*******************~%~
                 ** Loading tests **~%~
                 *******************~%")
  (asdf:oos 'asdf:load-op 'sheeple-tests)
  (format t "~&~%*******************~%~
                 ** Starting test **~%~
                 *******************~%")
  (asdf:oos 'asdf:test-op 'sheeple-tests)
  (format t "~&*****************************************~%~
               **            Tests finished           **~%~
               *****************************************~%~
               ** If there were any failures on your  **~%~
               ** platform, please report them to me: **~%~
               **  (sykopomp at sykosomatic dot org)  **~%~
               ** or just file a bugreport on github: **~%~
               ** github.com/sykopomp/sheeple/issues  **~%~
               *****************************************~%"))
