(defpackage #:sheeplette-tests
  (:use :cl :sheeplette :fiveam)
  (:import-from :sheeplette . #.(loop for x being the symbols in :sheeplette collect x)))
