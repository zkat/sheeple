(defpackage #:sheeple-tests
  (:use :cl :sheeple :fiveam)
  (:import-from :sheeple . #.(loop for x being the symbols in :sheeple collect x)))
