;; This file is part of Sheeple.

;; tests/compatibility.lisp
;;
;; Unit tests for src/compatibility.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple-tests)

(def-suite compat-tests :in sheeple)

(in-suite compat-tests)

(test fleece-of-basic
  "Tests that the fleece-of function returns the right fleeced-wolf for each lisp type"
  (is (equal =null= (fleece-of nil)))
  (is (equal =symbol= (fleece-of 'foo)))
  (is (equal =complex= (fleece-of #C (10 10))))
  (is (equal =integer= (fleece-of 1)))
  (is (equal =float= (fleece-of 1.0)))
  (is (equal =cons= (fleece-of (cons 1 2))))
  (is (equal =character= (fleece-of #\a)))
  (is (equal =hash-table= (fleece-of (make-hash-table))))
  (is (equal =package= (fleece-of (find-package :sheeple-tests))))
  (is (equal =pathname= (fleece-of #P"compatibility.lisp")))
;;  (is (equal =readtable= (fleece-of ???))) ; uhhh??
;;  (is (equal =stream= (fleece-of ???))) ; dunno
  (is (equal =number= (fleece-of 1/2)))
  (is (equal =string= (fleece-of "foo")))
  (is (equal =bit-vector= (fleece-of #*)))
  (is (equal =vector= (fleece-of (vector 1 2 3))))
  (is (equal =array= (fleece-of (make-array '(1 2)))))
;;  (is (equal =sequence= (fleece-of ???))) ;hm
  (is (equal =function= (fleece-of (lambda () 1)))))

;; (test clos-fleecing
;;   "These will all fail for now."
;;   (defclass foo () ())
;;   (is (not (equal =white-fang= (fleece-of (make-instance 'foo))))))