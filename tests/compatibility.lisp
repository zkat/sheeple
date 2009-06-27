;; This file is part of Sheeple.

;; tests/compatibility.lisp
;;
;; Unit tests for src/builtins.lisp (and later, for clos-boxing)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sheeple)

(def-suite compat-tests :in sheeple)

(in-suite compat-tests)

(test box-type-of-basic
  "Tests that the box-type-of function returns the right fleeced-wolf for each lisp type"
  (is (equal (find-proto 'null) (box-type-of nil)))
  (is (equal (find-proto 'symbol) (box-type-of 'foo)))
  (is (equal (find-proto 'complex) (box-type-of #C (10 10))))
  (is (equal (find-proto 'integer) (box-type-of 1)))
  (is (equal (find-proto 'float) (box-type-of 1.0)))
  (is (equal (find-proto 'cons) (box-type-of (cons 1 2))))
  (is (equal (find-proto 'character) (box-type-of #\a)))
  (is (equal (find-proto 'hash-table) (box-type-of (make-hash-table))))
  (is (equal (find-proto 'package) (box-type-of (find-package :sheeple-tests))))
  (is (equal (find-proto 'pathname) (box-type-of #P"compatibility.lisp")))
  ;;  (is (equal (find-proto 'readtable) (box-type-of ???))) ; uhhh??
  ;;  (is (equal (find-proto 'stream) (box-type-of ???))) ; dunno
  (is (equal (find-proto 'number) (box-type-of 1/2)))
  (is (equal (find-proto 'string) (box-type-of "foo")))
  (is (equal (find-proto 'bit-vector) (box-type-of #*)))
  (is (equal (find-proto 'vector) (box-type-of (vector 1 2 3))))
  (is (equal (find-proto 'array) (box-type-of (make-array '(1 2)))))
;;  (is (equal =sequence= (box-type-of ???))) ;hm
  (is (equal (find-proto 'function) (box-type-of (lambda () 1)))))

;; (test clos-fleecing
;;   "These will all fail for now."
;;   (defclass foo () ())
;;   (is (not (equal =white-fang= (fleece-of (make-instance 'foo))))))